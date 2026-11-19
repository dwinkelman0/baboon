package io.septimalmind.baboon.typer

import io.septimalmind.baboon.typer.model._
import io.septimalmind.baboon.typer.model.FnTypes._

import scala.collection.mutable

/** P4: sensitivity extraction for fn bodies (standalone; not yet wired into `BaboonTranslator`).
  *
  * Input: a P3 [[TypedBody]] plus the P2 [[TypedCapture]], the seed [[ScopeTable]] and the
  * owning slot (the DTO field the fn is attached to). Output: a [[Sensitivity]] with leaf
  * (field-level) reads, shape (collection/ADT-structure) reads, prev reads (always empty:
  * `!old` has no AST representation and P2 resolves no prev bindings) and field writes.
  *
  * Reads: the body is walked recursively. A [[TypedExpr.Path]] rooted at a capture becomes a
  * [[SchemaPath]] of capture-root owner + ordered capture fields + local segments; a path
  * rooted at the `self` param becomes the owning DTO slot; a path rooted at a `let` propagates
  * that let's reads (extended with any further segments). Index expressions, for-iterables,
  * match scrutinees, assert/if conditions and call arguments are all recursed into.
  * Shape-inspecting calls (receiver with a user/collection-of-user type), `for-in` over a
  * collection, `match` on an ADT/`opt`, and star-chain reads add the owning DTO to `shapeReads`.
  *
  * Writes: the attachment slot is always written. Every `Assign` target resolving to a known
  * slot is a field write (`map[k] = v` additionally records the map shape); a bare-root
  * replacement of the owning record carries struct semantics and expands to all fields of the
  * owner ([[structWrites]]). Locals (`let`s, for binders, match bindings) are never field
  * writes: assigning to one is ignored, never an error.
  *
  * Checks: every produced [[SchemaPath]] is re-validated against the schema, `isPrev` is
  * asserted false (prev reads can never arise), and assigning through an unknown root fails.
  */
object FnSensitivityExtractor {

  /** The field slot a fn is attached to (e.g. `Child.count` for a `default` fn on `count`). */
  final case class Slot(owner: TypeId.User, field: String) {
    def path: SchemaPath = SchemaPath(owner, List(field))
  }

  /** A leaf read/write: the owning DTO plus the field chain selected from it. */
  final case class SchemaPath(owner: TypeId.User, segments: List[String]) {
    def render: String = (owner.name.name :: segments).mkString(".")
  }

  /** Extracted sensitivity for one fn site. */
  final case class Sensitivity(
    leafReads: Set[SchemaPath],
    shapeReads: Set[TypeId.User],
    prevReads: Set[SchemaPath],
    writes: Set[SchemaPath]
  )

  def extract(
    body: TypedBody,
    capture: Option[TypedCapture],
    scope: ScopeTable,
    schema: FnSchema,
    attachment: Slot
  ): Either[List[String], Sensitivity] =
    new Run(capture, scope, schema, attachment).run(body)

  /** Struct-literal write expansion: materialising a whole `owner` record writes every field. */
  def structWrites(owner: TypeId.User, schema: FnSchema): List[SchemaPath] =
    dtoFields(TypeRef.Scalar(owner), schema)
      .getOrElse(Nil)
      .map(f => SchemaPath(owner, List(f.name.name)))

  // -- driver ---------------------------------------------------------------

  private final case class Ctx(scope: ScopeTable, lets: Map[String, TypedExpr]) {
    def withLet(name: String, tpe: TypeRef, value: TypedExpr): Ctx =
      copy(
        scope = scope + (name -> (ScopeEntry.LetBinding(tpe): ScopeEntry)),
        lets = lets + (name -> value)
      )
    def withBinder(name: String, tpe: TypeRef): Ctx =
      copy(scope = scope + (name -> (ScopeEntry.LetBinding(tpe): ScopeEntry)))
  }

  private final class Run(
    capture: Option[TypedCapture],
    baseScope: ScopeTable,
    schema: FnSchema,
    attachment: Slot
  ) {
    private val writes = mutable.LinkedHashSet.empty[SchemaPath]
    private val errors = mutable.ListBuffer.empty[String]

    private type LeafB  = mutable.Set[SchemaPath]
    private type ShapeB = mutable.Set[TypeId.User]

    def run(body: TypedBody): Either[List[String], Sensitivity] = {
      val leafB: LeafB   = mutable.LinkedHashSet.empty
      val shapeB: ShapeB = mutable.LinkedHashSet.empty
      var ctx            = Ctx(baseScope, Map.empty)
      body.stmts.foreach(s => ctx = stmt(s, ctx, Set.empty, leafB, shapeB))
      expr(body.result, ctx, Set.empty, leafB, shapeB)
      // The attachment slot is always written, then every produced path is re-validated.
      writes += attachment.path
      (leafB.toSet ++ writes.toSet).foreach(validate)
      if (errors.nonEmpty) Left(errors.toList.distinct)
      else {
        val out = Sensitivity(leafB.toSet, shapeB.toSet, Set.empty, writes.toSet)
        // `isPrev` has no AST representation and P2 resolves no prev bindings,
        // so prev reads can never arise; assert the invariant explicitly.
        if (out.prevReads.nonEmpty) Left(List("prev reads are not supported (isPrev must be false)"))
        else Right(out)
      }
    }

    // -- statements ---------------------------------------------------------

    private def stmt(s: TypedStmt, ctx: Ctx, visiting: Set[String], leafB: LeafB, shapeB: ShapeB): Ctx =
      s match {
        case TypedStmt.Let(name, tpe, value) =>
          expr(value, ctx, visiting, leafB, shapeB)
          ctx.withLet(name, tpe, value)
        case TypedStmt.Effect(e) =>
          expr(e, ctx, visiting, leafB, shapeB)
          ctx
        case TypedStmt.Assert(a) =>
          expr(a.cond, ctx, visiting, leafB, shapeB)
          a.msg.foreach(expr(_, ctx, visiting, leafB, shapeB))
          ctx
      }

    private def block(
      b: TypedExpr.Block,
      ctx: Ctx,
      visiting: Set[String],
      leafB: LeafB,
      shapeB: ShapeB
    ): Unit = {
      var c = ctx
      b.stmts.foreach(s => c = stmt(s, c, visiting, leafB, shapeB))
      b.value.foreach(expr(_, c, visiting, leafB, shapeB))
    }

    // -- expressions --------------------------------------------------------

    private def expr(e: TypedExpr, ctx: Ctx, visiting: Set[String], leafB: LeafB, shapeB: ShapeB): Unit =
      e match {
        case TypedExpr.Lit(_, _) => ()
        case p: TypedExpr.Path =>
          pathRead(p, ctx, visiting, leafB, shapeB, leafB += _)
        case TypedExpr.Call(target, _, args, _) =>
          expr(target, ctx, visiting, leafB, shapeB)
          args.foreach(expr(_, ctx, visiting, leafB, shapeB))
          shapeB ++= shapeOf(target.tpe)
        case TypedExpr.BinOp(_, l, r, _) =>
          expr(l, ctx, visiting, leafB, shapeB)
          expr(r, ctx, visiting, leafB, shapeB)
        case TypedExpr.UnaryOp(_, x, _) =>
          expr(x, ctx, visiting, leafB, shapeB)
        case TypedExpr.If(cond, t, el, _) =>
          expr(cond, ctx, visiting, leafB, shapeB)
          expr(t, ctx, visiting, leafB, shapeB)
          expr(el, ctx, visiting, leafB, shapeB)
        case TypedExpr.Assign(target, value, _) =>
          expr(value, ctx, visiting, leafB, shapeB)
          assignWrite(target, ctx, visiting, leafB, shapeB)
        case TypedExpr.ForIn(varName, iterable, b, _) =>
          val tmpL: LeafB   = mutable.LinkedHashSet.empty
          val tmpS: ShapeB  = mutable.LinkedHashSet.empty
          expr(iterable, ctx, visiting, tmpL, tmpS)
          leafB ++= tmpL
          shapeB ++= tmpS
          // Iterating a collection inspects its shape: the owning DTOs plus the element owner.
          tmpL.foreach(p => shapeB += p.owner)
          shapeB ++= shapeOf(iterable.tpe)
          block(b, ctx.withBinder(varName, forBinderT(iterable.tpe).getOrElse(iterable.tpe)), visiting, leafB, shapeB)
        case TypedExpr.ForC(init, cond, step, b, _) =>
          expr(init.value, ctx, visiting, leafB, shapeB)
          val ctx1 = ctx.withBinder(init.name, init.tpe)
          expr(cond, ctx1, visiting, leafB, shapeB)
          expr(step, ctx1, visiting, leafB, shapeB)
          block(b, ctx1, visiting, leafB, shapeB)
        case TypedExpr.Match(scr, arms, _) =>
          val tmpL: LeafB  = mutable.LinkedHashSet.empty
          val tmpS: ShapeB = mutable.LinkedHashSet.empty
          expr(scr, ctx, visiting, tmpL, tmpS)
          leafB ++= tmpL
          shapeB ++= tmpS
          // Matching on an ADT/`opt` scrutinee inspects its shape.
          tmpL.foreach(p => shapeB += p.owner)
          shapeB ++= shapeOf(scr.tpe)
          arms.foreach { arm =>
            val actx = arm.pattern match {
              case TypedPattern.Variant(_, Some(bd), Some(p)) => ctx.withBinder(bd, p)
              case _                                          => ctx
            }
            block(arm.body, actx, visiting, leafB, shapeB)
          }
        case TypedExpr.Block(stmts, value, _) =>
          var c = ctx
          stmts.foreach(s => c = stmt(s, c, visiting, leafB, shapeB))
          value.foreach(expr(_, c, visiting, leafB, shapeB))
      }

    // -- reads --------------------------------------------------------------

    private def pathRead(
      p: TypedExpr.Path,
      ctx: Ctx,
      visiting: Set[String],
      leafB: LeafB,
      shapeB: ShapeB,
      add: SchemaPath => Unit
    ): Unit = {
      p.segments.foreach {
        case TypedPathSeg.Index(idx, _) => expr(idx, ctx, visiting, leafB, shapeB)
        case TypedPathSeg.Field(_, _)   => ()
      }
      val segFields = p.segments.collect { case TypedPathSeg.Field(n, _) => n }
      ctx.scope.get(p.root) match {
        case None =>
          errors += s"unknown slot: ${p.root}"
        case Some(ScopeEntry.CaptureRoot(_)) =>
          capture match {
            case None =>
              errors += s"capture root `${p.root}` with no capture"
            case Some(c) =>
              userOwner(c.rootType) match {
                case None => ()
                case Some(owner) =>
                  if (isStarCapture(c)) shapeB += owner
                  add(SchemaPath(owner, captureFields(c) ++ segFields))
              }
          }
        case Some(ScopeEntry.Param(t)) =>
          val owner = if (p.root == "self") Some(attachment.owner) else userOwner(t)
          owner.foreach(o => if (segFields.nonEmpty) add(SchemaPath(o, segFields)))
        case Some(ScopeEntry.LetBinding(_)) if ctx.lets.contains(p.root) && !visiting.contains(p.root) =>
          // A `let` use propagates the let's reads, extended with any further segments.
          val tmpL: LeafB  = mutable.LinkedHashSet.empty
          val tmpS: ShapeB = mutable.LinkedHashSet.empty
          expr(ctx.lets(p.root), ctx, visiting + p.root, tmpL, tmpS)
          tmpL.foreach(l => add(l.copy(segments = l.segments ++ segFields)))
          shapeB ++= tmpS
        case Some(ScopeEntry.LetBinding(t)) =>
          userOwner(t).foreach(o => if (segFields.nonEmpty) add(SchemaPath(o, segFields)))
        case Some(ScopeEntry.StarChain(_)) =>
          capture match {
            case None =>
              errors += s"star chain `${p.root}` with no capture"
            case Some(c) =>
              starPrefix(c, p.root) match {
                case None =>
                  errors += s"unknown star chain: ${p.root}"
                case Some((owner, prefix, sub)) =>
                  // A star read yields the element leafs plus the collection shape.
                  shapeB += owner
                  add(SchemaPath(owner, prefix ++ sub ++ segFields))
              }
          }
      }
    }

    // -- writes -------------------------------------------------------------

    private def assignWrite(
      target: TypedExpr,
      ctx: Ctx,
      visiting: Set[String],
      leafB: LeafB,
      shapeB: ShapeB
    ): Unit =
      target match {
        case p: TypedExpr.Path =>
          ctx.scope.get(p.root) match {
            case None =>
              errors += s"unknown assign target: ${p.root}"
            case Some(_: ScopeEntry.LetBinding) =>
              () // locals (lets, for binders, match bindings) are never field writes
            case Some(_: ScopeEntry.StarChain) =>
              errors += s"cannot assign to star chain: ${p.root}"
            case Some(_: ScopeEntry.CaptureRoot) | Some(_: ScopeEntry.Param) =>
              if (p.segments.isEmpty) {
                // Whole-record replacement carries struct semantics: every field is written.
                ownerOfRoot(p.root, ctx) match {
                  case Some(o) => writes ++= structWrites(o, schema)
                  case None    => errors += s"unknown assign target: ${p.root}"
                }
              } else {
                val tmpL: LeafB  = mutable.LinkedHashSet.empty
                val tmpS: ShapeB = mutable.LinkedHashSet.empty
                pathRead(p, ctx, visiting, leafB, tmpS, tmpL += _)
                writes ++= tmpL
                shapeB ++= tmpS
                // `map[k] = v` writes inspect the map shape as well as the slot.
                tmpL.foreach(sp => if (hasMapField(sp)) shapeB += sp.owner)
              }
          }
        case other =>
          errors += s"assign target must be a path, got ${other.getClass.getSimpleName}"
      }

    // -- checks -------------------------------------------------------------

    private def validate(sp: SchemaPath): Unit = {
      if (schema.lookup(sp.owner.name.name).isEmpty)
        errors += s"unknown type: ${sp.owner.name.name}"
      else {
        val init: Either[String, TypeRef] = Right(TypeRef.Scalar(sp.owner))
        sp.segments.foldLeft(init) { (acc, seg) => acc.flatMap(t => stepField(t, seg)) } match {
          case Left(m)  => errors += m
          case Right(_) => ()
        }
      }
    }

    private def stepField(cur: TypeRef, seg: String): Either[String, TypeRef] = {
      val t = peelOpt(cur)
      if (isLst(t) || isSet(t) || isMap(t)) {
        elemType(t).toRight(s"cannot situate into $t").flatMap { et =>
          dtoFields(et, schema).toRight(s"no fields on $et (field `$seg`)").flatMap { fs =>
            fs.find(_.name.name == seg).toRight(s"unknown field `$seg` on $et").map(_.tpe)
          }
        }
      } else {
        dtoFields(t, schema).toRight(s"no fields on $t (field `$seg`)").flatMap { fs =>
          fs.find(_.name.name == seg).toRight(s"unknown field `$seg` on $t").map(_.tpe)
        }
      }
    }

    private def hasMapField(sp: SchemaPath): Boolean = {
      if (schema.lookup(sp.owner.name.name).isEmpty) return false
      var cur: TypeRef = TypeRef.Scalar(sp.owner)
      var found        = false
      val it           = sp.segments.iterator
      var ok           = true
      while (ok && !found && it.hasNext) {
        stepField(cur, it.next()) match {
          case Left(_)  => ok = false
          case Right(n) => if (isMap(n)) found = true; cur = n
        }
      }
      found
    }

    // -- small helpers ------------------------------------------------------

    private def userOwner(t: TypeRef): Option[TypeId.User] = t match {
      case TypeRef.Scalar(u: TypeId.User) => Some(u)
      case _                              => None
    }

    private def shapeOf(t: TypeRef): Set[TypeId.User] = t match {
      case TypeRef.Scalar(u: TypeId.User) => Set(u)
      case TypeRef.Constructor(_, args)   => args.toList.flatMap(shapeOf).toSet
      case _                              => Set.empty
    }

    private def forBinderT(iterable: TypeRef): Option[TypeRef] =
      if (isLst(iterable) || isSet(iterable)) elemType(iterable)
      else if (isMap(iterable)) keyType(iterable)
      else None

    private def ownerOfRoot(root: String, ctx: Ctx): Option[TypeId.User] =
      ctx.scope.get(root) match {
        case Some(ScopeEntry.Param(_)) if root == "self" => Some(attachment.owner)
        case Some(e)                                    => userOwner(e.tpe)
        case None                                       => None
      }

    private def isStarCapture(c: TypedCapture): Boolean =
      c.path.exists {
        case _: TypedCaptureStep.StarStep => true
        case _                            => false
      }

    /** Ordered field names of a capture path: plain fields plus star-substep fields. */
    private def captureFields(c: TypedCapture): List[String] =
      c.path.flatMap {
        case TypedCaptureStep.FieldStep(f, _, _)   => List(f)
        case TypedCaptureStep.KeyStep(_, _)        => Nil
        case TypedCaptureStep.StarStep(_, _, sub)  => sub.collect { case TypedCaptureStep.FieldStep(f, _, _) => f }
      }

    /** Split a capture at a star chain: root owner, fields before the star, star-substep fields. */
    private def starPrefix(c: TypedCapture, chain: String): Option[(TypeId.User, List[String], List[String])] =
      userOwner(c.rootType).flatMap { owner =>
        val idx = c.path.indexWhere {
          case TypedCaptureStep.StarStep(ch, _, _) if ch == chain => true
          case _                                                  => false
        }
        if (idx < 0) None
        else {
          val before = c.path.take(idx).flatMap {
            case TypedCaptureStep.FieldStep(f, _, _) => List(f)
            case _                                   => Nil
          }
          val sub = c.path(idx) match {
            case TypedCaptureStep.StarStep(_, _, s) => s.collect { case TypedCaptureStep.FieldStep(f, _, _) => f }
            case _                                 => Nil
          }
          Some((owner, before, sub))
        }
      }

    private def peelOpt(t: TypeRef): TypeRef = t match {
      case x if isOpt(x) => elemType(x).map(peelOpt).getOrElse(x)
      case x             => x
    }
  }
}
