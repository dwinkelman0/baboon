package io.septimalmind.baboon.typer

import io.septimalmind.baboon.parser.model.{RawFn, RawTypeName, RawTypeRef}
import io.septimalmind.baboon.typer.FnSensitivityExtractor.Slot
import io.septimalmind.baboon.typer.model._

/** P7: IR assembly & final invariants (standalone; not yet wired into `BaboonTranslator`).
  *
  * `assemble` runs P1→P2→P3→P4→P5→P6 in order over one batch of fn sites,
  * short-circuiting per-pass batch errors (resolution before gates before
  * cycles), then enforces the final backend-ready invariants (R3/R12).
  *
  * Translator gates stay untouched: everything here exercises the standalone
  * passes only. Errors are plain strings, consistent with P5/P6.
  */
object FnPipeline {

  /** One fn awaiting assembly. `expected` is the owning field's (raw) type;
    * `declaredSources` is the migrate source set (empty for other roles);
    * `schema` is the minimal P2/P3 view — all inputs in one batch are
    * expected to share it (P5/P6 run against the first input's view).
    */
  final case class FnSiteInputs(
    rawFn: RawFn,
    owner: String,
    field: String,
    role: FnRole,
    expected: RawTypeRef,
    declaredSources: Set[SchemaPath],
    schema: FnSchema,
  )

  /** P7 IR node: one site lowered through P1→P3. Single-capture is structural:
    * zero or one capture (`Option`), never a list.
    */
  final case class TypedFn(
    site: FnSite,
    capture: Option[TypedCapture],
    body: TypedBody,
    expected: TypeRef,
    params: List[(String, TypeRef)] = Nil
  )

  /** Backend-ready program: one IR node + sensitivity per site, the P5 gated
    * fns, the full P6 edge set and the deterministic emission order.
    */
  final case class ValidatedFnProgram(
    fns: List[(FnSite, TypedFn, Sensitivity)],
    gated: List[FnReadGate.Gated],
    graph: List[FnDepGraphBuilder.DepEdge],
    emissionOrder: FnDepGraphBuilder.EmissionOrder,
  )

  def assemble(sites: List[FnSiteInputs]): Either[List[String], ValidatedFnProgram] =
    if (sites.isEmpty) Right(ValidatedFnProgram(Nil, Nil, Nil, FnDepGraphBuilder.EmissionOrder(Nil, Nil)))
    else
      for {
        p1 <- pass1(sites)
        p2 <- pass2(p1)
        p3 <- pass3(p2)
        p4 <- pass4(p3)
        p5 <- pass5(p4)
        p6 <- pass6(p4, p5)
        out <- finalizeProgram(sites, p4, p5, p6)
      } yield out

  // -- P1: extract sites + head checks -----------------------------------------

  private final case class P1(site: FnSite, in: FnSiteInputs)

  private def tag(site: FnSite): String =
    s"fn ${FnReadGate.renderSite(site)}"

  private def pass1(sites: List[FnSiteInputs]): Either[List[String], List[P1]] = {
    val errs = List.newBuilder[String]
    val out = sites.map { in =>
      val site = FnSite(FnSiteId(in.owner, in.field, in.role), in.rawFn)
      FnSiteExtractor.check(in.rawFn) match {
        case Left(es) => es.foreach(e => errs += s"${tag(site)}: $e")
        case Right(_) => ()
      }
      P1(site, in)
    }
    val e = errs.result().distinct
    if (e.nonEmpty) Left(e) else Right(out)
  }

  // -- P2: resolve expected/param/capture types ---------------------------------

  private final case class P2(
    site: FnSite,
    in: FnSiteInputs,
    expected: TypeRef,
    params: List[(String, TypeRef)],
    capture: Option[TypedCapture],
    bindings: List[(String, ScopeEntry)],
  )

  /** Role-dependent `Self` (container-self clarification).
    *
    * `Self` in a capture root or param type means:
    *   - `Computed` / `Migrate`: the enclosing container instance (owner DTO);
    *   - `Default` / `Normalize`: the field's own value (the expected type).
    * Rewritten here (P2) so the standalone pipeline and the translator share
    * one home for `Self`; the translator must not pre-rewrite. A capture path
    * on `= fn` (computed) therefore situates the container subtree, while a
    * normalize/default `self: Self` types as the field value.
    */
  private def selfFor(role: FnRole, owner: String, expected: RawTypeRef, raw: RawTypeRef): RawTypeRef = raw match {
    case RawTypeRef.Simple(n, pre) if n.name == "Self" =>
      role match {
        case FnRole.Computed | FnRole.Migrate   => RawTypeRef.Simple(RawTypeName(owner), pre)
        case FnRole.Default | FnRole.Normalize => expected
      }
    case other => other
  }

  /** A `Computed` (`= fn`) `self` must be the enclosing container: an explicit
    * annotation naming anything else (e.g. the field scalar when the owner is
    * a DTO) is a resolution error. Other roles leave explicit annotations
    * unchecked. An unknown owner is skipped here (P4 reports it).
    */
  private def checkComputedSelf(
    site: FnSite,
    in: FnSiteInputs,
    schema: FnSchema,
    params: List[(String, TypeRef)],
  ): FnTypes.Res[Unit] =
    in.role match {
      case FnRole.Computed =>
        params.find(_._1 == "self") match {
          case None => FnTypes.ok(())
          case Some((_, t)) =>
            schema.lookup(in.owner) match {
              case Some(u: Typedef.User) =>
                val ownerT = TypeRef.Scalar(u.id)
                if (t == ownerT) FnTypes.ok(())
                else FnTypes.fail(s"${tag(site)}: computed `self` must be the enclosing container `${u.id.name.name}`, got $t")
              case _ => FnTypes.ok(())
            }
        }
      case _ => FnTypes.ok(())
    }

  private def pass2(p1: List[P1]): Either[List[String], List[P2]] = {
    val errs = List.newBuilder[String]
    val out = p1.flatMap { case P1(site, in) =>
      val schema = in.schema
      val expR = FnTypes
        .resolveRaw(in.expected, schema)
        .left
        .map(_.map(m => s"${tag(site)}: expected type: $m"))
      val paramsR = FnTypes
        .traverse(in.rawFn.params.map { p =>
          FnTypes.resolveRaw(selfFor(in.role, in.owner, in.expected, p.tpe), schema).left.map(_.map(m => s"${tag(site)}: param `${p.name}`: $m")).map(t => p.name -> t)
        })
      val capR: FnTypes.Res[(Option[TypedCapture], List[(String, ScopeEntry)])] =
        in.rawFn.captures match {
          case Nil => FnTypes.ok((None, Nil))
          case cap :: Nil =>
            FnCaptureResolver
              .resolveCapture(cap.copy(rootType = selfFor(in.role, in.owner, in.expected, cap.rootType)), schema)
              .left
              .map(_.map(m => s"${tag(site)}: $m"))
              .map { case (c, b) => (Some(c), b) }
          case many =>
            FnTypes.fail(s"${tag(site)}: fn has ${many.size} captures, at most one is allowed")
        }
      (expR, paramsR, capR) match {
        case (Right(exp), Right(params), Right((cap, bindings))) =>
          checkComputedSelf(site, in, schema, params) match {
            case Right(_) => Some(P2(site, in, exp, params, cap, bindings))
            case Left(es) =>
              es.foreach(errs += _)
              None
          }
        case _ =>
          expR.left.toOption.foreach(es => es.foreach(errs += _))
          paramsR.left.toOption.foreach(es => es.foreach(errs += _))
          capR.left.toOption.foreach(es => es.foreach(errs += _))
          None
      }
    }
    val e = errs.result().distinct
    if (e.nonEmpty) Left(e) else Right(out)
  }

  // -- P3: type bodies ------------------------------------------------------------

  private final case class P3(p2: P2, typed: TypedFn, scope: ScopeTable)

  private def pass3(p2s: List[P2]): Either[List[String], List[P3]] = {
    val errs = List.newBuilder[String]
    val out = p2s.flatMap { p2 =>
      val scope = FnBodyTyper.seedScope(p2.params, p2.bindings)
      FnBodyTyper.typeBody(p2.in.rawFn.body, scope, p2.in.schema, p2.expected) match {
        case Right(body) => Some(P3(p2, TypedFn(p2.site, p2.capture, body, p2.expected, p2.params), scope))
        case Left(es) =>
          es.foreach(e => errs += s"${tag(p2.site)}: $e")
          None
      }
    }
    val e = errs.result().distinct
    if (e.nonEmpty) Left(e) else Right(out)
  }

  // -- P4: sensitivity ---------------------------------------------------------------

  private final case class P4(p3: P3, slot: Slot, read: Sensitivity, write: Sensitivity)

  private def pass4(p3s: List[P3]): Either[List[String], List[P4]] = {
    val errs = List.newBuilder[String]
    val out = p3s.flatMap { p3 =>
      val schema = p3.p2.in.schema
      schema.lookup(p3.p2.in.owner) match {
        case Some(t: Typedef.User) =>
          val slot = Slot(t.id, p3.p2.in.field)
          FnSensitivityExtractor.extract(p3.typed.body, p3.typed.capture, p3.scope, schema, slot) match {
            case Right((read, write)) => Some(P4(p3, slot, read, write))
            case Left(es) =>
              es.foreach(e => errs += s"${tag(p3.p2.site)}: $e")
              None
          }
        case _ =>
          errs += s"${tag(p3.p2.site)}: unknown owner type `${p3.p2.in.owner}`"
          None
      }
    }
    val e = errs.result().distinct
    if (e.nonEmpty) Left(e) else Right(unionCalleeReads(out))
  }

  /** P4 callee union: a caller that issues `recv.method(args)` also reads
    * whatever the callee method body reads. Without this the caller's
    * sensitivity would cover only the receiver shape, and the read gate /
    * dep graph would miss table-field ordering edges.
    */
  private def unionCalleeReads(p4s: List[P4]): List[P4] = {
    if (p4s.isEmpty) return p4s
    val bySite: Map[(String, String), P4] =
      p4s.map(p => (p.p3.p2.in.owner, p.p3.p2.in.field) -> p).toMap
    def calleeSites(body: TypedBody): List[(String, String)] = {
      val out = List.newBuilder[(String, String)]
      def expr(e: TypedExpr): Unit = e match {
        case TypedExpr.UserCall(t, m, as, _) =>
          t.tpe match {
            case TypeRef.Scalar(id: TypeId.User) => out += ((id.name.name, m))
            case _                               => ()
          }
          expr(t); as.foreach(expr)
        case TypedExpr.Call(t, _, as, _)  => expr(t); as.foreach(expr)
        case TypedExpr.BinOp(_, l, r, _)  => expr(l); expr(r)
        case TypedExpr.UnaryOp(_, x, _)   => expr(x)
        case TypedExpr.If(c, t, el, _)    => expr(c); expr(t); expr(el)
        case TypedExpr.Assign(t, v, _)    => expr(t); expr(v)
        case TypedExpr.ForIn(_, it, b, _) => expr(it); block(b)
        case TypedExpr.ForC(i, c, s, b, _) => expr(i.value); expr(c); expr(s); block(b)
        case TypedExpr.Match(s, arms, _)  => expr(s); arms.foreach(a => block(a.body))
        case TypedExpr.Block(sts, v, _)   => sts.foreach(stmt); v.foreach(expr)
        case TypedExpr.Lit(_, _)          => ()
        case TypedExpr.Path(_, segs, _)   => segs.foreach {
          case TypedPathSeg.Index(i, _) => expr(i)
          case _                        => ()
        }
      }
      def block(b: TypedExpr.Block): Unit = { b.stmts.foreach(stmt); b.value.foreach(expr) }
      def stmt(s: TypedStmt): Unit = s match {
        case TypedStmt.Let(_, _, v) => expr(v)
        case TypedStmt.Effect(e)    => expr(e)
        case TypedStmt.Assert(a)    => expr(a.cond); a.msg.foreach(expr)
      }
      body.stmts.foreach(stmt); expr(body.result)
      out.result()
    }
    // Fixpoint: callees may themselves call further methods.
    var reads: Map[(String, String), Sensitivity] =
      p4s.map(p => (p.p3.p2.in.owner, p.p3.p2.in.field) -> p.read).toMap
    var changed = true
    while (changed) {
      changed = false
      p4s.foreach { p =>
        val key = (p.p3.p2.in.owner, p.p3.p2.in.field)
        calleeSites(p.p3.typed.body).foreach { ck =>
          bySite.get(ck).foreach { _ =>
            val add = reads.getOrElse(ck, Sensitivity(Set.empty, Set.empty, Set.empty))
            val cur = reads.getOrElse(key, Sensitivity(Set.empty, Set.empty, Set.empty))
            val merged = Sensitivity(cur.leafReads ++ add.leafReads, cur.shapeReads ++ add.shapeReads, cur.prevReads ++ add.prevReads)
            if (merged != cur) { reads = reads.updated(key, merged); changed = true }
          }
        }
      }
    }
    p4s.map(p => p.copy(read = reads.getOrElse((p.p3.p2.in.owner, p.p3.p2.in.field), p.read)))
  }

  // -- P5: read gate -------------------------------------------------------------------

  private def pass5(p4s: List[P4]): Either[List[String], List[FnReadGate.Gated]] = {
    val schema = p4s.headOption.map(_.p3.p2.in.schema).getOrElse(FnSchema.empty)
    val ins = p4s.map(p => FnReadGate.Input(p.p3.p2.site, p.p3.typed.body, p.read, p.write, p.p3.p2.in.declaredSources))
    FnReadGate.gate(ins, schema)
  }

  // -- P6: dep graph + emission order -------------------------------------------------------

  private def pass6(p4s: List[P4], gated: List[FnReadGate.Gated]): Either[List[String], FnDepGraphBuilder.EmissionOrder] = {
    val schema = p4s.headOption.map(_.p3.p2.in.schema).getOrElse(FnSchema.empty)
    val bySite = p4s.map(p => p.p3.p2.site -> p.p3.typed.capture).toMap
    val withCaps: Map[FnDepGraphBuilder.FnNodeId, Option[TypedCapture]] =
      gated.map(g => FnDepGraphBuilder.nodeIdOf(g) -> bySite.getOrElse(g.site, None)).toMap
    FnDepGraphBuilder.build(FnDepGraphBuilder.fromGated(gated, withCaps), schema)
  }

  // -- final invariants (R3/R12) ------------------------------------------------------------------

  private def finalizeProgram(
    sites: List[FnSiteInputs],
    p4s: List[P4],
    gated: List[FnReadGate.Gated],
    order: FnDepGraphBuilder.EmissionOrder,
  ): Either[List[String], ValidatedFnProgram] = {
    val errs = List.newBuilder[String]
    val schema = p4s.headOption.map(_.p3.p2.in.schema).getOrElse(FnSchema.empty)

    // Every FnSite yielded exactly one TypedFn (no orphans/dupes).
    val ids = p4s.map(_.p3.p2.site.id)
    if (p4s.size != sites.size)
      errs += s"orphan check: ${sites.size} sites yielded ${p4s.size} typed fns"
    val dupes = ids.groupBy(identity).collect { case (id, l) if l.size > 1 => id }
    dupes.foreach(id => errs += s"duplicate fn site: ${id.owner}.${id.field}(${FnReadGate.roleName(id.role)})")
    if (p4s.size != gated.size)
      errs += s"orphan check: ${p4s.size} typed fns yielded ${gated.size} gated fns"

    // No RawTypeRef remains: every TypedCapture root/endpoint/step type and
    // every body type is a resolved TypeRef (an `any` would prove a raw leak,
    // since P2 rejects `any` in signatures).
    p4s.foreach { p =>
      p.p3.typed.capture.foreach { c =>
        if (hasAny(c.rootType)) errs += s"${tag(p.p3.p2.site)}: capture root type is not fully resolved: ${c.rootType}"
        if (hasAny(c.endpoint)) errs += s"${tag(p.p3.p2.site)}: capture endpoint is not fully resolved: ${c.endpoint}"
        c.path.foreach {
          case TypedCaptureStep.FieldStep(_, _, t)  => if (hasAny(t)) errs += s"${tag(p.p3.p2.site)}: capture step type is not fully resolved: $t"
          case TypedCaptureStep.KeyStep(_, elem)    => if (hasAny(elem)) errs += s"${tag(p.p3.p2.site)}: capture key type is not fully resolved: $elem"
          case TypedCaptureStep.StarStep(_, elem, sub) =>
            if (hasAny(elem)) errs += s"${tag(p.p3.p2.site)}: capture star type is not fully resolved: $elem"
            sub.foreach {
              case TypedCaptureStep.FieldStep(_, _, t) => if (hasAny(t)) errs += s"${tag(p.p3.p2.site)}: capture star-substep type is not fully resolved: $t"
              case _                                   => ()
            }
        }
      }
      collectBodyTypes(p.p3.typed.body).foreach { t =>
        if (hasAny(t)) errs += s"${tag(p.p3.p2.site)}: body type is not fully resolved: $t"
      }
    }

    // Single-capture asserted on IR nodes.
    p4s.foreach { p =>
      if (p.p3.p2.in.rawFn.captures.size > 1)
        errs += s"${tag(p.p3.p2.site)}: fn has ${p.p3.p2.in.rawFn.captures.size} captures, at most one is allowed"
    }

    // Deferred-prev: prevReads == ∅. SchemaPath carries no isPrev flag (there is
    // no `!old` AST representation and P2 resolves no prev bindings), so empty
    // prevReads is the whole invariant.
    p4s.foreach { p =>
      if (p.read.prevReads.nonEmpty)
        errs += s"${tag(p.p3.p2.site)}: prev reads are not supported (isPrev must be false)"
    }

    // Sealing: every read target resolves in the schema.
    p4s.foreach { p =>
      (p.read.leafReads ++ p.p3.p2.in.declaredSources).foreach { r =>
        resolvePath(schema, r) match {
          case Some(m) => errs += s"${tag(p.p3.p2.site)}: $m"
          case None    => ()
        }
      }
    }

    // Emission order covers all graph nodes (every fn node runs somewhere).
    val fnNodes: Set[FnDepGraphBuilder.DepNode] =
      gated.map(FnDepGraphBuilder.nodeIdOf).map(FnDepGraphBuilder.DepNode.Fn(_)).toSet
    val ordered = order.nodes.toSet
    (fnNodes -- ordered).foreach(n => errs += s"emission order misses fn node: ${n.render}")

    val e = errs.result().distinct
    if (e.nonEmpty) Left(e)
    else {
      // Deterministic output under shuffled inputs: sort entries and gated
      // fns by (owner, field, role rank); the emission order already is.
      val byKey = (p: P4) => (p.p3.p2.in.owner, p.p3.p2.in.field, FnReadGate.roleRank(p.p3.p2.in.role))
      val sorted = p4s.sortBy(byKey)
      val entries = sorted.map(p => (p.p3.p2.site, p.p3.typed, p.read))
      val sortedGated = gated.sortBy(g => (g.site.id.owner, g.site.id.field, FnReadGate.roleRank(g.site.id.role)))
      Right(ValidatedFnProgram(entries, sortedGated, order.edges, order))
    }
  }

  private def hasAny(t: TypeRef): Boolean = t match {
    case TypeRef.Any(_, _)            => true
    case TypeRef.Constructor(_, args) => args.toList.exists(hasAny)
    case _                            => false
  }

  private def collectBodyTypes(body: TypedBody): List[TypeRef] = {
    val out = List.newBuilder[TypeRef]
    def block(b: TypedExpr.Block): Unit = {
      b.stmts.foreach(stmt)
      b.value.foreach(expr)
    }
    def expr(e: TypedExpr): Unit = {
      out += e.tpe
      e match {
        case TypedExpr.Lit(_, _)          => ()
        case TypedExpr.Path(_, segs, _)   => segs.foreach {
          case TypedPathSeg.Field(_, t) => out += t
          case TypedPathSeg.Index(i, t) => out += t; expr(i)
        }
        case TypedExpr.Call(t, _, as, _)  => expr(t); as.foreach(expr)
        case TypedExpr.UserCall(t, _, as, _) => expr(t); as.foreach(expr)
        case TypedExpr.BinOp(_, l, r, _)  => expr(l); expr(r)
        case TypedExpr.UnaryOp(_, x, _)   => expr(x)
        case TypedExpr.If(c, t, el, _)    => expr(c); expr(t); expr(el)
        case TypedExpr.Assign(t, v, _)    => expr(t); expr(v)
        case TypedExpr.ForIn(_, it, b, _) => expr(it); block(b)
        case TypedExpr.ForC(i, c, s, b, _) => expr(i.value); expr(c); expr(s); block(b)
        case TypedExpr.Match(s, arms, _)  => expr(s); arms.foreach(a => block(a.body))
        case TypedExpr.Block(sts, v, _)   => sts.foreach(stmt); v.foreach(expr)
      }
    }
    def stmt(s: TypedStmt): Unit = s match {
      case TypedStmt.Let(_, t, v) => out += t; expr(v)
      case TypedStmt.Effect(e)    => expr(e)
      case TypedStmt.Assert(a)    => expr(a.cond); a.msg.foreach(expr)
    }
    body.stmts.foreach(stmt)
    expr(body.result)
    out.result()
  }

  private def resolvePath(schema: FnSchema, path: SchemaPath): Option[String] = {
    if (schema.lookup(path.root.name.name).isEmpty)
      return Some(s"unknown type: ${path.root.name.name}")
    var cur: TypeRef = TypeRef.Scalar(path.root)
    val it = path.fieldNames.iterator
    while (it.hasNext) {
      val seg = it.next()
      // Peel transparent opt layers, then situate collections into elements.
      var t = cur
      while (FnTypes.isOpt(t)) t = FnTypes.elemType(t).getOrElse(t)
      if (FnTypes.isLst(t) || FnTypes.isSet(t) || FnTypes.isMap(t))
        t = FnTypes.elemType(t).getOrElse(t)
      FnTypes.dtoFields(t, schema) match {
        case None => return Some(s"no fields on $t (field `$seg`)")
        case Some(fs) =>
          fs.find(_.name.name == seg) match {
            case None    => return Some(s"unknown field `$seg` on $t")
            case Some(f) => cur = f.tpe
          }
      }
    }
    None
  }
}
