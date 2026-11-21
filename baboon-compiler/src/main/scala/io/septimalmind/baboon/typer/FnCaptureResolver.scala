package io.septimalmind.baboon.typer

import io.septimalmind.baboon.parser.model._
import io.septimalmind.baboon.typer.model._
import FnTypes._

/** P2: resolve a capture head + ordered path against a minimal schema view.
  *
  *   - root type name must resolve (builtins + `schema.types`);
  *   - every `Field` step must exist on the (collection-transparent) owner;
  *   - collections (`lst`/`map`/`set`/`opt`) are transparent for `.field`
  *     traversal; an explicit `.Some` on `opt[T]` is canonicalized away;
  *   - `Key` (`[$k]`) binds its var (`map[K,V] -> K`, `lst/set -> i32`)
  *     and situates into the element type;
  *   - `Star` roots must sit on `lst`/`map`/`set` (`opt` payload transparent);
  *     the chain var binds the sub-path endpoint; nested stars are rejected;
  *   - `Variant` segments are rejected (variants only match in `match` arms);
  *   - `!old` (`isPrev`) prefixes do not exist at AST level; nothing to assert.
  *
  * The capture name itself binds the path **endpoint** (the situated value).
  */
object FnCaptureResolver {

  /** Resolve one capture. Returns the typed capture plus scope bindings to
    * seed the fn [[ScopeTable]] (capture root, key vars, star chains).
    */
  def resolveCapture(cap: RawCapture, schema: FnSchema): Res[(TypedCapture, List[(String, ScopeEntry)])] = {
    for {
      rootType <- resolveRaw(cap.rootType, schema).left.map(_.map(m => s"capture `${cap.name}`: $m"))
      rootName = rawBaseName(cap.rootType)
      acc <- resolveSteps(cap.name, cap.path, rootType, rootName, schema, nestedStarOk = true)
      (steps, endpoint, bindings) = acc
      _ <- checkBinders(cap.name, bindings)
    } yield (
      TypedCapture(cap.name, rootType, rootName, steps, endpoint),
      (cap.name -> (ScopeEntry.CaptureRoot(endpoint): ScopeEntry)) :: bindings,
    )
  }

  private def checkBinders(capName: String, bindings: List[(String, ScopeEntry)]): Res[Unit] = {
    val names = bindings.map(_._1)
    val dups  = names.groupBy(identity).collect { case (n, l) if l.size > 1 => n }
    val errs  = dups.map(n => s"duplicate capture binder: $n").toList ++
      (if (names.contains(capName)) List(s"binder shadows capture name: $capName") else Nil)
    if (errs.nonEmpty) Left(errs) else Right(())
  }

  private final case class Acc(
    steps: List[TypedCaptureStep],
    cur: TypeRef,
    bindings: List[(String, ScopeEntry)],
  )

  private def resolveSteps(
    capName: String,
    path: List[RawCaptureStep],
    start: TypeRef,
    startOwner: String,
    schema: FnSchema,
    nestedStarOk: Boolean,
  ): Res[(List[TypedCaptureStep], TypeRef, List[(String, ScopeEntry)])] = {
    val init: Res[Acc] = ok(Acc(Nil, start, Nil))
    val out = path.foldLeft(init) { (accR, step) =>
      accR.flatMap(acc => resolveOne(capName, step, acc, schema, nestedStarOk))
    }
    out.map(a => (a.steps.reverse, a.cur, a.bindings.reverse))
  }

  private def resolveOne(
    capName: String,
    step: RawCaptureStep,
    acc: Acc,
    schema: FnSchema,
    nestedStarOk: Boolean,
  ): Res[Acc] = step match {
    case RawCaptureStep.Single(seg) => resolveSeg(capName, seg, acc, schema)
    case RawCaptureStep.Star(star) =>
      if (!nestedStarOk) fail(s"capture `$capName': nested stars are not supported (chain `${star.chainName}`)")
      else resolveStar(capName, star, acc, schema)
  }

  private def resolveSeg(
    capName: String,
    seg: RawLocatorSeg,
    acc: Acc,
    schema: FnSchema,
  ): Res[Acc] = seg match {
    case RawLocatorSeg.Variant(name) =>
      fail(s"capture `$capName`: variant `$name` is not allowed in a capture path")
    case RawLocatorSeg.Key(varName) =>
      acc.cur match {
        case t if isCollection(t) =>
          (for {
            kt <- keyType(t).toRight(List(s"capture `$capName`: cannot bind key on $t"))
            et <- elemType(t).toRight(List(s"capture `$capName`: cannot situate into $t"))
          } yield Acc(
            TypedCaptureStep.KeyStep(varName, kt) :: acc.steps,
            et,
            (varName -> (ScopeEntry.KeyVar(kt): ScopeEntry)) :: acc.bindings,
          ))
        case other =>
          fail(s"capture `$capName`: key segment `[$varName]` on non-collection $other")
      }
    case RawLocatorSeg.Field(name) =>
      descendField(capName, name, acc.cur, schema).map { case (stepOpt, next) =>
        Acc(stepOpt.toList ::: acc.steps, next, acc.bindings)
      }
  }

  /** Resolve one `.field` against `cur`, descending transparently through
    * collections. Returns the emitted step (None when an explicit `.Some`
    * is canonicalized away) and the next type. The step list is in reverse
    * accumulation order (at most one step here, kept as a list for uniformity).
    */
  private def descendField(
    capName: String,
    name: String,
    cur: TypeRef,
    schema: FnSchema,
  ): Res[(Option[TypedCaptureStep], TypeRef)] = {
    // Explicit `.Some` on `opt[T]` is implied situating: canonicalize away.
    cur match {
      case t if isOpt(t) && name == "Some" =>
        elemType(t)
          .toRight(List(s"capture `$capName`: cannot situate into $t"))
          .map(next => (None, next))
      case _ =>
        transparent(cur) match {
          case t if isCollection(t) =>
            // Still a collection after descent (map with field traversal goes
            // to the value type; nested collections stay transparent).
            elemType(t)
              .toRight(List(s"capture `$capName`: cannot situate into $t"))
              .flatMap(et => descendField(capName, name, et, schema))
          case t =>
            dtoFields(t, schema) match {
              case Some(fields) =>
                fields.find(_.name.name == name) match {
                  case Some(f) =>
                    ok((Some(TypedCaptureStep.FieldStep(name, ownerName(t), f.tpe)), f.tpe))
                  case None =>
                    fail(s"capture `$capName`: unknown field `$name` on $t")
                }
              case None =>
                fail(s"capture `$capName`: no fields on $t (field `$name`)")
            }
        }
    }
  }

  /** Peel transparent `opt` layers (payload situating). `lst`/`set`/`map`
    * are NOT peeled here: field traversal through them goes via `elemType`
    * in `descendField`, while star roots must see the collection itself.
    */
  private def transparent(cur: TypeRef): TypeRef = cur match {
    case t if isOpt(t) => elemType(t).map(transparent).getOrElse(t)
    case t            => t
  }

  private def resolveStar(
    capName: String,
    star: RawStarCapture,
    acc: Acc,
    schema: FnSchema,
  ): Res[Acc] = {
    val rooted = transparent(acc.cur)
    if (!isLst(rooted) && !isSet(rooted) && !isMap(rooted))
      return fail(s"capture `$capName`: star chain `${star.chainName}` roots in $rooted, expected lst/map/set")
    val elem: Res[TypeRef] = elemType(rooted).toRight(List(s"capture `$capName`: cannot iterate $rooted"))
    elem.flatMap { e =>
      // Star sub-steps are flat (the parser rejects nested stars); resolve
      // them against the element type with nesting disabled, defense in depth.
      val sub: Res[(List[TypedCaptureStep], TypeRef, List[(String, ScopeEntry)])] =
        resolveSegsFlat(capName, star.steps, e, schema)
      sub.map { case (substeps, endpoint, subBindings) =>
        Acc(
          TypedCaptureStep.StarStep(star.chainName, endpoint, substeps) :: acc.steps,
          endpoint,
          (star.chainName -> (ScopeEntry.StarChain(endpoint): ScopeEntry)) :: subBindings ::: acc.bindings,
        )
      }
    }
  }

  private def resolveSegsFlat(
    capName: String,
    segs: List[RawLocatorSeg],
    start: TypeRef,
    schema: FnSchema,
  ): Res[(List[TypedCaptureStep], TypeRef, List[(String, ScopeEntry)])] = {
    val init: Res[Acc] = ok(Acc(Nil, start, Nil))
    val out = segs.foldLeft(init) { (accR, seg) =>
      accR.flatMap { acc =>
        seg match {
          case RawLocatorSeg.Variant(name) =>
            Left(List(s"capture `$capName`: variant `$name` is not allowed in a capture path"))
          case RawLocatorSeg.Key(varName) =>
            acc.cur match {
              case t if isCollection(t) =>
                (for {
                  kt <- keyType(t).toRight(List(s"capture `$capName`: cannot bind key on $t"))
                  et <- elemType(t).toRight(List(s"capture `$capName`: cannot situate into $t"))
                } yield Acc(
                  TypedCaptureStep.KeyStep(varName, kt) :: acc.steps,
                  et,
                  (varName -> (ScopeEntry.KeyVar(kt): ScopeEntry)) :: acc.bindings,
                ))
              case other =>
                Left(List(s"capture `$capName`: key segment `[$varName]` on non-collection $other"))
            }
          case RawLocatorSeg.Field(name) =>
            descendField(capName, name, acc.cur, schema).map { case (stepOpt, next) =>
              Acc(stepOpt.toList ::: acc.steps, next, acc.bindings)
            }
        }
      }
    }
    out.map(a => (a.steps.reverse, a.cur, a.bindings.reverse))
  }

  private def ownerName(t: TypeRef): String = t match {
    case TypeRef.Scalar(id: TypeId.User) => id.name.name
    case other                           => other.toString
  }

  private def rawBaseName(raw: RawTypeRef): String = raw match {
    case RawTypeRef.Simple(name, _)       => name.name
    case RawTypeRef.Constructor(name, _, _) => name.name
    case RawTypeRef.AnyRef(_, _)          => "any"
  }
}
