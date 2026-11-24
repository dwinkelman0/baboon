package io.septimalmind.baboon.typer.model

/** Schema-level address of a schema element within one version.
  *
  * Canonical per `plans/schema-instance-paths.md` §1:
  * path-root is the top-level container `TypeId.User` (full pkg+owner+name,
  * already namespace-qualified — NOT a `Domain.roots` wire root);
  * steps are schema-only (field steps through `data` members and ADT
  * variant steps only); collections (`lst`/`set`/`map`/`opt`) are
  * transparent and contribute no segment (the traversed element/value
  * type is recorded in `fieldType`, not as a path step).
  *
  * There is no previous-version scope on the path: `!old` has no AST
  * representation and P2 resolves no prev bindings.
  */
case class SchemaPath(
  root: TypeId.User,
  steps: List[SchemaStep] = Nil
) {
  override def toString: String = {
    val segs  = steps.map(_.asString).mkString("/", "/", "")
    s"$root@this$segs"
  }

  /** Dotted rendering (`Owner.a.b`) used in diagnostics. Stable across the
    * P4/P5/P6 passes and their tests; distinct from `toString`.
    */
  def render: String =
    (root.name.name :: steps.map(_.asString)).mkString(".")

  /** Field names of a leaf path (variant steps never occur in leaf reads). */
  def fieldNames: List[String] =
    steps.collect { case SchemaStep.FieldStep(f, _, _) => f.name }

  def headName: Option[String] = fieldNames.headOption

  /** Materialization slot: the head field of a leaf path. */
  def slot: SchemaPath = copy(steps = steps.take(1))
}

sealed trait SchemaStep {
  def asString: String = this match {
    case SchemaStep.FieldStep(field, _, _) => field.name
    case SchemaStep.VariantStep(member)    => s"?${member.name.name}"
  }
}

object SchemaStep {
  case class FieldStep(field: FieldName, owner: TypeId.User, fieldType: TypeRef) extends SchemaStep
  case class VariantStep(member: TypeId.User) extends SchemaStep
}

object SchemaPath {

  /** Resolve `root.seg1.seg2...` against `schema`, recording the owning DTO
    * and field type of every step. Collections (`lst`/`set`/`map`) are
    * transparent for `.field` traversal (the traversed element/value type is
    * recorded in `fieldType`); `opt` layers peel. Error strings mirror the
    * legacy P4/P5 validation messages.
    */
  def resolve(root: TypeId.User, names: List[String], schema: FnSchema): Either[String, SchemaPath] = {
    import FnTypes._
    if (schema.lookup(root.name.name).isEmpty)
      return Left(s"unknown type: ${root.name.name}")
    walk(TypeRef.Scalar(root), names, schema).map(steps => SchemaPath(root, steps))
  }

  /** Extend an already-resolved path with further field names, resolved
    * against the path's endpoint type.
    */
  def extend(base: SchemaPath, names: List[String], schema: FnSchema): Either[String, SchemaPath] = {
    val endpoint: TypeRef =
      base.steps.lastOption.collect { case SchemaStep.FieldStep(_, _, t) => t }.getOrElse(TypeRef.Scalar(base.root))
    walk(endpoint, names, schema).map(extra => base.copy(steps = base.steps ++ extra))
  }

  private def walk(cur: TypeRef, names: List[String], schema: FnSchema): Either[String, List[SchemaStep]] = {
    var at: TypeRef = cur
    val out = List.newBuilder[SchemaStep]
    for (seg <- names) {
      stepField(at, seg, schema) match {
        case Left(m)          => return Left(m)
        case Right((owner, t)) =>
          out += SchemaStep.FieldStep(FieldName(seg), owner, t)
          at = t
      }
    }
    Right(out.result())
  }

  /** One schema step: peel `opt`, situate collections into their element
    * type, then resolve the named field. Returns the field's owning DTO and
    * its type. Error strings mirror the legacy P4/P5 validation messages.
    */
  def stepField(cur: TypeRef, seg: String, schema: FnSchema): Either[String, (TypeId.User, TypeRef)] = {
    import FnTypes._
    val t = peelOpt(cur)
    val elem =
      if (isLst(t) || isSet(t) || isMap(t)) elemType(t)
      else Some(t)
    elem.toRight(s"cannot situate into $t").flatMap { et =>
      for {
        owner <- et match {
          case TypeRef.Scalar(u: TypeId.User) => Right(u)
          case _                              => Left(s"no fields on $et (field `$seg`)")
        }
        fs <- FnTypes.dtoFields(et, schema).toRight(s"no fields on $et (field `$seg`)")
        f  <- fs.find(_.name.name == seg).toRight(s"unknown field `$seg` on $et").map(_.tpe)
      } yield (owner, f)
    }
  }

  private def peelOpt(t: TypeRef): TypeRef = {
    import FnTypes._
    t match {
      case x if isOpt(x) => elemType(x).map(peelOpt).getOrElse(x)
      case x             => x
    }
  }
}
