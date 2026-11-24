package io.septimalmind.baboon.typer.model

import FieldDefault.*

/** Shared `FieldDefault` lowering used by the source-emitting backends.
  *
  * Literal shapes that differ per language (booleans, null, special floats)
  * are parameterized through [[LangLiterals]]; shapes shared by every backend
  * (raw int/float passthrough, double-quoted string escaping, enum member
  * short name) live here once.
  */
object DefaultLiteralSupport {

  /** Per-language spellings of the non-portable literals. */
  final case class LangLiterals(
    trueLit: String,
    falseLit: String,
    nullLit: String,
    nanLit: String,
    posInfLit: String,
    negInfLit: String,
  )

  val pythonLiterals: LangLiterals =
    LangLiterals("True", "False", "None", "float('nan')", "float('inf')", "float('-inf')")

  val typeScriptLiterals: LangLiterals =
    LangLiterals("true", "false", "null", "NaN", "Infinity", "-Infinity")

  def escapeDoubleQuoted(s: String): String = s.flatMap {
    case '"'  => "\\\""
    case '\\' => "\\\\"
    case '\n' => "\\n"
    case '\r' => "\\r"
    case '\t' => "\\t"
    case c    => c.toString
  }

  def quoted(s: String): String = s"\"${escapeDoubleQuoted(s)}\""

  def enumMemberShortName(memberName: String): String = memberName.split('.').last

  /** Lower a `FieldDefault` to a source literal. `resolveEnum` qualifies the
    * short member name (backend type reference plus keyword escaping).
    * Returns `None` for `Fn` defaults, which are eval-only and lowered
    * per-backend (`@default(fn)`); callers fall back to their fn lowering.
    */
  def literal(fd: FieldDefault, lit: LangLiterals, resolveEnum: String => String): Option[String] =
    fd match {
      case IntVal(_, raw, _) =>
        Some(raw)
      case FloatVal(_, raw) =>
        Some(raw)
      case BoolVal(value) =>
        Some(if (value) lit.trueLit else lit.falseLit)
      case StrVal(value) =>
        Some(quoted(value))
      case NullVal =>
        Some(lit.nullLit)
      case EnumVal(memberName) =>
        Some(resolveEnum(enumMemberShortName(memberName)))
      case SpecialFloatVal(kind) =>
        Some(kind match {
          case SpecialFloatKind.NaN    => lit.nanLit
          case SpecialFloatKind.PosInf => lit.posInfLit
          case SpecialFloatKind.NegInf => lit.negInfLit
        })
      case Fn(_) =>
        None
    }

  /** v1 capability hook: only the Python and TypeScript emitters lower
    * `Field.default` (see [[literal]]); every other backend ignores defaults.
    * Backends call this instead of leaving per-backend TODOs, so the gap is
    * documented once — here — and this is the single point to attach a
    * diagnostic when a backend starts supporting defaults.
    */
  def ignoreDefaults(dto: Typedef.Dto): Unit = ()
}
