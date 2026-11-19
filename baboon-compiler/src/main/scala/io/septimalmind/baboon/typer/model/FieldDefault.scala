package io.septimalmind.baboon.typer.model

sealed trait FieldDefault {
  override def toString: String = this match {
    case FieldDefault.IntVal(_, raw, _)     => raw
    case FieldDefault.FloatVal(_, raw)      => raw
    case FieldDefault.BoolVal(v)            => v.toString
    case FieldDefault.StrVal(v)             => FieldDefault.renderString(v)
    case FieldDefault.NullVal               => "null"
    case FieldDefault.EnumVal(member)       => member
    case FieldDefault.SpecialFloatVal(kind) =>
      kind match {
        case FieldDefault.SpecialFloatKind.NaN    => "nan"
        case FieldDefault.SpecialFloatKind.PosInf => "+inf"
        case FieldDefault.SpecialFloatKind.NegInf => "-inf"
      }
    case FieldDefault.Fn(_) => "fn(...)"
  }
}

object FieldDefault {
  final case class IntVal(value: Long, raw: String, targetType: TypeId.BuiltinScalar) extends FieldDefault
  final case class FloatVal(value: Double, raw: String) extends FieldDefault
  final case class BoolVal(value: Boolean) extends FieldDefault
  final case class StrVal(value: String) extends FieldDefault
  case object NullVal extends FieldDefault
  final case class EnumVal(memberName: String) extends FieldDefault

  sealed trait SpecialFloatKind
  object SpecialFloatKind {
    case object NaN extends SpecialFloatKind
    case object PosInf extends SpecialFloatKind
    case object NegInf extends SpecialFloatKind
  }

  final case class SpecialFloatVal(kind: SpecialFloatKind) extends FieldDefault

  final case class Fn(fn: TypedFn) extends FieldDefault

  private def renderString(s: String): String = {
    val escaped = s.flatMap {
      case '"'  => "\\\""
      case '\\' => "\\\\"
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case c    => c.toString
    }
    s"\"$escaped\""
  }
}
