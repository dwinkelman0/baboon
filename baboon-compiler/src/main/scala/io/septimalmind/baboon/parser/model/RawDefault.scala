package io.septimalmind.baboon.parser.model

sealed trait RawDefaultValue

object RawDefaultValue {
  final case class IntLit(value: Long, raw: String) extends RawDefaultValue
  final case class FloatLit(value: Double, raw: String) extends RawDefaultValue
  final case class BoolLit(value: Boolean) extends RawDefaultValue
  final case class StrLit(value: String) extends RawDefaultValue
  case object NullLit extends RawDefaultValue
  final case class EnumRef(value: String) extends RawDefaultValue
  final case class Fn(fn: RawFn) extends RawDefaultValue

  sealed trait SpecialFloat extends RawDefaultValue
  object SpecialFloat {
    case object NaN extends SpecialFloat
    case object PosInf extends SpecialFloat
    case object NegInf extends SpecialFloat
  }
}
