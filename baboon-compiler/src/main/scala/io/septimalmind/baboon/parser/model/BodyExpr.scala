package io.septimalmind.baboon.parser.model

sealed trait BodyExpr

object BodyExpr {
  sealed trait Segment

  object Segment {
    final case class Field(name: String) extends Segment
    final case class Variant(name: String) extends Segment
    final case class Index(expr: BodyExpr) extends Segment
  }

  final case class Lit(lit: BodyLiteral) extends BodyExpr
  final case class Path(root: String, segments: List[Segment]) extends BodyExpr
  final case class Call(target: BodyExpr, method: String, args: List[BodyExpr]) extends BodyExpr
  final case class BinOp(op: String, left: BodyExpr, right: BodyExpr) extends BodyExpr
  final case class UnaryOp(op: String, expr: BodyExpr) extends BodyExpr
  final case class If(cond: BodyExpr, thenE: BodyExpr, elseE: BodyExpr) extends BodyExpr
}

sealed trait BodyLiteral

object BodyLiteral {
  final case class IntLit(value: Long) extends BodyLiteral
  final case class FloatLit(value: Double) extends BodyLiteral
  final case class BoolLit(value: Boolean) extends BodyLiteral
  final case class StrLit(value: String) extends BodyLiteral
  case object NullLit extends BodyLiteral
}
