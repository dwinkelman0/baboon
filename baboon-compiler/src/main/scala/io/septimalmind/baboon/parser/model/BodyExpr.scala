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

  // ── structure-changing nodes (statement position only; expression nodes above are untouched) ──
  final case class Assign(target: BodyExpr, value: BodyExpr) extends BodyExpr
  final case class ForIn(varName: String, iterable: BodyExpr, body: Block) extends BodyExpr
  final case class ForC(init: ForInit, cond: BodyExpr, step: BodyExpr, body: Block) extends BodyExpr
  final case class Match(scrutinee: BodyExpr, arms: List[MatchArm]) extends BodyExpr
  final case class Block(stmts: List[BodyStmt], value: Option[BodyExpr]) extends BodyExpr
}

/** C-style for-loop initializer: `name (: tpe)? = value`. */
final case class ForInit(name: String, tpe: Option[RawTypeRef], value: BodyExpr)

final case class MatchArm(pattern: MatchPattern, body: BodyExpr.Block)

sealed trait MatchPattern

object MatchPattern {
  final case class Variant(name: String, binding: Option[String]) extends MatchPattern
  case object Wildcard extends MatchPattern
}

/** Statement-level nodes for fn bodies and blocks. */
sealed trait BodyStmt

object BodyStmt {
  final case class Let(name: String, tpe: Option[RawTypeRef], value: BodyExpr) extends BodyStmt
  final case class Effect(expr: BodyExpr) extends BodyStmt

  /** Validation assertion: `assert(cond);` or `assert(cond, msg);`.
    *
    * Statement position only (never a trailing value expression): `cond`
    * must be a `bit` expression, `msg` (when present) must be a `str`
    * expression. Parsed with a `;` terminator like the other statements, so
    * it is allowed anywhere statements occur — fn bodies, blocks, for
    * bodies and match arms.
    */
  final case class Assert(cond: BodyExpr, msg: Option[BodyExpr]) extends BodyStmt
}

sealed trait BodyLiteral

object BodyLiteral {
  final case class IntLit(value: Long) extends BodyLiteral
  final case class FloatLit(value: Double) extends BodyLiteral
  final case class BoolLit(value: Boolean) extends BodyLiteral
  final case class StrLit(value: String) extends BodyLiteral
  case object NullLit extends BodyLiteral
}
