package io.septimalmind.baboon.bbin.model

import io.septimalmind.baboon.parser.model.{FSPath, RawTypeName, RawTypeRef}

final case class RawBbin(
  path: FSPath,
  usings: List[BbinUsing],
  imports: List[BbinImport],
  statements: List[BbinStmt],
)

final case class BbinUsing(pkg: String, version: String)

final case class BbinImport(names: List[BbinImportItem], from: String)

final case class BbinImportItem(name: String, alias: Option[String], ascribed: Option[BbinType])

sealed trait BbinStmt

object BbinStmt {
  final case class LetStmt(let: BbinLet) extends BbinStmt
  final case class AssignStmt(assign: BbinAssign) extends BbinStmt
  final case class ForStmt(loop: BbinFor) extends BbinStmt
  final case class WithStmt(withBlock: BbinWith) extends BbinStmt
}

final case class BbinLet(name: String, tpe: BbinType, expr: BbinExpr)

/** Extended `.bbin` let-type.
  *
  * Plain baboon types back directly onto [[RawTypeRef]]; the `~` partial
  * marker and the `{...}` set shapes are explicit nodes because neither is
  * expressible as a [[RawTypeRef]].
  */
sealed trait BbinType

object BbinType {
  final case class Plain(ref: RawTypeRef) extends BbinType
  final case class Partial(underlying: RawTypeRef) extends BbinType
  final case class SetOf(elem: RawTypeRef) extends BbinType
  final case class IndexedSet(idx: String, idxType: RawTypeRef, elem: RawTypeRef) extends BbinType
}

final case class BbinAssign(target: BbinPath, op: BbinAssignOp, value: BbinExpr)

sealed trait BbinAssignOp

object BbinAssignOp {
  case object Set extends BbinAssignOp
  case object Add extends BbinAssignOp
  case object Mul extends BbinAssignOp
}

/** `for ((idx, row) in set) { stmts }`. Binders are read-only: bodies may
  * read them but must not assign to them or re-bind them.
  */
final case class BbinFor(idx: String, row: String, set: BbinExpr, body: List[BbinStmt])

final case class BbinWith(provenance: List[BbinField], body: List[BbinStmt])

sealed trait BbinExpr

object BbinExpr {
  final case class SetLit(set: BbinSet) extends BbinExpr
  final case class Loader(call: BbinLoaderCall) extends BbinExpr
  final case class Rel(path: String) extends BbinExpr
  final case class SchemaPath(path: BbinPath) extends BbinExpr
  final case class Ident(name: String) extends BbinExpr
  final case class StrLit(value: String) extends BbinExpr
  final case class IntLit(value: Long, raw: String) extends BbinExpr
  final case class FloatLit(value: Double, raw: String) extends BbinExpr
  final case class BoolLit(value: Boolean) extends BbinExpr
  case object NoneLit extends BbinExpr
  case object UndefinedLit extends BbinExpr
  final case class BinOp(op: BbinBinOp, left: BbinExpr, right: BbinExpr) extends BbinExpr
  final case class Leaf(base: BbinExpr, overrides: List[BbinField]) extends BbinExpr
  final case class StructLit(row: BbinRow) extends BbinExpr
  final case class PartialSeal(target: Option[BbinExpr]) extends BbinExpr
  final case class Chain(base: BbinExpr, ops: List[BbinChainOp]) extends BbinExpr
}

sealed trait BbinBinOp

object BbinBinOp {
  case object Eq extends BbinBinOp
  case object Ne extends BbinBinOp
  case object Lt extends BbinBinOp
  case object Le extends BbinBinOp
  case object Gt extends BbinBinOp
  case object Ge extends BbinBinOp
  case object Add extends BbinBinOp
  case object Sub extends BbinBinOp
  case object Mul extends BbinBinOp
  case object Div extends BbinBinOp
}

sealed trait BbinChainOp

object BbinChainOp {
  final case class Map(args: List[BbinMapArg]) extends BbinChainOp
  final case class Filter(pred: BbinExpr) extends BbinChainOp
  final case class Cross(other: BbinExpr) extends BbinChainOp
  final case class InnerJoin(other: BbinExpr) extends BbinChainOp
  final case class OuterJoin(other: BbinExpr) extends BbinChainOp
  final case class LeftJoin(other: BbinExpr) extends BbinChainOp
  final case class RightJoin(other: BbinExpr) extends BbinChainOp
  final case class Zip(other: BbinExpr) extends BbinChainOp
  final case class Concat(other: BbinExpr) extends BbinChainOp
  final case class Reindex(other: BbinExpr) extends BbinChainOp
  case object Seal extends BbinChainOp
  case object Unseal extends BbinChainOp
  final case class WithProvenance(fields: List[BbinField]) extends BbinChainOp
  case object Unwrap extends BbinChainOp
}

final case class BbinSet(entries: List[BbinSetEntry])

final case class BbinSetEntry(key: Option[String], row: BbinRow)

final case class BbinRow(typeName: RawTypeName, fields: List[BbinField])

final case class BbinField(name: String, value: BbinFieldValue)

sealed trait BbinFieldValue

object BbinFieldValue {
  final case class Str(value: String) extends BbinFieldValue
  final case class Int(value: Long, raw: String) extends BbinFieldValue
  final case class Float(value: Double, raw: String) extends BbinFieldValue
  final case class Bool(value: Boolean) extends BbinFieldValue
  case object NoneLit extends BbinFieldValue
  case object Undefined extends BbinFieldValue
  final case class Path(segments: List[BbinPathSeg]) extends BbinFieldValue
  final case class Rel(path: String) extends BbinFieldValue
  final case class Ident(name: String) extends BbinFieldValue
}

sealed trait BbinLoaderKind

object BbinLoaderKind {
  case object Csv extends BbinLoaderKind
  case object Json extends BbinLoaderKind
}

final case class BbinLoaderCall(
  kind: BbinLoaderKind,
  rowType: BbinType,
  source: String,
  maps: List[BbinMapCall],
)

final case class BbinMapCall(args: List[BbinMapArg])

sealed trait BbinMapArg

object BbinMapArg {
  final case class Str(value: String) extends BbinMapArg
  final case class Path(segments: List[BbinPathSeg]) extends BbinMapArg
}

final case class BbinPath(segments: List[BbinPathSeg])

sealed trait BbinPathSeg

object BbinPathSeg {
  final case class Field(name: String) extends BbinPathSeg
  final case class Index(idx: Int) extends BbinPathSeg
}
