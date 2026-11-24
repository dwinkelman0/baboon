package io.septimalmind.baboon.typer.model

import io.septimalmind.baboon.parser.model.{RawFn, RawTypeRef}
import izumi.fundamentals.collections.nonempty.NEList

/** P1/P2/P3 fn-pipeline IR.
  *
  * `RawFn` (parser AST) is lowered in three passes:
  *   - P1 `FnSiteExtractor`: [[RawFn]] + member role => [[FnSite]] (no schema).
  *   - P2 `FnCaptureResolver`: capture root + ordered path => [[TypedCapture]].
  *   - P3 `FnBodyTyper`: body + scope => [[TypedBody]] with `tpe` everywhere.
  */

/** Role a context function plays on its owning field. */
sealed trait FnRole
object FnRole {
  case object Default   extends FnRole
  case object Normalize extends FnRole
  case object Migrate   extends FnRole
  case object Computed  extends FnRole
}

/** P1 output: one extracted fn site. `owner` is a placeholder until the
  * pipeline is bridged into the translator (which owns real `TypeId`s).
  */
final case class FnSiteId(owner: String, field: String, role: FnRole)
final case class FnSite(id: FnSiteId, rawFn: RawFn)

/** One resolved step of a capture path, in source order. */
sealed trait TypedCaptureStep
object TypedCaptureStep {
  final case class FieldStep(field: String, owner: String, fieldType: TypeRef) extends TypedCaptureStep
  final case class KeyStep(varName: String, elemType: TypeRef)                 extends TypedCaptureStep
  final case class StarStep(chain: String, elemType: TypeRef, substeps: List[TypedCaptureStep]) extends TypedCaptureStep
}

/** P2 output: a capture whose root and every path step are schema-resolved.
  * `endpoint` is the type the full path situates at.
  */
final case class TypedCapture(
  name: String,
  rootType: TypeRef,
  rootTypeName: String,
  path: List[TypedCaptureStep],
  endpoint: TypeRef,
)

/** What a name in a fn scope refers to. */
sealed trait ScopeEntry {
  def tpe: TypeRef
}
object ScopeEntry {
  final case class CaptureRoot(tpe: TypeRef) extends ScopeEntry
  final case class Param(tpe: TypeRef)       extends ScopeEntry
  final case class LetBinding(tpe: TypeRef)  extends ScopeEntry
  final case class StarChain(tpe: TypeRef)   extends ScopeEntry
  /** Read-only key variable bound by a capture `[$k]` / star-subpath key
    * step. Readable like a let, but never an assignment target (node-level
    * `@capture` shares these across every fn of the node). */
  final case class KeyVar(tpe: TypeRef)      extends ScopeEntry
}

/** Per-fn scope table: `name -> entry`. */
final case class ScopeTable(entries: Map[String, ScopeEntry]) {
  def get(name: String): Option[ScopeEntry] = entries.get(name)
  def +(kv: (String, ScopeEntry)): ScopeTable = copy(entries = entries + kv)
  def ++(kvs: Iterable[(String, ScopeEntry)]): ScopeTable = copy(entries = entries ++ kvs)
}
object ScopeTable {
  val empty: ScopeTable = ScopeTable(Map.empty)
}

/** A body literal with its closed type. */
sealed trait TypedLit {
  def tpe: TypeRef
}
object TypedLit {
  final case class IntLit(value: Long)     extends TypedLit { def tpe: TypeRef = TypeRef.Scalar(TypeId.Builtins.i32) }
  final case class FloatLit(value: Double) extends TypedLit { def tpe: TypeRef = TypeRef.Scalar(TypeId.Builtins.f64) }
  final case class BoolLit(value: Boolean) extends TypedLit { def tpe: TypeRef = TypeRef.Scalar(TypeId.Builtins.bit) }
  final case class StrLit(value: String)   extends TypedLit { def tpe: TypeRef = TypeRef.Scalar(TypeId.Builtins.str) }
  final case class NullLit() extends TypedLit {
    // `null` conforms to any `opt[T]` (see `FnTypes.conforms`); the carried
    // `opt[str]` is only a placeholder so `tpe` is total.
    def tpe: TypeRef =
      TypeRef.Constructor(TypeId.Builtins.opt, NEList.unsafeFrom(List(TypeRef.Scalar(TypeId.Builtins.str))))
  }
}

/** P3 output: every node carries its `tpe`. Statement-position nodes
  * (`Assign`, `ForIn`, `ForC`, value-less `Block`) carry `tpe` of their
  * natural result (`Assign` => value type; loops / valueless blocks / asserts
  * => `void`, never inspected for conformance).
  */
sealed trait TypedExpr {
  def tpe: TypeRef
}
object TypedExpr {
  /** Shared visitor: every `TypedExpr`/`TypedStmt` recursion in the pipeline
    * (P4 callee union, R12 body-type sweep, assert collection, sensitivity)
    * goes through here, so new ADT cases need exactly one edit.
    */
  def foreach(e: TypedExpr)(f: TypedExpr => Unit): Unit = {
    f(e)
    e match {
      case Lit(_, _) => ()
      case Path(_, segs, _) =>
        segs.foreach {
          case TypedPathSeg.Index(i, _) => foreach(i)(f)
          case _                        => ()
        }
      case Call(t, _, as, _)     => foreach(t)(f); as.foreach(foreach(_)(f))
      case UserCall(t, _, as, _) => foreach(t)(f); as.foreach(foreach(_)(f))
      case BinOp(_, l, r, _)     => foreach(l)(f); foreach(r)(f)
      case UnaryOp(_, x, _)      => foreach(x)(f)
      case If(c, t, el, _)       => foreach(c)(f); foreach(t)(f); foreach(el)(f)
      case Assign(t, v, _)       => foreach(t)(f); foreach(v)(f)
      case ForIn(_, it, b, _)    => foreach(it)(f); foreachBlock(b)(f)
      case ForC(i, c, s, b, _)   => foreach(i.value)(f); foreach(c)(f); foreach(s)(f); foreachBlock(b)(f)
      case Match(s, arms, _)     => foreach(s)(f); arms.foreach(a => foreachBlock(a.body)(f))
      case Block(sts, v, _)      => sts.foreach(foreachStmt(_)(f)); v.foreach(foreach(_)(f))
    }
  }
  def foreachStmt(s: TypedStmt)(f: TypedExpr => Unit): Unit = s match {
    case TypedStmt.Let(_, _, v) => foreach(v)(f)
    case TypedStmt.Effect(e)    => foreach(e)(f)
    case TypedStmt.Assert(a)    => foreach(a.cond)(f); a.msg.foreach(foreach(_)(f))
  }
  def foreachBlock(b: TypedExpr.Block)(f: TypedExpr => Unit): Unit = {
    b.stmts.foreach(foreachStmt(_)(f)); b.value.foreach(foreach(_)(f))
  }
  def fold[A](e: TypedExpr, z: A)(op: (A, TypedExpr) => A): A = {
    var acc = z
    foreach(e)(n => acc = op(acc, n))
    acc
  }
  /** Direct child expressions. `Block`/`For`/`Match` bodies flatten to their
    * expression children (see `stmtChildren`); nested `Assert` *statements*
    * inside those bodies are NOT included — use `foreachStmt`/`foreachBlock`
    * for statement-aware walks.
    */
  def children(e: TypedExpr): List[TypedExpr] = e match {
    case Lit(_, _)             => Nil
    case Path(_, segs, _)      => segs.collect { case TypedPathSeg.Index(i, _) => i }
    case Call(t, _, as, _)     => t :: as
    case UserCall(t, _, as, _) => t :: as
    case BinOp(_, l, r, _)     => List(l, r)
    case UnaryOp(_, x, _)      => List(x)
    case If(c, t, el, _)       => List(c, t, el)
    case Assign(t, v, _)       => List(t, v)
    case ForIn(_, it, b, _)    => it :: blockChildren(b)
    case ForC(i, c, s, b, _)   => i.value :: c :: s :: blockChildren(b)
    case Match(s, arms, _)     => s :: arms.flatMap(a => blockChildren(a.body))
    case Block(sts, v, _)      => sts.flatMap(stmtChildren) ++ v.toList
  }
  def stmtChildren(s: TypedStmt): List[TypedExpr] = s match {
    case TypedStmt.Let(_, _, v) => List(v)
    case TypedStmt.Effect(e)    => List(e)
    case TypedStmt.Assert(a)    => a.cond :: a.msg.toList
  }
  def blockChildren(b: TypedExpr.Block): List[TypedExpr] =
    b.stmts.flatMap(stmtChildren) ++ b.value.toList
  final case class Lit(lit: TypedLit, tpe: TypeRef) extends TypedExpr
  final case class Path(root: String, segments: List[TypedPathSeg], tpe: TypeRef) extends TypedExpr
  final case class Call(target: TypedExpr, method: String, args: List[TypedExpr], tpe: TypeRef) extends TypedExpr
  final case class UserCall(target: TypedExpr, method: String, args: List[TypedExpr], tpe: TypeRef) extends TypedExpr
  final case class BinOp(op: String, left: TypedExpr, right: TypedExpr, tpe: TypeRef) extends TypedExpr
  final case class UnaryOp(op: String, expr: TypedExpr, tpe: TypeRef) extends TypedExpr
  final case class If(cond: TypedExpr, thenE: TypedExpr, elseE: TypedExpr, tpe: TypeRef) extends TypedExpr
  final case class Assign(target: TypedExpr, value: TypedExpr, tpe: TypeRef) extends TypedExpr
  final case class ForIn(varName: String, iterable: TypedExpr, body: Block, tpe: TypeRef) extends TypedExpr
  final case class ForC(init: TypedForInit, cond: TypedExpr, step: TypedExpr, body: Block, tpe: TypeRef) extends TypedExpr
  final case class Match(scrutinee: TypedExpr, arms: List[TypedMatchArm], tpe: TypeRef) extends TypedExpr
  final case class Block(stmts: List[TypedStmt], value: Option[TypedExpr], tpe: TypeRef) extends TypedExpr
}

sealed trait TypedPathSeg
object TypedPathSeg {
  final case class Field(name: String, tpe: TypeRef) extends TypedPathSeg
  final case class Index(index: TypedExpr, tpe: TypeRef) extends TypedPathSeg
}

final case class TypedForInit(name: String, tpe: TypeRef, value: TypedExpr)
final case class TypedMatchArm(pattern: TypedPattern, body: TypedExpr.Block)
sealed trait TypedPattern
object TypedPattern {
  final case class Variant(name: String, binding: Option[String], payload: Option[TypeRef]) extends TypedPattern
  case object Wildcard extends TypedPattern
}

sealed trait TypedStmt
object TypedStmt {
  final case class Let(name: String, tpe: TypeRef, value: TypedExpr) extends TypedStmt
  final case class Effect(expr: TypedExpr) extends TypedStmt
  final case class Assert(assert: TypedAssert) extends TypedStmt
}

/** P3 output for one assertion: `cond` must be `bit`, `msg` (when present)
  * must be `str`. `capturedVars` records the distinct `Path`s involved
  * in `cond`, in first-occurrence order, for backend trace metadata: full
  * paths (e.g. `self.opsPerTask`, `element[i]`) so a failure reports only
  * the participating values instead of dumping whole root objects.
  * `trace` is a backend slot (e.g. rendered message template); P3 leaves it
  * empty (`None`).
  */
final case class TypedAssert(
  cond: TypedExpr,
  msg: Option[TypedExpr],
  capturedVars: List[TypedExpr.Path],
  trace: Option[String] = None,
)

/** P3 output for one fn body: typed statements plus the typed result.
  * `assertions` is the flattened list of every `TypedAssert` in the body
  * (including ones nested in blocks, loops and match arms), in source order.
  */
final case class TypedBody(stmts: List[TypedStmt], result: TypedExpr, tpe: TypeRef, assertions: List[TypedAssert] = Nil)

/** One ADT variant for match checking: `payload=None` means the variant
  * carries no data (e.g. `None`); `Some(t)` is the binding type.
  */
final case class FnAdtVariant(name: String, payload: Option[TypeRef])

/** Minimal schema view for P2/P3: user typedefs by simple name plus ADT
  * variant tables by simple type name. Test-built; the translator bridge
  * will project the real `Domain` into this shape later.
  */
final case class FnSchema(
  types: Map[String, Typedef],
  adts: Map[String, List[FnAdtVariant]],
  /** Root owners (projected from `DomainMember.User.isRoot`): a root node
    * may be instantiated without a parent, so no capture is allowed on it
    * even when the static parent graph shows incoming edges.
    */
  roots: Set[String] = Set.empty,
) {
  def lookup(name: String): Option[Typedef] = types.get(name)
}
object FnSchema {
  val empty: FnSchema = FnSchema(Map.empty, Map.empty)
}

/** Shared type helpers for P2/P3: `RawTypeRef` resolution, collection
  * transparency, conformance and the closed operator/method tables.
  */
object FnTypes {
  type Err = List[String]
  type Res[T] = Either[Err, T]

  def ok[T](t: T): Res[T] = Right(t)
  def fail[T](msg: String): Res[T] = Left(List(msg))

  def scalar(name: String): TypeRef =
    TypeRef.Scalar(TypeId.BuiltinScalar(TypeName(name)))

  /** Explicit unit type for valueless positions (`void`). */
  def voidTpe: TypeRef = TypeRef.Scalar(TypeId.Builtins.void)

  def isVoid(t: TypeRef): Boolean = t match {
    case TypeRef.Scalar(id: TypeId.BuiltinScalar) => id == TypeId.Builtins.void
    case _                                        => false
  }

  def ctor(name: TypeId.BuiltinCollection, args: List[TypeRef]): TypeRef =
    TypeRef.Constructor(name, NEList.unsafeFrom(args))

  def isOpt(t: TypeRef): Boolean = t match {
    case TypeRef.Constructor(id, _) => id == TypeId.Builtins.opt
    case _                          => false
  }

  def isLst(t: TypeRef): Boolean = t match {
    case TypeRef.Constructor(id, _) => id == TypeId.Builtins.lst
    case _                          => false
  }

  def isSet(t: TypeRef): Boolean = t match {
    case TypeRef.Constructor(id, _) => id == TypeId.Builtins.set
    case _                          => false
  }

  def isMap(t: TypeRef): Boolean = t match {
    case TypeRef.Constructor(id, _) => id == TypeId.Builtins.map
    case _                          => false
  }

  def isCollection(t: TypeRef): Boolean =
    isOpt(t) || isLst(t) || isSet(t) || isMap(t)

  def collArgs(t: TypeRef): List[TypeRef] = t match {
    case TypeRef.Constructor(_, args) => args.toList
    case _                            => Nil
  }

  /** Element type a collection iterates / situates into:
    * `lst[T]/set[T] -> T`, `opt[T] -> T`, `map[K, V] -> V`.
    */
  def elemType(t: TypeRef): Option[TypeRef] = t match {
    case TypeRef.Constructor(id, args) if id == TypeId.Builtins.map => args.toList.lift(1)
    case TypeRef.Constructor(id, args)
        if id == TypeId.Builtins.lst || id == TypeId.Builtins.set || id == TypeId.Builtins.opt =>
      args.toList.headOption
    case _ => None
  }

  /** Key type a `[$k]` binder takes: `map[K, V] -> K`, `lst/set -> i32`
    * (positional index), `opt[T] -> T`'s key (transparent, rarely used).
    */
  def keyType(t: TypeRef): Option[TypeRef] = t match {
    case TypeRef.Constructor(id, args) if id == TypeId.Builtins.map => args.toList.headOption
    case TypeRef.Constructor(id, _) if id == TypeId.Builtins.lst || id == TypeId.Builtins.set =>
      Some(TypeRef.Scalar(TypeId.Builtins.i32))
    case TypeRef.Constructor(id, args) if id == TypeId.Builtins.opt => args.toList.headOption
    case _ => None
  }

  /** Resolve a `RawTypeRef` against builtins + the provided schema.
    * User generics are rejected (type args only live on builtin collections).
    */
  def resolveRaw(raw: RawTypeRef, schema: FnSchema): Res[TypeRef] = raw match {
    case RawTypeRef.Simple(name, _) =>
      builtinScalar(name.name).map(ok).getOrElse {
        schema.lookup(name.name) match {
          case Some(d: Typedef.Dto)      => ok(TypeRef.Scalar(d.id))
          case Some(e: Typedef.Enum)     => ok(TypeRef.Scalar(e.id))
          case Some(a: Typedef.Adt)      => ok(TypeRef.Scalar(a.id))
          case Some(c: Typedef.Contract) => ok(TypeRef.Scalar(c.id))
          case Some(other)               => fail(s"unknown type reference: ${name.name} (unsupported typedef $other)")
          case None                      => fail(s"unknown type: ${name.name}")
        }
      }
    case RawTypeRef.Constructor(name, params, _) =>
      val args = params.toList
      name.name match {
        case "lst" | "set" | "opt" if args.size == 1 =>
          traverse(args.map(resolveRaw(_, schema))).map(a => ctor(collId(name.name), a))
        case "map" if args.size == 2 =>
          traverse(args.map(resolveRaw(_, schema))).map(a => ctor(TypeId.Builtins.map, a))
        case other =>
          fail(s"unsupported generic type: $other[${args.map(_.render).mkString(", ")}]")
      }
    case RawTypeRef.AnyRef(_, _) =>
      fail("`any` types are not supported in fn signatures")
  }

  private def builtinScalar(name: String): Option[TypeRef] = name match {
    case "bit" | "i08" | "i16" | "i32" | "i64" | "u08" | "u16" | "u32" | "u64" |
        "f32" | "f64" | "f128" | "str" | "uid" | "tsu" | "tso" | "bytes" =>
      Some(scalar(name))
    case _ => None
  }

  private def collId(name: String): TypeId.BuiltinCollection = name match {
    case "lst" => TypeId.Builtins.lst
    case "set" => TypeId.Builtins.set
    case _     => TypeId.Builtins.opt
  }

  def traverse[T](l: List[Res[T]]): Res[List[T]] =
    l.foldRight(ok(List.empty[T])) { (e, acc) =>
      for { h <- e; t <- acc } yield h :: t
    }

  /** Field lookup on a DTO/contract/ADT typedef by simple type name. */
  def dtoFields(tpe: TypeRef, schema: FnSchema): Option[List[Field]] = tpe match {
    case TypeRef.Scalar(id: TypeId.User) =>
      schema.lookup(id.name.name) match {
        case Some(d: Typedef.Dto)      => Some(d.fields)
        case Some(c: Typedef.Contract) => Some(c.fields)
        case Some(a: Typedef.Adt)      => Some(a.fields)
        case _                         => None
      }
    case _ => None
  }

  /** Owner display name for diagnostics: user-type simple name, otherwise the full type rendering. */
  def ownerName(t: TypeRef): String = t match {
    case TypeRef.Scalar(id: TypeId.User) => id.name.name
    case other                           => other.toString
  }

  def isNumeric(t: TypeRef): Boolean = t match {
    case TypeRef.Scalar(id: TypeId.BuiltinScalar) =>
      Set("i08", "i16", "i32", "i64", "u08", "u16", "u32", "u64", "f32", "f64", "f128").contains(id.name.name)
    case _ => false
  }

  def isFloat(t: TypeRef): Boolean = t match {
    case TypeRef.Scalar(id: TypeId.BuiltinScalar) =>
      Set("f32", "f64", "f128").contains(id.name.name)
    case _ => false
  }

  def isInt(t: TypeRef): Boolean = isNumeric(t) && !isFloat(t)

  def isBool(t: TypeRef): Boolean = t match {
    case TypeRef.Scalar(id: TypeId.BuiltinScalar) => id.name.name == "bit"
    case _                                       => false
  }

  def isStr(t: TypeRef): Boolean = t match {
    case TypeRef.Scalar(id: TypeId.BuiltinScalar) => id.name.name == "str"
    case _                                        => false
  }

  private def numRank(t: TypeRef): Option[(Int, Boolean)] = t match {
    case TypeRef.Scalar(id: TypeId.BuiltinScalar) =>
      id.name.name match {
        case "i08" => Some((0, false))
        case "i16" => Some((1, false))
        case "i32" => Some((2, false))
        case "i64" => Some((3, false))
        case "u08" => Some((0, true))
        case "u16" => Some((1, true))
        case "u32" => Some((2, true))
        case "u64" => Some((3, true))
        case "f32" => Some((4, false))
        case "f64" => Some((5, false))
        case "f128" => Some((6, false))
        case _     => None
      }
    case _ => None
  }

  private def rankTpe(rank: Int, unsigned: Boolean): TypeRef =
    if (rank <= 3) {
      if (unsigned) scalar(rank match {
        case 0 => "u08"; case 1 => "u16"; case 3 => "u64"; case _ => "u32"
      })
      else scalar(rank match {
        case 0 => "i08"; case 1 => "i16"; case 3 => "i64"; case _ => "i32"
      })
    } else scalar(rank match {
      case 4 => "f32"; case 6 => "f128"; case _ => "f64"
    })

  /** Arithmetic numeric join: same => same; int/int => wider width
    * (mixed signedness rejected); int/float => float; float/float => wider.
    */
  def numericJoin(a: TypeRef, b: TypeRef): Option[TypeRef] =
    if (a == b) Some(a)
    else
      (numRank(a), numRank(b)) match {
        case (Some((ra, sa)), Some((rb, sb))) =>
          if (ra <= 3 && rb <= 3) {
            if (sa != sb) None
            else Some(rankTpe(math.max(ra, rb), sa))
          } else if (ra > 3 && rb > 3) Some(rankTpe(math.max(ra, rb), unsigned = false))
          else Some(rankTpe(math.max(ra, rb), unsigned = false))
        case _ => None
      }

  /** Which literal form (if any) produced `actual`: int/float literals carry
    * their default widths (`i32`/`f64`) and conform to any numeric/float,
    * `null` conforms to any `opt`; anything else must match strictly.
    */
  sealed trait LiteralKind
  object LiteralKind {
    case object Plain extends LiteralKind
    case object IntLit extends LiteralKind
    case object FloatLit extends LiteralKind
    case object NullLit extends LiteralKind
  }

  /** Value of type `actual` may flow into a slot of type `expected`.
    * Strict equality, plus: `i32`-defaulted int literals fit any numeric,
    * `f64`-defaulted float literals fit any float, `null` fits any `opt`.
    */
  def conforms(actual: TypeRef, kind: LiteralKind, expected: TypeRef): Boolean =
    actual == expected ||
      (kind == LiteralKind.IntLit && isNumeric(expected)) ||
      (kind == LiteralKind.FloatLit && isFloat(expected)) ||
      (kind == LiteralKind.NullLit && isOpt(expected))

  def conformsLit(actual: TypedExpr, expected: TypeRef): Boolean = actual match {
    case TypedExpr.Lit(TypedLit.IntLit(_), t)   => conforms(t, LiteralKind.IntLit, expected)
    case TypedExpr.Lit(TypedLit.FloatLit(_), t) => conforms(t, LiteralKind.FloatLit, expected)
    case TypedExpr.Lit(TypedLit.NullLit(), t)  => conforms(t, LiteralKind.NullLit, expected)
    case other                                  => conforms(other.tpe, LiteralKind.Plain, expected)
  }

  /** 18-method closed call table (typer-owned rejection: unknown method names are rejected here, not by the parser):
    * str: uppercase/lowercase/capitalize/trim/toString/length/split/join/
    * contains/startsWith/endsWith/concat; numeric: abs/floor/ceil/round/min/max.
    */
  sealed trait MethodSig {
    def receiver: String
    def arity: Int
  }
  object MethodSig {
    final case class Str0(result: TypeRef) extends MethodSig {
      def receiver: String = "str"; def arity: Int = 0
    }
    final case class Str1(result: TypeRef) extends MethodSig {
      def receiver: String = "str"; def arity: Int = 1
    }
    case object Num0 extends MethodSig { def receiver: String = "num"; def arity: Int = 0 }
    case object Num1 extends MethodSig { def receiver: String = "num"; def arity: Int = 1 }
    case object Join extends MethodSig { def receiver: String = "lststr"; def arity: Int = 1 }
    case object Length extends MethodSig { def receiver: String = "str"; def arity: Int = 0 }
  }

  def methodSig(name: String): Option[MethodSig] = {
    import MethodSig._
    name match {
      case "uppercase" | "lowercase" | "capitalize" | "trim" | "toString" => Some(Str0(scalar("str")))
      case "length"      => Some(Length)
      case "split"       => Some(Str1(ctor(TypeId.Builtins.lst, List(scalar("str")))))
      case "contains" | "startsWith" | "endsWith" => Some(Str1(scalar("bit")))
      case "concat"      => Some(Str1(scalar("str")))
      case "join"        => Some(Join)
      case "abs" | "floor" | "ceil" | "round" => Some(Num0)
      case "min" | "max" => Some(Num1)
      case _             => None
    }
  }
}

/** Validated-program fn node as stored on [[Field]] (`normalize` / `migrate` /
  * `computed`) and in [[FieldDefault.Fn]].
  *
  * This is the model-level projection of the pipeline output
  * (`FnPipeline.TypedFn` minus its site): the ordered capture, the typed body
  * and the owning field's expected type. Sensitivity and ordering live in the
  * pipeline program, not here.
  */
case class TypedFn(
  capture: Option[TypedCapture],
  body: TypedBody,
  expected: TypeRef,
  params: List[(String, TypeRef)] = Nil
)

case class TypedMigrate(fn: TypedFn)
