package io.septimalmind.baboon.tests

import io.septimalmind.baboon.parser.model._
import io.septimalmind.baboon.typer.{FnBodyTyper, FnCaptureResolver, FnSensitivityExtractor}
import io.septimalmind.baboon.typer.model._
import io.septimalmind.baboon.typer.model.FnTypes.{ctor, scalar}
import izumi.fundamentals.collections.nonempty.NEList
import org.scalatest.wordspec.AnyWordSpec

/** Direct unit tests for the P4 sensitivity extractor with a tiny hand-built schema. */
final class FnSensitivityTest extends AnyWordSpec {

  private val i32 = scalar("i32")
  private val str = scalar("str")
  private val bit = scalar("bit")
  private val f64 = scalar("f64")
  private val lst = TypeId.Builtins.lst
  private val set = TypeId.Builtins.set
  private val map = TypeId.Builtins.map
  private val opt = TypeId.Builtins.opt

  private def uid(name: String): TypeId.User =
    TypeId.User(Pkg(NEList.unsafeFrom(List("t"))), Owner.Toplevel, TypeName(name))
  private def uref(name: String): TypeRef = TypeRef.Scalar(uid(name))
  private def field(name: String, tpe: TypeRef): Field = Field(FieldName(name), tpe, None)

  private val itemDto = Typedef.Dto(
    uid("Item"),
    List(field("label", str), field("qty", i32)),
    Nil,
  )
  private val basketDto = Typedef.Dto(
    uid("Basket"),
    List(
      field("items", ctor(lst, List(uref("Item")))),
      field("byName", ctor(map, List(str, uref("Item")))),
    ),
    Nil,
  )
  private val childDto = Typedef.Dto(
    uid("Child"),
    List(
      field("data", str),
      field("tags", ctor(lst, List(str))),
      field("scores", ctor(map, List(str, i32))),
      field("maybe", ctor(opt, List(str))),
      field("count", i32),
      field("flag", bit),
    ),
    Nil,
  )
  private val shapeAdt = Typedef.Adt(uid("Shape"), NEList.unsafeFrom(List(uid("Circle"))), Nil, Nil)

  private val schema = FnSchema(
    Map(
      "Item"   -> itemDto,
      "Basket" -> basketDto,
      "Child"  -> childDto,
      "Shape"  -> shapeAdt,
    ),
    Map("Shape" -> List(FnAdtVariant("Circle", Some(f64)), FnAdtVariant("Dot", None))),
  )

  // -- small AST builders -----------------------------------------------------

  private def rt(name: String): RawTypeRef = RawTypeRef.Simple(RawTypeName(name), Nil)
  private def intLit(v: Long): BodyExpr = BodyExpr.Lit(BodyLiteral.IntLit(v))
  private def strLit(v: String): BodyExpr = BodyExpr.Lit(BodyLiteral.StrLit(v))
  private def path(root: String, segs: BodyExpr.Segment*): BodyExpr = BodyExpr.Path(root, segs.toList)
  private def fld(name: String): BodyExpr.Segment = BodyExpr.Segment.Field(name)
  private def idx(e: BodyExpr): BodyExpr.Segment = BodyExpr.Segment.Index(e)
  private def letS(name: String, t: String, v: BodyExpr): BodyStmt = BodyStmt.Let(name, Some(rt(t)), v)
  private def eff(e: BodyExpr): BodyStmt = BodyStmt.Effect(e)

  private def rawBody(stmts: List[BodyStmt], result: BodyExpr): RawBody =
    RawBody(stmts.collect { case BodyStmt.Let(n, _, v) => (n, v) }, result, stmts)

  private def mustType(
    stmts: List[BodyStmt],
    result: BodyExpr,
    scope: ScopeTable,
    expected: TypeRef,
  ): TypedBody =
    FnBodyTyper.typeBody(rawBody(stmts, result), scope, schema, expected) match {
      case Right(b)  => b
      case Left(err) => fail(s"expected success, got errors: ${err.mkString("; ")}")
    }

  private def childScope(extra: (String, ScopeEntry)*): ScopeTable =
    FnBodyTyper.seedScope(List("self" -> uref("Child")), Nil) ++ extra

  private def slot(owner: String, fieldName: String): FnSensitivityExtractor.Slot =
    FnSensitivityExtractor.Slot(uid(owner), fieldName)

  private def mustExtract(
    body: TypedBody,
    capture: Option[TypedCapture],
    scope: ScopeTable,
    attachment: FnSensitivityExtractor.Slot,
  ): FnSensitivityExtractor.Sensitivity =
    FnSensitivityExtractor.extract(body, capture, scope, schema, attachment) match {
      case Right(s)  => s
      case Left(err) => fail(s"expected sensitivity, got errors: ${err.mkString("; ")}")
    }

  private def mustFailExtract(
    body: TypedBody,
    capture: Option[TypedCapture],
    scope: ScopeTable,
    attachment: FnSensitivityExtractor.Slot,
  ): List[String] =
    FnSensitivityExtractor.extract(body, capture, scope, schema, attachment) match {
      case Left(err) => err
      case Right(s)  => fail(s"expected extraction failure, got $s")
    }

  private def leaf(owner: String, segs: String*): FnSensitivityExtractor.SchemaPath =
    FnSensitivityExtractor.SchemaPath(uid(owner), segs.toList)

  // -- hand-built typed snippets (bypass the typer where it would reject the shape) --

  private def tLit(v: Long): TypedExpr = TypedExpr.Lit(TypedLit.IntLit(v), TypedLit.IntLit(v).tpe)
  private def tStrLit(v: String): TypedExpr = TypedExpr.Lit(TypedLit.StrLit(v), TypedLit.StrLit(v).tpe)
  private def tPath(root: String, segs: List[TypedPathSeg], tpe: TypeRef): TypedExpr =
    TypedExpr.Path(root, segs, tpe)
  private def tFld(name: String, tpe: TypeRef): TypedPathSeg = TypedPathSeg.Field(name, tpe)
  private def tBody(stmts: List[TypedStmt], result: TypedExpr): TypedBody =
    TypedBody(stmts, result, result.tpe)

  "P4 sensitivity (positives)" should {
    "read a leaf through a capture path" in {
      val raw = RawCapture("b", rt("Basket"), List(RawCaptureStep.Single(RawLocatorSeg.Field("items"))))
      val (cap, binds) = FnCaptureResolver.resolveCapture(raw, schema) match {
        case Right(v)  => v
        case Left(err) => fail(s"capture failed: ${err.mkString("; ")}")
      }
      val scope = FnBodyTyper.seedScope(Nil, binds)
      val body  = mustType(Nil, path("b", fld("label")), scope, str)
      val s     = mustExtract(body, Some(cap), scope, slot("Basket", "items"))
      assert(s.leafReads == Set(leaf("Basket", "items", "label")))
      assert(s.shapeReads.isEmpty)
      assert(s.prevReads.isEmpty)
      assert(s.writes == Set(leaf("Basket", "items")))
    }

    "read a self param as the owning DTO slot" in {
      val body = mustType(Nil, path("self", fld("count")), childScope(), i32)
      val s    = mustExtract(body, None, childScope(), slot("Child", "count"))
      assert(s.leafReads == Set(leaf("Child", "count")))
      assert(s.shapeReads.isEmpty)
      assert(s.writes == Set(leaf("Child", "count")))
    }

    "record collection shape for for-in over a field" in {
      val loop = BodyExpr.ForIn(
        "t",
        path("self", fld("tags")),
        BodyExpr.Block(Nil, Some(path("t"))),
      )
      val body = mustType(List(eff(loop)), strLit("done"), childScope(), str)
      val s    = mustExtract(body, None, childScope(), slot("Child", "flag"))
      assert(s.leafReads == Set(leaf("Child", "tags")))
      assert(s.shapeReads == Set(uid("Child")))
      assert(s.writes == Set(leaf("Child", "flag")))
    }

    "read star element leafs plus the collection shape" in {
      val raw = RawCapture(
        "b",
        rt("Basket"),
        List(
          RawCaptureStep.Single(RawLocatorSeg.Field("items")),
          RawCaptureStep.Star(RawStarCapture("c", List(RawLocatorSeg.Field("label")))),
        ),
      )
      val (cap, binds) = FnCaptureResolver.resolveCapture(raw, schema) match {
        case Right(v)  => v
        case Left(err) => fail(s"capture failed: ${err.mkString("; ")}")
      }
      val scope = FnBodyTyper.seedScope(Nil, binds)
      val body  = mustType(Nil, path("c"), scope, str)
      val s     = mustExtract(body, Some(cap), scope, slot("Basket", "items"))
      assert(s.leafReads == Set(leaf("Basket", "items", "label")))
      assert(s.shapeReads == Set(uid("Basket")))
    }

    "record a field assign as a write" in {
      val target = path("self", fld("count"))
      val body = mustType(
        List(eff(BodyExpr.Assign(target, intLit(1)))),
        path("self", fld("count")),
        childScope(),
        i32,
      )
      val s = mustExtract(body, None, childScope(), slot("Child", "flag"))
      assert(s.leafReads == Set(leaf("Child", "count")))
      assert(s.shapeReads.isEmpty)
      assert(s.writes == Set(leaf("Child", "flag"), leaf("Child", "count")))
    }

    "expand a whole-record replacement to all fields (struct rule)" in {
      val selfT  = uref("Child")
      val target = tPath("self", Nil, selfT)
      val body   = tBody(List(TypedStmt.Effect(TypedExpr.Assign(target, tStrLit("x"), selfT))), tLit(0))
      val s      = mustExtract(body, None, childScope(), slot("Child", "count"))
      assert(s.leafReads.isEmpty)
      assert(s.writes == Set(
        leaf("Child", "data"),
        leaf("Child", "tags"),
        leaf("Child", "scores"),
        leaf("Child", "maybe"),
        leaf("Child", "count"),
        leaf("Child", "flag"),
      ))
    }

    "collect assert condition reads" in {
      val cond = BodyExpr.BinOp(">", path("self", fld("count")), intLit(0))
      val body = mustType(List(BodyStmt.Assert(cond, None)), path("self", fld("count")), childScope(), i32)
      val s    = mustExtract(body, None, childScope(), slot("Child", "count"))
      assert(s.leafReads == Set(leaf("Child", "count")))
      assert(body.assertions.size == 1)
    }

    "collect match scrutinee leaf and shape" in {
      val m = BodyExpr.Match(
        path("self", fld("maybe")),
        List(
          MatchArm(MatchPattern.Variant("Some", Some("x")), BodyExpr.Block(Nil, Some(path("x")))),
          MatchArm(MatchPattern.Variant("None", None), BodyExpr.Block(Nil, Some(strLit("")))),
        ),
      )
      val body = mustType(List(eff(m)), strLit("z"), childScope(), str)
      val s    = mustExtract(body, None, childScope(), slot("Child", "data"))
      assert(s.leafReads == Set(leaf("Child", "maybe")))
      assert(s.shapeReads == Set(uid("Child")))
    }

    "record the receiver owner for shape-inspecting calls" in {
      val itemT  = uref("Item")
      val target = tPath("item", Nil, itemT)
      val call   = TypedExpr.Call(target, "trim", Nil, str)
      val body   = tBody(Nil, call)
      val scope  = childScope("item" -> (ScopeEntry.Param(itemT): ScopeEntry))
      val s      = mustExtract(body, None, scope, slot("Child", "count"))
      assert(s.leafReads.isEmpty)
      assert(s.shapeReads == Set(uid("Item")))
    }

    "record map index assign as slot plus map shape" in {
      val scope  = childScope("k" -> (ScopeEntry.Param(str): ScopeEntry))
      val target = path("self", fld("scores"), idx(path("k")))
      val body   = mustType(List(eff(BodyExpr.Assign(target, intLit(7)))), strLit("ok"), scope, str)
      val s      = mustExtract(body, None, scope, slot("Child", "flag"))
      assert(s.leafReads.isEmpty)
      assert(s.shapeReads == Set(uid("Child")))
      assert(s.writes == Set(leaf("Child", "flag"), leaf("Child", "scores")))
    }
  }

  "P4 sensitivity (negatives)" should {
    "reject assign to an unknown slot" in {
      val target = tPath("ghost", List(tFld("x", i32)), i32)
      val body   = tBody(List(TypedStmt.Effect(TypedExpr.Assign(target, tLit(1), i32))), tLit(0))
      val errs   = mustFailExtract(body, None, childScope(), slot("Child", "count"))
      assert(errs.mkString.contains("unknown assign target"))
    }

    "re-validate schema paths" in {
      val bad  = tPath("self", List(tFld("nope", i32)), i32)
      val body = tBody(Nil, bad)
      val errs = mustFailExtract(body, None, childScope(), slot("Child", "count"))
      assert(errs.mkString.contains("unknown field"))
    }

    "report no reads for a pure literal body" in {
      val body = tBody(Nil, tLit(1))
      val s    = mustExtract(body, None, childScope(), slot("Child", "count"))
      assert(s.leafReads.isEmpty)
      assert(s.shapeReads.isEmpty)
      assert(s.prevReads.isEmpty)
      assert(s.writes == Set(leaf("Child", "count")))
    }

    "ignore assigns to let-locals instead of erroring" in {
      val x     = path("x")
      val body  = mustType(
        List(letS("x", "i32", intLit(1)), eff(BodyExpr.Assign(x, intLit(2)))),
        x,
        childScope(),
        i32,
      )
      val s = mustExtract(body, None, childScope(), slot("Child", "count"))
      assert(s.writes == Set(leaf("Child", "count")))
      assert(s.leafReads.isEmpty)
    }

    "never report prev reads" in {
      val body = mustType(Nil, path("self", fld("data")), childScope(), str)
      val s    = mustExtract(body, None, childScope(), slot("Child", "data"))
      assert(s.prevReads.isEmpty)
    }
  }
}
