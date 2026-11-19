package io.septimalmind.baboon.tests

import io.septimalmind.baboon.parser.model._
import io.septimalmind.baboon.typer.{FnDepGraphBuilder, FnReadGate, FnSensitivityExtractor}
import io.septimalmind.baboon.typer.FnDepGraphBuilder.{DepKind, DepNode, FnNodeId}
import io.septimalmind.baboon.typer.FnSensitivityExtractor.{SchemaPath, Sensitivity}
import io.septimalmind.baboon.typer.model._
import io.septimalmind.baboon.typer.model.FnTypes.{ctor, scalar}
import izumi.fundamentals.collections.nonempty.NEList
import org.scalatest.wordspec.AnyWordSpec

/** P5 read-gate + P6 dep-graph/ordering tests on a tiny hand-built schema.
  * Translator gates are untouched: everything here exercises the standalone
  * `FnReadGate` / `FnDepGraphBuilder` passes only.
  */
final class FnOrderingTest extends AnyWordSpec {

  private val i32 = scalar("i32")
  private val str = scalar("str")

  private def uid(name: String): TypeId.User =
    TypeId.User(Pkg(NEList.unsafeFrom(List("t"))), Owner.Toplevel, TypeName(name))
  private def uref(name: String): TypeRef = TypeRef.Scalar(uid(name))
  private def field(name: String, tpe: TypeRef): Field = Field(FieldName(name), tpe, None)

  private val itemDto = Typedef.Dto(
    uid("Item"),
    List(field("qty", i32), field("label", str)),
    Nil,
  )
  private val basketDto = Typedef.Dto(
    uid("Basket"),
    List(
      field("items", ctor(TypeId.Builtins.lst, List(uref("Item")))),
      field("total", i32),
      field("vat", i32),
      field("grand", i32),
      field("note", str),
      field("rate", i32),
      field("v", i32),
      field("w", i32),
      field("a", i32),
      field("b", i32),
      field("c", i32),
      field("d", i32),
    ),
    Nil,
  )
  private val schema = FnSchema(Map("Item" -> itemDto, "Basket" -> basketDto), Map.empty)

  // -- builders ---------------------------------------------------------------

  private def sp(owner: String, segs: String*): SchemaPath =
    SchemaPath(uid(owner), segs.toList)
  private def slot(owner: String, f: String): SchemaPath = sp(owner, f)

  private def sens(
    reads: Set[SchemaPath] = Set.empty,
    writes: Set[SchemaPath] = Set.empty,
    shape: Set[TypeId.User] = Set.empty
  ): Sensitivity = Sensitivity(reads, shape, Set.empty, writes)

  private def rawFn: RawFn =
    RawFn(Nil, Nil, RawBody(Nil, BodyExpr.Lit(BodyLiteral.IntLit(0))))

  private def site(owner: String, f: String, role: FnRole): FnSite =
    FnSite(FnSiteId(owner, f, role), rawFn)

  private def bod: TypedBody =
    TypedBody(Nil, TypedExpr.Lit(TypedLit.IntLit(0), i32), i32)

  private def gateIn(
    owner: String,
    f: String,
    role: FnRole,
    s: Sensitivity,
    sources: Set[SchemaPath] = Set.empty
  ): FnReadGate.Input = FnReadGate.Input(site(owner, f, role), bod, s, sources)

  private def depIn(
    owner: String,
    f: String,
    role: FnRole,
    s: Sensitivity,
    captureOwner: Option[String] = None
  ): FnDepGraphBuilder.Input =
    FnDepGraphBuilder.Input(
      FnNodeId(uid(owner), f, role),
      s,
      captureOwner.map(uid),
      FnReadGate.passOf(role)
    )

  private def mustGate(ins: List[FnReadGate.Input]): List[FnReadGate.Gated] =
    FnReadGate.gate(ins, schema) match {
      case Right(g)  => g
      case Left(err) => fail(s"expected gate success, got: ${err.mkString("; ")}")
    }

  private def mustFailGate(ins: List[FnReadGate.Input]): List[String] =
    FnReadGate.gate(ins, schema) match {
      case Left(err) => err
      case Right(g)  => fail(s"expected gate failure, got $g")
    }

  private def mustBuild(ins: List[FnDepGraphBuilder.Input]): FnDepGraphBuilder.EmissionOrder =
    FnDepGraphBuilder.build(ins, schema) match {
      case Right(o)  => o
      case Left(err) => fail(s"expected order success, got: ${err.mkString("; ")}")
    }

  private def mustFailBuild(ins: List[FnDepGraphBuilder.Input]): List[String] =
    FnDepGraphBuilder.build(ins, schema) match {
      case Left(err) => err
      case Right(o)  => fail(s"expected cycle failure, got ${o.nodes.map(_.render)}")
    }

  private def fnIdx(o: FnDepGraphBuilder.EmissionOrder, owner: String, f: String, role: FnRole): Int =
    o.nodes.indexWhere {
      case DepNode.Fn(id) => id == FnNodeId(uid(owner), f, role)
      case _              => false
    }

  // -- P5 ---------------------------------------------------------------------

  "P5 read gate" should {
    "assign migrate/default/normalize/computed to passes 0/1/1/2" in {
      val ins = List(
        gateIn("Basket", "v", FnRole.Migrate, sens(reads = Set(sp("Basket", "items", "qty")), writes = Set(slot("Basket", "v"))), Set(sp("Basket", "items"))),
        gateIn("Basket", "total", FnRole.Default, sens(reads = Set(sp("Basket", "items", "qty")), writes = Set(slot("Basket", "total")))),
        gateIn("Basket", "vat", FnRole.Normalize, sens(reads = Set(slot("Basket", "total")), writes = Set(slot("Basket", "vat")))),
        gateIn("Basket", "grand", FnRole.Computed, sens(reads = Set(slot("Basket", "total")), writes = Set(slot("Basket", "grand")))),
      )
      val gated = mustGate(ins)
      assert(gated.map(_.pass) == List(0, 1, 1, 2))
      assert(gated.head.readTargets == Set(sp("Basket", "items", "qty")))
    }

    "reject a default fn reading a computed slot" in {
      val ins = List(
        gateIn("Basket", "note", FnRole.Computed, sens(writes = Set(slot("Basket", "note")))),
        gateIn("Basket", "total", FnRole.Default, sens(reads = Set(slot("Basket", "note")), writes = Set(slot("Basket", "total")))),
      )
      val errs = mustFailGate(ins)
      assert(errs.exists(e => e.contains("computed") && e.contains("Basket.total")), errs.mkString("; "))
    }

    "reject migrate reads outside the declared sources" in {
      val ins = List(
        gateIn(
          "Basket", "v", FnRole.Migrate,
          sens(reads = Set(slot("Basket", "total")), writes = Set(slot("Basket", "v"))),
          Set(sp("Basket", "items"))
        )
      )
      val errs = mustFailGate(ins)
      assert(errs.exists(e => e.contains("undeclared") && e.contains("Basket.total")), errs.mkString("; "))
    }

    "accept migrate reads within the declared sources" in {
      val ins = List(
        gateIn(
          "Basket", "v", FnRole.Migrate,
          sens(reads = Set(sp("Basket", "items", "qty")), writes = Set(slot("Basket", "v"))),
          Set(sp("Basket", "items"))
        )
      )
      assert(mustGate(ins).head.pass == 0)
    }

    "reject unknown field reads" in {
      val ins = List(
        gateIn("Basket", "total", FnRole.Default, sens(reads = Set(sp("Basket", "nope")), writes = Set(slot("Basket", "total"))))
      )
      val errs = mustFailGate(ins)
      assert(errs.exists(e => e.contains("unknown field") && e.contains("nope")), errs.mkString("; "))
    }
  }

  // -- P6 ---------------------------------------------------------------------

  "P6 dep graph" should {
    "order a child fn before a capturing parent when the child reads nothing back" in {
      val child  = depIn("Item", "qty", FnRole.Default, sens(writes = Set(slot("Item", "qty"))))
      val parent = depIn("Basket", "total", FnRole.Default, sens(reads = Set(sp("Basket", "items", "qty")), writes = Set(slot("Basket", "total"))), Some("Item"))
      val o      = mustBuild(List(parent, child))
      assert(fnIdx(o, "Item", "qty", FnRole.Default) < fnIdx(o, "Basket", "total", FnRole.Default))
    }

    "report a hard cycle with a path when the captured fn reads the parent" in {
      val child  = depIn("Item", "qty", FnRole.Default, sens(reads = Set(slot("Basket", "total")), writes = Set(slot("Item", "qty"))))
      val parent = depIn("Basket", "total", FnRole.Default, sens(reads = Set(sp("Basket", "items", "qty")), writes = Set(slot("Basket", "total"))), Some("Item"))
      val errs   = mustFailBuild(List(child, parent))
      assert(errs.exists(e => e.contains("cycle") && e.contains("Basket.total") && e.contains("Item.qty")), errs.mkString("; "))
    }

    "report a self-read as a cycle" in {
      val self = depIn("Basket", "total", FnRole.Default, sens(reads = Set(slot("Basket", "total")), writes = Set(slot("Basket", "total"))))
      val errs = mustFailBuild(List(self))
      assert(errs.exists(e => e.contains("cycle") && e.contains("Basket.total")), errs.mkString("; "))
    }

    "order migrate before default and computed last" in {
      val m = depIn("Basket", "v", FnRole.Migrate, sens(writes = Set(slot("Basket", "v"))))
      val d = depIn("Basket", "w", FnRole.Default, sens(writes = Set(slot("Basket", "w"))))
      val c = depIn("Basket", "grand", FnRole.Computed, sens(writes = Set(slot("Basket", "grand"))))
      val o = mustBuild(List(c, d, m))
      val (mi, di, ci) = (
        fnIdx(o, "Basket", "v", FnRole.Migrate),
        fnIdx(o, "Basket", "w", FnRole.Default),
        fnIdx(o, "Basket", "grand", FnRole.Computed)
      )
      assert(mi < di && di < ci, o.nodes.map(_.render).mkString(", "))
    }

    "order normalize after its descendant reads" in {
      val child  = depIn("Item", "qty", FnRole.Default, sens(writes = Set(slot("Item", "qty"))))
      val parent = depIn("Basket", "vat", FnRole.Normalize, sens(reads = Set(sp("Basket", "items", "qty")), writes = Set(slot("Basket", "vat"))), Some("Item"))
      val o      = mustBuild(List(parent, child))
      assert(fnIdx(o, "Item", "qty", FnRole.Default) < fnIdx(o, "Basket", "vat", FnRole.Normalize))
    }

    "order a diamond deterministically" in {
      val a = depIn("Basket", "a", FnRole.Default, sens(writes = Set(slot("Basket", "a"))))
      val b = depIn("Basket", "b", FnRole.Default, sens(reads = Set(slot("Basket", "a")), writes = Set(slot("Basket", "b"))))
      val c = depIn("Basket", "c", FnRole.Default, sens(reads = Set(slot("Basket", "a")), writes = Set(slot("Basket", "c"))))
      val d = depIn("Basket", "d", FnRole.Default, sens(reads = Set(slot("Basket", "b"), slot("Basket", "c")), writes = Set(slot("Basket", "d"))))
      val o = mustBuild(List(d, c, b, a))
      val idx = (f: String) => fnIdx(o, "Basket", f, FnRole.Default)
      assert(idx("a") < idx("b") && idx("a") < idx("c") && idx("b") < idx("d") && idx("c") < idx("d"))
      assert(idx("b") < idx("c"), o.nodes.map(_.render).mkString(", "))
    }

    "produce the same order for shuffled inputs" in {
      val a = depIn("Basket", "a", FnRole.Default, sens(writes = Set(slot("Basket", "a"))))
      val b = depIn("Basket", "b", FnRole.Default, sens(reads = Set(slot("Basket", "a")), writes = Set(slot("Basket", "b"))))
      val c = depIn("Basket", "c", FnRole.Default, sens(reads = Set(slot("Basket", "a")), writes = Set(slot("Basket", "c"))))
      val d = depIn("Basket", "d", FnRole.Default, sens(reads = Set(slot("Basket", "b"), slot("Basket", "c")), writes = Set(slot("Basket", "d"))))
      val o1 = mustBuild(List(a, b, c, d))
      val o2 = mustBuild(List(d, c, b, a))
      assert(o1.nodes.map(_.render) == o2.nodes.map(_.render))
    }

    "defer a star capture with a single annotated edge per captured fn" in {
      val c1 = gateIn("Item", "qty", FnRole.Default, sens(writes = Set(slot("Item", "qty"))))
      val c2 = gateIn("Item", "label", FnRole.Default, sens(writes = Set(slot("Item", "label"))))
      val p = gateIn(
        "Basket", "total", FnRole.Default,
        sens(reads = Set(sp("Basket", "items", "qty")), writes = Set(slot("Basket", "total")))
      )
      val gated = mustGate(List(c1, c2, p))
      val star = TypedCapture(
        "it",
        uref("Item"),
        "Item",
        List(TypedCaptureStep.StarStep("chain", uref("Item"), List(TypedCaptureStep.FieldStep("qty", "Item", i32)))),
        uref("Item")
      )
      val pId    = FnDepGraphBuilder.nodeIdOf(gated(2))
      val inputs = FnDepGraphBuilder.fromGated(gated, Map(pId -> Some(star)))
      val o      = mustBuild(inputs)
      assert(fnIdx(o, "Item", "qty", FnRole.Default) < fnIdx(o, "Basket", "total", FnRole.Default))
      assert(fnIdx(o, "Item", "label", FnRole.Default) < fnIdx(o, "Basket", "total", FnRole.Default))
      val defers = o.edges.filter(e =>
        e.kind == DepKind.CaptureDefer && e.after == DepNode.Fn(pId)
      )
      assert(defers.size == 2, defers.mkString(", "))
    }
  }
}
