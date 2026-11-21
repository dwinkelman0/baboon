package io.septimalmind.baboon.tests

import io.septimalmind.baboon.parser.model._
import io.septimalmind.baboon.typer.FnPipeline
import io.septimalmind.baboon.typer.FnPipeline.{FnSiteInputs, ValidatedFnProgram}
import io.septimalmind.baboon.typer.FnPipeline
import io.septimalmind.baboon.typer.model._
import io.septimalmind.baboon.typer.model.FnTypes.{ctor, scalar}
import izumi.fundamentals.collections.nonempty.NEList
import org.scalatest.wordspec.AnyWordSpec

/** End-to-end tests for the P7 fn pipeline: P1→P6 assembly plus the final
  * backend-ready invariants, on tiny hand-built schemas. Translator gates are
  * untouched: everything here exercises the standalone pipeline only.
  */
final class FnPipelineTest extends AnyWordSpec {

  private val i32 = scalar("i32")
  private val str = scalar("str")

  private def uid(name: String): TypeId.User =
    TypeId.User(Pkg(NEList.unsafeFrom(List("t"))), Owner.Toplevel, TypeName(name))
  private def uref(name: String): TypeRef = TypeRef.Scalar(uid(name))
  private def field(name: String, tpe: TypeRef): Field = Field(FieldName(name), tpe, None)

  private val childDto = Typedef.Dto(
    uid("Child"),
    List(
      field("data", str),
      field("count", i32),
      field("total", i32),
      field("grand", i32),
      field("v", i32),
    ),
    Nil,
  )
  private val itemDto = Typedef.Dto(
    uid("Item"),
    List(field("label", str), field("qty", i32), field("price", i32)),
    Nil,
  )
  private val basketDto = Typedef.Dto(
    uid("Basket"),
    List(field("a", i32), field("b", i32), field("items", ctor(TypeId.Builtins.lst, List(uref("Item"))))),
    Nil,
  )
  private val cartDto = Typedef.Dto(
    uid("Cart"),
    List(field("c", i32), field("items", ctor(TypeId.Builtins.lst, List(uref("Item"))))),
    Nil,
  )
  private val shopDto = Typedef.Dto(
    uid("Shop"),
    List(field("basket", uref("Basket")), field("cart", uref("Cart"))),
    Nil,
  )
  // Diamond: Item is shared between Basket.items and Cart.items; Shop is the
  // common root containing both parents. Basket alone dominates nothing
  // below the diamond (every Item is also reachable via Cart).
  private val schema = FnSchema(Map("Child" -> childDto, "Basket" -> basketDto, "Item" -> itemDto, "Cart" -> cartDto, "Shop" -> shopDto), Map.empty)

  // -- small AST builders -----------------------------------------------------

  private def rt(name: String): RawTypeRef = RawTypeRef.Simple(RawTypeName(name), Nil)
  private def intLit(v: Long): BodyExpr = BodyExpr.Lit(BodyLiteral.IntLit(v))
  private def path(root: String, segs: BodyExpr.Segment*): BodyExpr = BodyExpr.Path(root, segs.toList)
  private def fld(name: String): BodyExpr.Segment = BodyExpr.Segment.Field(name)
  private def letS(name: String, t: String, v: BodyExpr): BodyStmt = BodyStmt.Let(name, Some(rt(t)), v)
  private def letU(name: String, v: BodyExpr): BodyStmt = BodyStmt.Let(name, None, v)

  private def rawBody(stmts: List[BodyStmt], result: BodyExpr): RawBody =
    RawBody(stmts.collect { case BodyStmt.Let(n, _, v) => (n, v) }, result, stmts)

  private def fn0(selfTpe: String, stmts: List[BodyStmt], result: BodyExpr): RawFn =
    RawFn(Nil, List(RawFnParam("self", rt(selfTpe))), rawBody(stmts, result))

  private def fnSelfPlus(selfTpe: String, extra: String, extraTpe: String, result: BodyExpr): RawFn =
    RawFn(Nil, List(RawFnParam("self", rt(selfTpe)), RawFnParam(extra, rt(extraTpe))), rawBody(Nil, result))

  private def fnCap(cap: RawCapture, selfTpe: String, stmts: List[BodyStmt], result: BodyExpr): RawFn =
    RawFn(List(cap), List(RawFnParam("self", rt(selfTpe))), rawBody(stmts, result))

  private def cap1(name: String, root: String, segs: RawLocatorSeg*): RawCapture =
    RawCapture(name, rt(root), segs.toList.map(RawCaptureStep.Single(_)))

  private def sp(owner: String, segs: String*): SchemaPath =
    SchemaPath.resolve(uid(owner), segs.toList, schema) match {
      case Right(p)  => p
      case Left(err) => fail(s"bad expected path $owner.${segs.mkString(".")}: $err")
    }

  private def in(
    owner: String,
    f: String,
    role: FnRole,
    fn: RawFn,
    expected: String = "i32",
    sources: Set[SchemaPath] = Set.empty,
  ): FnSiteInputs = FnSiteInputs(fn, owner, f, role, rt(expected), sources, schema)

  private def inN(
    owner: String,
    f: String,
    role: FnRole,
    fn: RawFn,
    nodeCap: RawCapture,
    expected: String = "i32",
    sources: Set[SchemaPath] = Set.empty,
  ): FnSiteInputs = FnSiteInputs(fn, owner, f, role, rt(expected), sources, schema, nodeCapture = Some(nodeCap))

  private def mustAssemble(sites: List[FnSiteInputs]): ValidatedFnProgram =
    FnPipeline.assemble(sites) match {
      case Right(p)  => p
      case Left(err) => fail(s"expected assembly success, got: ${err.mkString("; ")}")
    }

  private def mustFail(sites: List[FnSiteInputs]): List[String] =
    FnPipeline.assemble(sites) match {
      case Left(err) => err
      case Right(p)  => fail(s"expected assembly failure, got $p")
    }

  private def renders(p: ValidatedFnProgram): List[String] =
    p.emissionOrder.nodes.map(_.render)

  // -- success paths ----------------------------------------------------------

  "P7 pipeline" should {

    "assemble a default+normalize+computed program end to end" in {
      val sites = List(
        in("Child", "count", FnRole.Default, fn0("Child", Nil, intLit(0))),
        in("Child", "total", FnRole.Normalize, fn0("Child", Nil, path("self", fld("count")))),
        in("Child", "grand", FnRole.Computed, fn0("Child", Nil, path("self", fld("total")))),
      )
      val p = mustAssemble(sites)
      assert(p.fns.size == 3)
      assert(p.gated.map(_.pass).sorted == List(1, 1, 2))
      val r = renders(p)
      assert(r.contains("Child.count(default)"), r.mkString(", "))
      assert(r.contains("Child.total(normalize)"), r.mkString(", "))
      assert(r.contains("Child.grand(computed)"), r.mkString(", "))
      // Pass timeline: computed runs after defaults/normalizes.
      val idx = (s: String) => r.indexWhere(_.contains(s))
      assert(idx("grand(computed)") > idx("count(default)"))
      assert(idx("grand(computed)") > idx("total(normalize)"))
      // Read-after edge: normalize reads count, which default writes.
      assert(idx("total(normalize)") > idx("count(default)"))
    }

    "assemble a migrate fn with declared sources" in {
      val sites = List(
        in(
          "Child",
          "v",
          FnRole.Migrate,
          fn0("Child", Nil, path("self", fld("count"))),
          sources = Set(sp("Child", "count")),
        ),
      )
      val p = mustAssemble(sites)
      assert(p.fns.size == 1)
      assert(p.gated.head.pass == 0)
      assert(p.gated.head.readTargets == Set(sp("Child", "count")))
    }

    "succeed on empty input (orphan check)" in {
      val p = mustAssemble(Nil)
      assert(p.fns.isEmpty)
      assert(p.gated.isEmpty)
      assert(p.emissionOrder.nodes.isEmpty)
    }

    "keep prevReads empty on success (deferred-prev invariant)" in {
      val sites = List(
        in("Child", "count", FnRole.Default, fn0("Child", Nil, path("self", fld("total")))),
        in("Child", "grand", FnRole.Computed, fn0("Child", Nil, path("self", fld("total")))),
      )
      val p = mustAssemble(sites)
      p.fns.foreach { case (site, _, sens) =>
        assert(sens.prevReads.isEmpty, s"${site.id} has prev reads: ${sens.prevReads}")
      }
    }

    "assemble a parameterized computed fn with local-only extra params" in {
      val body = BodyExpr.BinOp("+", path("self", fld("count")), path("t"))
      val sites = List(
        in("Child", "grand", FnRole.Computed, fnSelfPlus("Child", "t", "i32", body)),
      )
      val p = mustAssemble(sites)
      assert(p.fns.size == 1)
      val (_, _, sens) = p.fns.head
      assert(sens.leafReads == Set(sp("Child", "count")), s"extra param leaked into leafReads: ${sens.leafReads}")
      assert(sens.prevReads.isEmpty)
    }

    "cover every fn node in the emission order" in {
      val sites = List(
        in("Child", "count", FnRole.Default, fn0("Child", Nil, intLit(0))),
        in("Child", "total", FnRole.Normalize, fn0("Child", Nil, path("self", fld("count")))),
      )
      val p = mustAssemble(sites)
      val ordered = renders(p).toSet
      p.fns.foreach { case (site, _: FnPipeline.TypedFn, _) =>
        val id = site.id
        assert(
          ordered.exists(_.contains(s"${id.field}")),
          s"emission order misses ${id.owner}.${id.field}: ${ordered.mkString(", ")}",
        )
      }
    }

    "be deterministic under shuffled inputs" in {
      val sites = List(
        in("Child", "count", FnRole.Default, fn0("Child", Nil, intLit(0))),
        in("Child", "total", FnRole.Normalize, fn0("Child", Nil, path("self", fld("count")))),
        in("Child", "grand", FnRole.Computed, fn0("Child", Nil, path("self", fld("total")))),
      )
      val a = renders(mustAssemble(sites))
      val b = renders(mustAssemble(sites.reverse))
      assert(a == b, s"order differs under shuffle:\n$a\n$b")
    }
  }

  // -- per-pass short-circuiting ----------------------------------------------

  "P7 error routing" should {

    "fail a missing let annotation at P3, not as a cycle" in {
      val bad = fn0("Child", List(letU("x", intLit(1))), path("x"))
      val errs = mustFail(List(in("Child", "count", FnRole.Default, bad)))
      assert(errs.exists(_.contains("explicit type annotation")), errs.mkString("; "))
      assert(!errs.exists(_.contains("cycle")), errs.mkString("; "))
    }

    "fail an unknown capture field at P2, not as a cycle" in {
      val bad = fnCap(cap1("p", "Child", RawLocatorSeg.Field("nope")), "Child", Nil, path("p"))
      val errs = mustFail(List(in("Child", "count", FnRole.Default, bad)))
      assert(errs.exists(_.contains("unknown field")), errs.mkString("; "))
      assert(!errs.exists(_.contains("cycle")), errs.mkString("; "))
    }

    "fail an unknown expected type before gating" in {
      val fn = fn0("Child", Nil, intLit(1))
      val errs = mustFail(List(in("Child", "count", FnRole.Default, fn, expected = "Nope")))
      assert(errs.exists(_.contains("unknown type")), errs.mkString("; "))
      assert(!errs.exists(_.contains("cycle")), errs.mkString("; "))
    }

    "fail a migrate fn reading outside its sources at P5, not as a cycle" in {
      val fn = fn0("Child", Nil, path("self", fld("total")))
      val errs = mustFail(List(in("Child", "v", FnRole.Migrate, fn, sources = Set(sp("Child", "data")))))
      assert(errs.exists(e => e.contains("migrate") && e.contains("undeclared")), errs.mkString("; "))
      assert(!errs.exists(_.contains("cycle")), errs.mkString("; "))
    }

    "fail a self-read as a P6 cycle" in {
      val fn = fn0("Child", Nil, path("self", fld("count")))
      val errs = mustFail(List(in("Child", "count", FnRole.Default, fn)))
      assert(errs.exists(_.contains("cycle")), errs.mkString("; "))
    }

    "fail parent-reads-fn overlap as a P6 cycle" in {
      val a = in("Basket", "a", FnRole.Default, fn0("Basket", Nil, path("self", fld("b"))))
      val b = in("Basket", "b", FnRole.Default, fn0("Basket", Nil, path("self", fld("a"))))
      val errs = mustFail(List(a, b))
      assert(errs.exists(_.contains("cycle")), errs.mkString("; "))
    }
  }

  "P7 Self roles" should {

    "resolve computed self: Self to the owner container with sibling access" in {
      val fn = RawFn(Nil, List(RawFnParam("self", rt("Self"))), rawBody(Nil, BodyExpr.BinOp("+", path("self", fld("count")), path("self", fld("total")))))
      val p = mustAssemble(List(in("Child", "grand", FnRole.Computed, fn)))
      val (_, typed, _) = p.fns.head
      assert(typed.params == List("self" -> uref("Child")))
      assert(typed.body.tpe == i32)
    }

    "resolve normalize self: Self to the field value type" in {
      val fn = RawFn(Nil, List(RawFnParam("self", rt("Self"))), rawBody(Nil, BodyExpr.Call(path("self"), "trim", Nil)))
      val p = mustAssemble(List(in("Child", "data", FnRole.Normalize, fn, expected = "str")))
      val (_, typed, _) = p.fns.head
      assert(typed.params == List("self" -> str))
      assert(typed.body.tpe == str)
    }

    "resolve default self: Self to the field value type" in {
      val fn = RawFn(Nil, List(RawFnParam("self", rt("Self"))), rawBody(Nil, path("self")))
      val p = mustAssemble(List(in("Child", "count", FnRole.Default, fn)))
      val (_, typed, _) = p.fns.head
      assert(typed.params == List("self" -> i32))
    }

    "reject computed self annotated as the field scalar" in {
      val errs = mustFail(List(in("Child", "grand", FnRole.Computed, fn0("i32", Nil, intLit(0)))))
      assert(errs.exists(_.contains("computed `self`")), errs.mkString("; "))
      assert(!errs.exists(_.contains("cycle")), errs.mkString("; "))
    }

    "situate a capture path ending at the container" in {
      val fn = fnCap(cap1("s", "Child"), "Child", Nil, path("s", fld("count")))
      val errs = mustFail(List(in("Child", "grand", FnRole.Computed, fn)))
      assert(errs.exists(_.contains("redundant")), errs.mkString("; "))
    }
  }

  "P7 capture ancestry" should {

    "reject a self capture (root is the owner) as redundant" in {
      val fn = fnCap(cap1("s", "Child"), "Child", Nil, path("s", fld("count")))
      val errs = mustFail(List(in("Child", "grand", FnRole.Computed, fn)))
      assert(errs.exists(_.contains("redundant")), errs.mkString("; "))
    }

    "reject a self capture on shared Item as redundant" in {
      val fn = fnCap(cap1("s", "Item"), "Item", Nil, path("s", fld("price")))
      val errs = mustFail(List(in("Item", "qty", FnRole.Computed, fn)))
      assert(errs.exists(_.contains("redundant")), errs.mkString("; "))
    }

    "pass a common-root capture (Shop dominates shared Item)" in {
      val fn = fnCap(cap1("s", "Shop"), "Item", Nil, path("s", fld("basket"), fld("a")))
      val p = mustAssemble(List(in("Item", "qty", FnRole.Computed, fn)))
      assert(p.fns.size == 1)
    }

    "reject a non-dominant ancestor capture (Basket does not dominate shared Item)" in {
      val fn = fnCap(cap1("b", "Basket"), "Item", Nil, path("b", fld("a")))
      val errs = mustFail(List(in("Item", "qty", FnRole.Computed, fn)))
      assert(errs.exists(_.contains("not an ancestor")), errs.mkString("; "))
      assert(errs.exists(_.contains("not dominant")), errs.mkString("; "))
    }

    "reject a non-dominant ancestor capture (Cart does not dominate shared Item)" in {
      val fn = fnCap(cap1("c", "Cart"), "Item", Nil, path("c", fld("c")))
      val errs = mustFail(List(in("Item", "qty", FnRole.Computed, fn)))
      assert(errs.exists(_.contains("not an ancestor")), errs.mkString("; "))
      assert(errs.exists(_.contains("not dominant")), errs.mkString("; "))
    }

    "reject a sibling capture (Child fn capturing Basket)" in {
      val fn = fnCap(cap1("b", "Basket"), "Child", Nil, path("b", fld("a")))
      val errs = mustFail(List(in("Child", "grand", FnRole.Computed, fn)))
      assert(errs.exists(_.contains("not an ancestor")), errs.mkString("; "))
    }

    "reject an unrelated capture (Item fn capturing Child)" in {
      val fn = fnCap(cap1("c", "Child"), "Item", Nil, path("c", fld("count")))
      val errs = mustFail(List(in("Item", "qty", FnRole.Computed, fn)))
      assert(errs.exists(_.contains("not an ancestor")), errs.mkString("; "))
    }

    "reject a non-self capture on a root (Child fn capturing Basket)" in {
      // Child has no parents in the parentMap, so it is a root: it may be
      // instantiated without a parent, and any non-self capture would dangle.
      val fn = fnCap(cap1("b", "Basket"), "Child", Nil, path("b", fld("a")))
      val errs = mustFail(List(in("Child", "grand", FnRole.Computed, fn)))
      assert(errs.exists(_.contains("may be instantiated without a parent")), errs.mkString("; "))
    }

    "reject a dominating capture on an explicit gcRoot (Item marked root capturing Shop)" in {
      // Shop statically dominates Item, but a gcRoot Item can exist
      // parentless at runtime, so the capture would dangle.
      val fn = fnCap(cap1("s", "Shop"), "Item", Nil, path("s", fld("basket"), fld("a")))
      val site = in("Item", "qty", FnRole.Computed, fn).copy(ownerIsRoot = true)
      val errs = mustFail(List(site))
      assert(errs.exists(_.contains("may be instantiated without a parent")), errs.mkString("; "))
    }

    "reject a self-capture on an explicit gcRoot (Item capturing Item) as redundant" in {
      val fn = fnCap(cap1("s", "Item"), "Item", Nil, path("s", fld("price")))
      val site = in("Item", "qty", FnRole.Computed, fn).copy(ownerIsRoot = true)
      val errs = mustFail(List(site))
      assert(errs.exists(_.contains("redundant")), errs.mkString("; "))
    }

    "pass a root fn with no capture (explicit gcRoot, parentless Shop)" in {
      val p1 = mustAssemble(List(in("Child", "grand", FnRole.Computed, fn0("Child", Nil, path("self", fld("total"))))))
      assert(p1.fns.size == 1)
      val site = in("Item", "qty", FnRole.Computed, fn0("Item", Nil, path("self", fld("price")))).copy(ownerIsRoot = true)
      val p2 = mustAssemble(List(site))
      assert(p2.fns.size == 1)
    }
  }

  "P7 node capture" should {

    "share one node capture across every fn of the node" in {
      val nc = cap1("s", "Shop")
      val a  = inN("Item", "qty", FnRole.Computed, fn0("Item", Nil, path("s", fld("basket"), fld("a"))), nc)
      val b  = inN("Item", "price", FnRole.Computed, fn0("Item", Nil, path("s", fld("cart"), fld("c"))), nc)
      val p  = mustAssemble(List(a, b))
      assert(p.fns.size == 2)
      p.fns.foreach { case (_, typed, _) =>
        assert(typed.capture.map(_.endpoint) == Some(uref("Shop")))
      }
    }

    "reject a per-fn capture when the node already has @capture" in {
      val nc = cap1("s", "Shop")
      val fn = fnCap(cap1("b", "Basket"), "Item", Nil, path("b", fld("a")))
      val errs = mustFail(List(inN("Item", "qty", FnRole.Computed, fn, nc)))
      assert(errs.exists(_.contains("only one source")), errs.mkString("; "))
    }

    "reject a node self-capture as redundant" in {
      val nc = cap1("s", "Item")
      val errs = mustFail(List(inN("Item", "qty", FnRole.Computed, fn0("Item", Nil, path("s", fld("price"))), nc)))
      assert(errs.exists(_.contains("redundant")), errs.mkString("; "))
    }

    "reject a node capture on a root node" in {
      val nc = cap1("b", "Basket")
      val errs = mustFail(List(inN("Child", "grand", FnRole.Computed, fn0("Child", Nil, path("b", fld("a"))), nc)))
      assert(errs.exists(_.contains("may be instantiated without a parent")), errs.mkString("; "))
    }

    "expose node-capture key vars as read-only locals in every fn" in {
      val nc = RawCapture(
        "s",
        rt("Shop"),
        List(
          RawCaptureStep.Single(RawLocatorSeg.Field("basket")),
          RawCaptureStep.Single(RawLocatorSeg.Field("items")),
          RawCaptureStep.Single(RawLocatorSeg.Key("k")),
        ),
      )
      val p = mustAssemble(List(inN("Item", "qty", FnRole.Computed, fn0("Item", Nil, path("k")), nc)))
      assert(p.fns.size == 1)
      val bad = RawFn(
        Nil,
        List(RawFnParam("self", rt("Item"))),
        rawBody(List(BodyStmt.Effect(BodyExpr.Assign(path("k"), intLit(1)))), intLit(0)),
      )
      val errs = mustFail(List(inN("Item", "qty", FnRole.Computed, bad, nc)))
      assert(errs.exists(_.contains("cannot assign to capture key var")), errs.mkString("; "))
    }

    "reject a node capture on a direct member of a capturing parent" in {
      val parentNc = cap1("s", "Shop")
      val childNc = cap1("s", "Shop")
      val parent = inN("Basket", "a", FnRole.Computed, fn0("Basket", Nil, path("s", fld("basket"), fld("a"))), parentNc)
      val child = inN("Item", "qty", FnRole.Computed, fn0("Item", Nil, path("s", fld("basket"), fld("a"))), childNc)
      val errs = mustFail(List(parent, child))
      assert(errs.exists(_.contains("nested capture not allowed")), errs.mkString("; "))
      assert(errs.exists(e => e.contains("`Item`") && e.contains("`Basket`")), errs.mkString("; "))
    }

    "reject a per-fn capture on a direct member of a capturing parent" in {
      val parentNc = cap1("s", "Shop")
      val parent = inN("Basket", "a", FnRole.Computed, fn0("Basket", Nil, path("s", fld("basket"), fld("a"))), parentNc)
      val childFn = fnCap(cap1("s", "Shop"), "Item", Nil, path("s", fld("basket"), fld("a")))
      val errs = mustFail(List(parent, in("Item", "qty", FnRole.Computed, childFn)))
      assert(errs.exists(_.contains("nested capture not allowed")), errs.mkString("; "))
    }

    "pass a direct member with no capture under a capturing parent" in {
      val parentNc = cap1("s", "Shop")
      val parent = inN("Basket", "a", FnRole.Computed, fn0("Basket", Nil, path("s", fld("basket"), fld("a"))), parentNc)
      val child = in("Item", "qty", FnRole.Computed, fn0("Item", Nil, path("self", fld("price"))))
      val p = mustAssemble(List(parent, child))
      assert(p.fns.size == 2)
    }

    "allow a grandchild capture via a non-capturing intermediary" in {
      val rTop = Typedef.Dto(uid("RTop"), List(field("a", uref("ATop"))), Nil)
      val aTop = Typedef.Dto(uid("ATop"), List(field("m", uref("Mid"))), Nil)
      val mid = Typedef.Dto(uid("Mid"), List(field("b", uref("Bot")), field("w", i32)), Nil)
      val bot = Typedef.Dto(uid("Bot"), List(field("v", i32)), Nil)
      val chain = FnSchema(Map("RTop" -> rTop, "ATop" -> aTop, "Mid" -> mid, "Bot" -> bot), Map.empty)
      def chainInN(owner: String, f: String, fn: RawFn, nc: RawCapture): FnSiteInputs =
        FnSiteInputs(fn, owner, f, FnRole.Computed, rt("i32"), Set.empty, chain, nodeCapture = Some(nc))
      def chainIn(owner: String, f: String, fn: RawFn): FnSiteInputs =
        FnSiteInputs(fn, owner, f, FnRole.Computed, rt("i32"), Set.empty, chain)
      val topNc = cap1("s", "RTop")
      val botNc = cap1("s", "RTop")
      val top = chainInN("ATop", "m", fn0("ATop", Nil, path("s", fld("a"), fld("m"), fld("b"), fld("v"))), topNc)
      val between = chainIn("Mid", "w", fn0("Mid", Nil, path("self", fld("b"), fld("v"))))
      val leaf = chainInN("Bot", "v", fn0("Bot", Nil, path("s", fld("a"), fld("m"), fld("b"), fld("v"))), botNc)
      val p = mustAssemble(List(top, between, leaf))
      assert(p.fns.size == 3)
    }
  }
}
