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
  private val basketDto = Typedef.Dto(
    uid("Basket"),
    List(field("a", i32), field("b", i32)),
    Nil,
  )
  private val schema = FnSchema(Map("Child" -> childDto, "Basket" -> basketDto), Map.empty)

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
}
