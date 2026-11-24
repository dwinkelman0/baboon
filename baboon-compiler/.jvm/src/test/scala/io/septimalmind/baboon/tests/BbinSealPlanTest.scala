package io.septimalmind.baboon.tests

import io.septimalmind.baboon.bbin.model.*
import io.septimalmind.baboon.bbin.typer.*
import io.septimalmind.baboon.parser.model.{RawTypeName, RawTypeRef}
import org.scalatest.wordspec.AnyWordSpec

/** Phase 5 checks for `.bbin` seal planning ([[BbinSealPlan]]): seal-site
  * discovery (sealed lets + `.seal()` chains), static required-slot
  * errors, the `lst` index heuristic, and provenance gating.
 */
final class BbinSealPlanTest extends AnyWordSpec {

  private def ref(n: String): RawTypeRef.Simple =
    RawTypeRef.Simple(RawTypeName(n), Nil)

  private def plain(n: String): BbinType = BbinType.Plain(ref(n))

  private def partial(n: String): BbinType = BbinType.Partial(ref(n))

  private def setOf(n: String): BbinType = BbinType.SetOf(ref(n))

  private def field(name: String): BbinPathSeg.Field = BbinPathSeg.Field(name)

  private def emptySet: BbinExpr = BbinExpr.SetLit(BbinSet(Nil))

  private def sealedLet(name: String, tpe: BbinType, expr: BbinExpr = emptySet): BbinStmt.LetStmt =
    BbinStmt.LetStmt(BbinLet(name, tpe, expr))

  private def assign(root: String, rest: BbinPathSeg*): BbinStmt.AssignStmt =
    BbinStmt.AssignStmt(
      BbinAssign(BbinPath(field(root) :: rest.toList), BbinAssignOp.Set, BbinExpr.IntLit(1L, "1")),
    )

  private def siteFor(sites: List[BbinSealSite], target: String): BbinSealSite =
    sites.find(_.target == target).getOrElse(fail(s"no seal site for '$target' in $sites"))

  private def loaderOf(row: BbinType): BbinExpr.Loader =
    BbinExpr.Loader(BbinLoaderCall(BbinLoaderKind.Csv, row, "ops.csv", Nil))

  "BbinSealPlan.plan" should {
    "accept a sealed let with all required slots filled statically" in {
      val lets  = List("x" -> plain("SoC"))
      val stmts = List(sealedLet("x", plain("SoC")), assign("x", field("arch")), assign("x", field("bwPeak")))
      val (sites, errs) =
        BbinSealPlan.plan(lets, stmts, Map("SoC" -> Set("arch", "bwPeak")), Set.empty)
      assert(errs.isEmpty, s"expected no global errors, got: $errs")
      val site = siteFor(sites, "x")
      assert(site.staticErrors.isEmpty, s"expected no static errors, got: ${site.staticErrors}")
      assert(
        site.runtimeChecks.contains(BbinRuntimeCheck.CheckDynamicCompleteness("x")),
        s"expected completeness check, got: ${site.runtimeChecks}",
      )
      assert(
        site.runtimeChecks.contains(BbinRuntimeCheck.CheckPerElementErrors("x")),
        s"expected per-element check, got: ${site.runtimeChecks}",
      )
    }

    "report a static error for a missing required slot" in {
      val lets  = List("x" -> plain("SoC"))
      val stmts = List(sealedLet("x", plain("SoC")), assign("x", field("arch")))
      val (sites, _) =
        BbinSealPlan.plan(lets, stmts, Map("SoC" -> Set("arch", "bwPeak")), Set.empty)
      val site = siteFor(sites, "x")
      assert(site.staticErrors.size == 1, s"expected one static error, got: ${site.staticErrors}")
      assert(
        site.staticErrors.head.contains("bwPeak"),
        s"expected the missing slot in the error, got: ${site.staticErrors}",
      )
    }

    "give dynamic/loaded rows completeness and per-element runtime checks" in {
      val lets  = List("ops" -> setOf("SoC"))
      val stmts = List(sealedLet("ops", setOf("SoC"), loaderOf(setOf("SoC"))))
      val (sites, _) =
        BbinSealPlan.plan(lets, stmts, Map("SoC" -> Set("arch", "bwPeak")), Set.empty)
      val site = siteFor(sites, "ops")
      assert(
        site.runtimeChecks.contains(BbinRuntimeCheck.CheckDynamicCompleteness("ops")),
        s"expected completeness check, got: ${site.runtimeChecks}",
      )
      assert(
        site.runtimeChecks.contains(BbinRuntimeCheck.CheckPerElementErrors("ops")),
        s"expected per-element check, got: ${site.runtimeChecks}",
      )
    }

    "emit a contiguity check when an indexed path is used" in {
      val lets = List("rows" -> setOf("SoC"))
      val stmts = List(
        sealedLet("rows", setOf("SoC")),
        assign("rows", BbinPathSeg.Index(0), field("v")),
        assign("plain", field("a")),
      )
      val plainLets = lets :+ ("plain" -> plain("SoC"))
      val fullStmts = stmts :+ sealedLet("plain", plain("SoC"))
      val (sites, _) =
        BbinSealPlan.plan(plainLets, fullStmts, Map("SoC" -> Set("v")), Set.empty)
      assert(
        siteFor(sites, "rows").runtimeChecks.contains(BbinRuntimeCheck.CheckLstContiguity("rows")),
        s"expected contiguity check, got: ${siteFor(sites, "rows").runtimeChecks}",
      )
      assert(
        !siteFor(sites, "plain").runtimeChecks.contains(BbinRuntimeCheck.CheckLstContiguity("plain")),
        s"expected no contiguity check, got: ${siteFor(sites, "plain").runtimeChecks}",
      )
    }

    "report a static error for a provenance-required type with empty provenance" in {
      val lets = List("x" -> plain("SoC"), "w" -> plain("SoC"))
      val chain = BbinExpr.Chain(BbinExpr.Ident("x"), List(BbinChainOp.WithProvenance(Nil)))
      val stmts = List(sealedLet("x", plain("SoC")), sealedLet("w", plain("SoC"), chain))
      val (sites, _) =
        BbinSealPlan.plan(lets, stmts, Map.empty, Set("SoC"))
      val site = siteFor(sites, "x")
      assert(
        site.staticErrors.exists(_.contains("provenance")),
        s"expected a provenance static error, got: ${site.staticErrors}",
      )
      assert(
        !site.runtimeChecks.contains(BbinRuntimeCheck.CheckProvenanceComplete("x")),
        s"empty provenance must not get a provenance runtime check, got: ${site.runtimeChecks}",
      )
    }

    "emit a provenance runtime check when provenance is present" in {
      val lets = List("x" -> plain("SoC"), "w" -> plain("SoC"))
      val prov = List(BbinField("src", BbinFieldValue.Str("csv")))
      val chain = BbinExpr.Chain(BbinExpr.Ident("x"), List(BbinChainOp.WithProvenance(prov)))
      val stmts = List(
        sealedLet("x", plain("SoC")),
        sealedLet("w", plain("SoC"), chain),
        assign("x", field("arch")),
      )
      val (sites, _) =
        BbinSealPlan.plan(lets, stmts, Map("SoC" -> Set("arch")), Set("SoC"))
      val site = siteFor(sites, "x")
      assert(site.staticErrors.isEmpty, s"expected no static errors, got: ${site.staticErrors}")
      assert(
        site.runtimeChecks.contains(BbinRuntimeCheck.CheckProvenanceComplete("x")),
        s"expected provenance check, got: ${site.runtimeChecks}",
      )
    }

    "require no provenance check for a non-required type" in {
      val lets = List("x" -> plain("Other"), "w" -> plain("Other"))
      val prov = List(BbinField("src", BbinFieldValue.Str("csv")))
      val chain = BbinExpr.Chain(BbinExpr.Ident("x"), List(BbinChainOp.WithProvenance(prov)))
      val stmts = List(
        sealedLet("x", plain("Other")),
        sealedLet("w", plain("Other"), chain),
        assign("x", field("arch")),
      )
      val (sites, _) =
        BbinSealPlan.plan(lets, stmts, Map("Other" -> Set("arch")), Set("SoC"))
      val site = siteFor(sites, "x")
      assert(site.staticErrors.isEmpty, s"expected no static errors, got: ${site.staticErrors}")
      assert(
        !site.runtimeChecks.exists(_.isInstanceOf[BbinRuntimeCheck.CheckProvenanceComplete]),
        s"expected no provenance check, got: ${site.runtimeChecks}",
      )
    }

    "plan a site for a `.seal()` chain call on a partial base" in {
      val lets = List("p" -> partial("SoC"), "q" -> partial("SoC"))
      val seal = BbinExpr.Chain(BbinExpr.Ident("p"), List(BbinChainOp.Seal))
      val stmts = List(
        sealedLet("p", partial("SoC")),
        BbinStmt.AssignStmt(BbinAssign(BbinPath(List(field("q"))), BbinAssignOp.Set, seal)),
      )
      val (sites, errs) =
        BbinSealPlan.plan(lets, stmts, Map.empty, Set.empty)
      assert(errs.isEmpty, s"expected no global errors, got: $errs")
      val site = siteFor(sites, "p")
      assert(
        site.runtimeChecks.contains(BbinRuntimeCheck.CheckDynamicCompleteness("p")),
        s"expected completeness check, got: ${site.runtimeChecks}",
      )
      assert(
        site.runtimeChecks.contains(BbinRuntimeCheck.CheckPerElementErrors("p")),
        s"expected per-element check, got: ${site.runtimeChecks}",
      )
    }

    "cover a `.seal()` RHS by its sealed let instead of a second site" in {
      val lets  = List("x" -> plain("SoC"))
      val seal  = BbinExpr.Chain(BbinExpr.Ident("p"), List(BbinChainOp.Seal))
      val stmts = List(sealedLet("x", plain("SoC"), seal))
      val (sites, _) =
        BbinSealPlan.plan(lets, stmts, Map.empty, Set.empty)
      assert(sites.map(_.target) == List("x"), s"expected only the let site, got: $sites")
    }
  }
}
