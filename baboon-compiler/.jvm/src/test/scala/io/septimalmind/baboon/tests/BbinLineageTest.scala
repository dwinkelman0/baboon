package io.septimalmind.baboon.tests

import io.septimalmind.baboon.bbin.model.*
import io.septimalmind.baboon.bbin.typer.*
import io.septimalmind.baboon.parser.model.{RawTypeName, RawTypeRef}
import org.scalatest.wordspec.AnyWordSpec

/** Phase 4 checks for `.bbin` lineage + placement ([[BbinLineage]]):
  * def-use across lets, filter/`for` control deps, loop collapsing to
  * `DerivedVia(fn, summary)`, weighted `contribs`, the parent-capture
  * placement rule, and the seal `using`-pin check.
 */
final class BbinLineageTest extends AnyWordSpec {

  private def ref(n: String): RawTypeRef.Simple =
    RawTypeRef.Simple(RawTypeName(n), Nil)

  private def plain(n: String): BbinType = BbinType.Plain(ref(n))

  private def setOf(n: String): BbinType = BbinType.SetOf(ref(n))

  private def emptySet: BbinExpr = BbinExpr.SetLit(BbinSet(Nil))

  private def segs(names: String*): List[BbinPathSeg] =
    names.toList.map(BbinPathSeg.Field.apply)

  "BbinLineage.analyze" should {
    "track def-use chains across lets" in {
      val a = BbinStmt.LetStmt(BbinLet("a", plain("User"), BbinExpr.StrLit("x")))
      val b = BbinStmt.LetStmt(BbinLet("b", plain("User"), BbinExpr.Ident("a")))
      val c = BbinStmt.LetStmt(
        BbinLet("c", plain("User"), BbinExpr.BinOp(BbinBinOp.Add, BbinExpr.Ident("a"), BbinExpr.Ident("b"))),
      )
      val lin = BbinLineage.analyze(List(a, b, c))
      assert(lin.deps("a").isEmpty, s"expected no deps for 'a', got: ${lin.deps("a")}")
      assert(lin.deps("b") == Set("a"), s"unexpected deps for 'b': ${lin.deps("b")}")
      assert(lin.deps("c") == Set("a", "b"), s"unexpected deps for 'c': ${lin.deps("c")}")
    }

    "ignore schema paths and assign-target roots" in {
      val users = BbinStmt.LetStmt(BbinLet("users", setOf("User"), emptySet))
      val other = BbinStmt.LetStmt(BbinLet("other", plain("User"), BbinExpr.StrLit("y")))
      // Schema paths resolve against row types, not lets: no deps.
      val pathLet = BbinStmt.LetStmt(
        BbinLet("v", plain("User"), BbinExpr.SchemaPath(BbinPath(segs("op", "voltage")))),
      )
      // Assign target root 'users' is a write; only 'other' is read.
      val bump = BbinStmt.AssignStmt(
        BbinAssign(
          BbinPath(List(BbinPathSeg.Field("users"))),
          BbinAssignOp.Set,
          BbinExpr.BinOp(BbinBinOp.Add, BbinExpr.Ident("users"), BbinExpr.Ident("other")),
        ),
      )
      val lin = BbinLineage.analyze(List(users, other, pathLet, bump))
      assert(lin.deps("v").isEmpty, s"schema paths must contribute nothing, got: ${lin.deps("v")}")
      assert(lin.deps("users") == Set("other"), s"target root must be excluded, got: ${lin.deps("users")}")
    }

    "attribute filter predicate names as control deps of the filtered binding" in {
      val users = BbinStmt.LetStmt(BbinLet("users", setOf("User"), emptySet))
      val limit = BbinStmt.LetStmt(BbinLet("limit", plain("User"), BbinExpr.IntLit(18L, "18")))
      val adults = BbinStmt.LetStmt(
        BbinLet(
          "adults",
          setOf("User"),
          BbinExpr.Chain(
            BbinExpr.Ident("users"),
            List(
              BbinChainOp.Filter(
                BbinExpr.BinOp(BbinBinOp.Gt, BbinExpr.Ident("self"), BbinExpr.Ident("limit")),
              ),
            ),
          ),
        ),
      )
      val lin = BbinLineage.analyze(List(users, limit, adults))
      assert(lin.deps("adults") == Set("users", "limit"), s"unexpected deps: ${lin.deps("adults")}")
      assert(lin.controlDeps("adults") == Set("limit"), s"unexpected control deps: ${lin.controlDeps("adults")}")
      assert(lin.controlDeps("users").isEmpty, s"filter must not touch the source binding: ${lin.controlDeps("users")}")
    }

    "collapse loops to DerivedVia without unrolling" in {
      val users = BbinStmt.LetStmt(BbinLet("users", setOf("User"), emptySet))
      val acc   = BbinStmt.LetStmt(BbinLet("acc", plain("User"), BbinExpr.StrLit("seed")))
      val loop = BbinStmt.ForStmt(
        BbinFor(
          "i",
          "r",
          BbinExpr.Ident("users"),
          List(
            BbinStmt.AssignStmt(
              BbinAssign(BbinPath(segs("acc")), BbinAssignOp.Set, BbinExpr.Ident("r")),
            ),
            BbinStmt.LetStmt(BbinLet("tmp", plain("User"), BbinExpr.Ident("r"))),
          ),
        ),
      )
      val lin = BbinLineage.analyze(List(users, acc, loop))
      assert(lin.deps("i") == Set("users"), s"unexpected binder deps: ${lin.deps("i")}")
      assert(lin.deps("r") == Set("users"), s"unexpected binder deps: ${lin.deps("r")}")
      assert(lin.deps("tmp") == Set("r"), s"unexpected body deps: ${lin.deps("tmp")}")
      assert(
        lin.controlDeps("tmp") == Set("users"),
        s"for set-expr must gate body bindings, got: ${lin.controlDeps("tmp")}",
      )
      assert(
        lin.derivedVia == Map("acc" -> BbinDerivedVia("for((i, r) in users)", "opaque loop, no unrolling")),
        s"expected a single DerivedVia entry, got: ${lin.derivedVia}",
      )
      val via = lin.derivedVia("acc")
      assert(via.fn == "for((i, r) in users)", s"unexpected fn: ${via.fn}")
      assert(via.summary == "opaque loop, no unrolling", s"unexpected summary: ${via.summary}")
    }

    "record contribs with weight Some(1.0) for direct copies and leaf assignments" in {
      val a = BbinStmt.LetStmt(BbinLet("a", plain("User"), BbinExpr.StrLit("x")))
      val b = BbinStmt.LetStmt(BbinLet("b", plain("User"), BbinExpr.Ident("a")))
      // A top-level leaf assignment of an Ident is a direct copy too.
      val overwrite = BbinStmt.AssignStmt(
        BbinAssign(BbinPath(segs("b")), BbinAssignOp.Set, BbinExpr.Ident("a")),
      )
      val lin = BbinLineage.analyze(List(a, b, overwrite))
      assert(lin.contribs("a").isEmpty, s"literal binding must have no contribs: ${lin.contribs("a")}")
      assert(
        lin.contribs("b") == List(BbinContrib("a", Some(1.0)), BbinContrib("a", Some(1.0))),
        s"direct copy + leaf assignment must each contribute weight 1.0, got: ${lin.contribs("b")}",
      )
    }

    "record contribs with weight None through a for-body assignment" in {
      val users = BbinStmt.LetStmt(BbinLet("users", setOf("User"), emptySet))
      val acc   = BbinStmt.LetStmt(BbinLet("acc", plain("User"), BbinExpr.StrLit("seed")))
      val loop = BbinStmt.ForStmt(
        BbinFor(
          "i",
          "r",
          BbinExpr.Ident("users"),
          List(
            BbinStmt.AssignStmt(
              BbinAssign(BbinPath(segs("acc")), BbinAssignOp.Set, BbinExpr.Ident("r")),
            ),
          ),
        ),
      )
      val lin = BbinLineage.analyze(List(users, acc, loop))
      assert(
        lin.contribs("acc") == List(BbinContrib("r", None)),
        s"loop-body assignment weight is runtime-determined, got: ${lin.contribs("acc")}",
      )
      assert(
        lin.contribs("r") == List(BbinContrib("users", None)),
        s"binder iteration weight is runtime-determined, got: ${lin.contribs("r")}",
      )
    }

    "weight contribs None when the value flows through a filter" in {
      val users  = BbinStmt.LetStmt(BbinLet("users", setOf("User"), emptySet))
      val limit  = BbinStmt.LetStmt(BbinLet("limit", plain("User"), BbinExpr.IntLit(18L, "18")))
      val adults = BbinStmt.LetStmt(
        BbinLet(
          "adults",
          setOf("User"),
          BbinExpr.Chain(
            BbinExpr.Ident("users"),
            List(
              BbinChainOp.Filter(
                BbinExpr.BinOp(BbinBinOp.Gt, BbinExpr.Ident("self"), BbinExpr.Ident("limit")),
              ),
            ),
          ),
        ),
      )
      val lin = BbinLineage.analyze(List(users, limit, adults))
      assert(
        lin.contribs("adults") == List(BbinContrib("limit", None), BbinContrib("users", None)),
        s"filter-changed cardinality must not get a static weight, got: ${lin.contribs("adults")}",
      )
    }
  }

  "BbinLineage.checkPlacement" should {
    val captures = Map("C" -> "A")

    "allow construction when an ancestor binding comes earlier" in {
      val lets = List("a" -> plain("A"), "c" -> plain("C"))
      assert(BbinLineage.checkPlacement(lets, captures).isEmpty)
    }

    "reject below-lineage instantiation when no ancestor is bound" in {
      val errs = BbinLineage.checkPlacement(List("c" -> plain("C")), captures)
      assert(errs.size == 1, s"expected one error, got: $errs")
      assert(
        errs.head.contains("below-lineage instantiation") && errs.head.contains("'C'"),
        s"unexpected error: ${errs.head}",
      )
    }

    "reject construction when the ancestor comes later" in {
      val lets = List("c" -> plain("C"), "a" -> plain("A"))
      val errs = BbinLineage.checkPlacement(lets, captures)
      assert(
        errs.exists(_.contains("below-lineage instantiation")),
        s"expected a below-lineage error, got: $errs",
      )
    }

    "reject set rows of a capturing type without an ancestor" in {
      val errs = BbinLineage.checkPlacement(List("s" -> setOf("C")), captures)
      assert(
        errs.exists(_.contains("below-lineage instantiation")),
        s"expected a below-lineage error, got: $errs",
      )
    }

    "always allow sealing to a non-capturing type" in {
      assert(BbinLineage.checkPlacement(List("x" -> plain("X")), captures).isEmpty)
      assert(BbinLineage.checkPlacement(List("s" -> setOf("X")), captures).isEmpty)
    }
  }

  "BbinLineage.checkSealPin" should {
    "reject a file with no using pin" in {
      val errs = BbinLineage.checkSealPin(Nil)
      assert(errs == List("no using pin: seal has no version"), s"unexpected errors: $errs")
    }

    "reject more than one using pin" in {
      val errs = BbinLineage.checkSealPin(List(BbinUsing("shop", "1.0.0"), BbinUsing("grid", "2.0.0")))
      assert(errs == List("multi-workspace not yet supported"), s"unexpected errors: $errs")
    }

    "accept exactly one using pin" in {
      assert(BbinLineage.checkSealPin(List(BbinUsing("shop", "1.0.0"))).isEmpty)
    }
  }
}
