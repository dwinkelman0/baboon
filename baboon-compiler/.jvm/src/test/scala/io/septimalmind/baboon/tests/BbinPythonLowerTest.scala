package io.septimalmind.baboon.tests

import io.septimalmind.baboon.bbin.model._
import io.septimalmind.baboon.bbin.translator.python.BbinPythonLower
import io.septimalmind.baboon.bbin.typer._
import io.septimalmind.baboon.parser.model.{RawTypeName, RawTypeRef}
import org.scalatest.wordspec.AnyWordSpec

/** First-attempt `.bbin` → Python lowering ([[BbinPythonLower]]): a small
  * hand-built [[IRProgram]] (set literal + broadcast assign + `for` +
  * `with` provenance + `leaf` + seal) lowers to a module calling into
  * `baboon_runtime_bbin`; unlowered combinators fail cleanly with
  * `NotImplementedError` naming the op. */
final class BbinPythonLowerTest extends AnyWordSpec {

  private def ref(n: String): RawTypeRef.Simple =
    RawTypeRef.Simple(RawTypeName(n), Nil)

  private def plain(n: String): BbinType = BbinType.Plain(ref(n))

  private def field(name: String): BbinPathSeg.Field = BbinPathSeg.Field(name)

  private def path(root: String, rest: BbinPathSeg*): BbinPath =
    BbinPath(field(root) :: rest.toList)

  private def emptyLineage: BbinLineage =
    BbinLineage(Map.empty, Map.empty, Map.empty, Map.empty)

  private def coreProgram: IRProgram = {
    val assign = IRAssign(
      path("seed", field("voltage")),
      None,
      BbinAssignOp.Set,
      IRExpr.Eager(BbinExpr.FloatLit(0.9, "0.9")),
    )
    val loop = IRFor(
      "i",
      plain("str"),
      "r",
      plain("User"),
      IRExpr.Eager(BbinExpr.Ident("seed")),
      List(
        IRAssign(
          path("acc", field("voltage")),
          None,
          BbinAssignOp.Set,
          IRExpr.Eager(BbinExpr.Ident("r")),
        ),
      ),
    )
    val withProv = IRWithProv(
      List(BbinField("source", BbinFieldValue.Str("csv"))),
      List(
        IRAssign(
          path("acc", field("freq")),
          None,
          BbinAssignOp.Set,
          IRExpr.Eager(BbinExpr.FloatLit(2.8e9, "2.8e9")),
        ),
      ),
    )
    IRProgram(
      sets = List(IRSet("seed", Some(("name", "str")), "User", partial = false, IROp.Lit(1))),
      rows = List(IRRow("User", List(IRSlot("name", None), IRSlot("voltage", Some("ambient"))))),
      stmts = List(assign, loop, withProv, IRLeaf("acc", List("voltage")), IRSeal("acc", "User")),
      seals = List(
        BbinSealSite("acc", "User", Nil, List(BbinRuntimeCheck.CheckDynamicCompleteness("acc"))),
      ),
      lineage = emptyLineage,
    )
  }

  "BbinPythonLower.lower" should {
    "emit runtime calls and constructs for the core subset" in {
      val src = BbinPythonLower.lower(coreProgram)

      assert(src.contains("from baboon_runtime_bbin import"), s"expected the runtime import, got:\n$src")
      assert(src.contains("IndexedSet("), s"expected IndexedSet construction, got:\n$src")
      assert(src.contains("__new_User()"), s"expected the slot-dict partial template, got:\n$src")
      assert(src.contains("bbin_assign_broadcast"), s"expected broadcast assignment, got:\n$src")
      assert(src.contains("for i, r in"), s"expected the for loop, got:\n$src")
      assert(src.contains("with_provenance("), s"expected the provenance block, got:\n$src")
      assert(src.contains("leaf("), s"expected the leaf() call, got:\n$src")
      assert(src.contains("seal_row("), s"expected the seal() call, got:\n$src")
    }

    "emit a clean NotImplementedError for Cross" in {
      val prog = coreProgram.copy(
        sets = List(IRSet("pairs", None, "C", partial = false, IROp.Cross("others"))),
        stmts = List(IRJoin("cross", "pairs", "others")),
      )
      val src = BbinPythonLower.lower(prog)

      assert(src.contains("NotImplementedError"), s"expected a clean failure, got:\n$src")
      assert(src.contains("Cross"), s"expected the op name in the failure, got:\n$src")
    }

    "emit a clean NotImplementedError for loader sets" in {
      val prog = coreProgram.copy(
        sets = List(IRSet("ops", None, "SoC", partial = true, IROp.LoaderRef("ops.csv"))),
      )
      val src = BbinPythonLower.lower(prog)

      assert(src.contains("NotImplementedError"), s"expected a clean failure, got:\n$src")
      assert(src.contains("LoaderRef"), s"expected the op name in the failure, got:\n$src")
    }
  }
}
