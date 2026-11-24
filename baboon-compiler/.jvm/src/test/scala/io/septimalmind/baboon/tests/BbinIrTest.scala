package io.septimalmind.baboon.tests

import io.septimalmind.baboon.bbin.model.*
import io.septimalmind.baboon.bbin.typer.*
import io.septimalmind.baboon.parser.model.{FSPath, RawTypeName, RawTypeRef}
import izumi.fundamentals.collections.nonempty.NEString
import org.scalatest.wordspec.AnyWordSpec

/** Phase 6 checks for `.bbin` end-to-end AST-to-IR assembly ([[BbinIr]]):
  * phase ordering with best-effort errors, golden-shape IR nodes, and
  * scope/slot/seal-pin failure modes. Lowering stays a stub and is not
  * covered here. */
final class BbinIrTest extends AnyWordSpec {

  private def ref(n: String): RawTypeRef.Simple =
    RawTypeRef.Simple(RawTypeName(n), Nil)

  private def plain(n: String): BbinType = BbinType.Plain(ref(n))

  private def setOf(n: String): BbinType = BbinType.SetOf(ref(n))

  private def field(name: String): BbinPathSeg.Field = BbinPathSeg.Field(name)

  private def path(root: String, rest: BbinPathSeg*): BbinPath =
    BbinPath(field(root) :: rest.toList)

  private def emptySet: BbinExpr = BbinExpr.SetLit(BbinSet(Nil))

  private def loader(source: String, row: BbinType): BbinExpr.Loader =
    BbinExpr.Loader(BbinLoaderCall(BbinLoaderKind.Csv, row, source, Nil))

  private def using: List[BbinUsing] = List(BbinUsing("ws", "1.0"))

  private def raw(stmts: List[BbinStmt], usings: List[BbinUsing]): RawBbin =
    RawBbin(FSPath.parse(NEString.unsafeFrom("x.bbin")), usings, Nil, stmts)

  private def assemble(
    stmts: List[BbinStmt],
    usings: List[BbinUsing] = using,
    known: Set[String] = Set("User"),
    required: Map[String, Set[String]] = Map.empty,
    provenance: Set[String] = Set.empty,
  ): (Option[IRProgram], List[String]) =
    BbinIr.assemble(raw(stmts, usings), Nil, known, required, provenance, Map.empty)

  private def programOf(result: (Option[IRProgram], List[String])): IRProgram =
    result._1.getOrElse(fail(s"expected an IR program, got errors: ${result._2}"))

  private def accBinding: BbinStmt.LetStmt =
    BbinStmt.LetStmt(
      BbinLet("acc", plain("User"), BbinExpr.StructLit(BbinRow(RawTypeName("User"), Nil))),
    )

  private def assignTo(slot: String, value: BbinExpr): BbinStmt.AssignStmt =
    BbinStmt.AssignStmt(BbinAssign(path("acc", field(slot)), BbinAssignOp.Set, value))

  private def firstAssign(prog: IRProgram): IRAssign =
    prog.stmts
      .collectFirst { case a: IRAssign => a }
      .getOrElse(fail(s"expected an IRAssign, got: ${prog.stmts}"))

  private def seedLet(name: String = "seed"): BbinStmt.LetStmt =
    BbinStmt.LetStmt(
      BbinLet(
        name,
        setOf("User"),
        BbinExpr.SetLit(
          BbinSet(
            List(BbinSetEntry(None, BbinRow(RawTypeName("User"), List(BbinField("name", BbinFieldValue.Str("ada")))))),
          ),
        ),
      ),
    )

  /** Golden mix: loader set + set literal + accumulating `for` + `with`
    * provenance block + `.seal()` let. */
  private def goldenStmts: List[BbinStmt] = {
    val seedRow  = BbinRow(RawTypeName("User"), List(BbinField("name", BbinFieldValue.Str("ada"))))
    val opsLet   = BbinStmt.LetStmt(BbinLet("ops", setOf("User"), loader("ops.csv", setOf("User"))))
    val seedLet  = BbinStmt.LetStmt(
      BbinLet("seed", setOf("User"), BbinExpr.SetLit(BbinSet(List(BbinSetEntry(None, seedRow))))),
    )
    val accLet   = BbinStmt.LetStmt(BbinLet("acc", plain("User"), BbinExpr.StructLit(BbinRow(RawTypeName("User"), Nil))))
    val loopBody = List(
      BbinStmt.AssignStmt(BbinAssign(path("acc", field("name")), BbinAssignOp.Set, BbinExpr.Ident("r"))),
    )
    val forStmt  = BbinStmt.ForStmt(BbinFor("i", "r", BbinExpr.Ident("ops"), loopBody))
    val withStmt = BbinStmt.WithStmt(
      BbinWith(
        List(BbinField("src", BbinFieldValue.Str("csv"))),
        List(BbinStmt.AssignStmt(BbinAssign(path("acc", field("age")), BbinAssignOp.Set, BbinExpr.IntLit(36L, "36")))),
      ),
    )
    val sealLet  = BbinStmt.LetStmt(
      BbinLet("final", plain("User"), BbinExpr.Chain(BbinExpr.Ident("seed"), List(BbinChainOp.Seal))),
    )
    List(opsLet, seedLet, accLet, forStmt, withStmt, sealLet)
  }

  "BbinIr.assemble" should {
    "assemble an empty program cleanly" in {
      val (program, errors) = assemble(Nil, known = Set.empty)
      assert(errors.isEmpty, s"expected no errors, got: $errors")
      val prog = programOf((program, errors))
      assert(prog.sets.isEmpty, s"expected no sets, got: ${prog.sets}")
      assert(prog.rows.isEmpty, s"expected no rows, got: ${prog.rows}")
      assert(prog.stmts.isEmpty, s"expected no stmts, got: ${prog.stmts}")
      assert(prog.seals.isEmpty, s"expected no seals, got: ${prog.seals}")
      assert(prog.lineage.deps.isEmpty, s"expected no lineage, got: ${prog.lineage.deps}")
    }

    "build golden-shape IR nodes with lineage deps" in {
      val (program, errors) = assemble(goldenStmts)
      assert(errors.isEmpty, s"expected no errors, got: $errors")
      val prog = programOf((program, errors))

      val byName = prog.sets.map(s => s.name -> s).toMap
      assert(byName.keySet == Set("ops", "seed"), s"expected ops+seed sets, got: ${byName.keySet}")
      assert(
        byName("ops").op == IROp.LoaderRef("ops.csv"),
        s"expected a loader op, got: ${byName("ops").op}",
      )
      assert(byName("seed").op == IROp.Lit(1), s"expected a 1-entry literal, got: ${byName("seed").op}")
      assert(byName.values.forall(_.row == "User"), s"expected User rows, got: ${prog.sets}")

      assert(
        prog.rows.exists(r => r.name == "User" && r.slots.exists(_.path == "name")),
        s"expected a User row with slot 'name', got: ${prog.rows}",
      )
      assert(
        prog.stmts.exists {
          case IRFor("i", _, "r", _, _, _) => true
          case _                           => false
        },
        s"expected an IRFor(i, r), got: ${prog.stmts}",
      )
      assert(
        prog.stmts.exists(_.isInstanceOf[IRWithProv]),
        s"expected an IRWithProv, got: ${prog.stmts}",
      )
      assert(
        prog.stmts.exists {
          case IRSeal("final", _) => true
          case _                  => false
        },
        s"expected an IRSeal for 'final', got: ${prog.stmts}",
      )
      assert(
        prog.lineage.deps.get("final").exists(_.contains("seed")),
        s"expected lineage final -> seed, got: ${prog.lineage.deps}",
      )
      assert(
        prog.lineage.deps.get("acc").exists(_.contains("r")),
        s"expected lineage acc -> r, got: ${prog.lineage.deps}",
      )
    }

    "resolve for-binder types from scope" in {
      val (_, errors) = assemble(goldenStmts)
      assert(errors.isEmpty, s"expected no errors, got: $errors")
      val prog = programOf(assemble(goldenStmts))
      val loop = prog.stmts.collectFirst { case f: IRFor => f }.getOrElse(fail("expected an IRFor"))
      assert(loop.rowType == plain("User"), s"expected row type User, got: ${loop.rowType}")
    }

    "return None on a scope error" in {
      val dup = List(
        BbinStmt.LetStmt(BbinLet("x", plain("User"), emptySet)),
        BbinStmt.LetStmt(BbinLet("x", plain("User"), emptySet)),
      )
      val (program, errors) = assemble(dup)
      assert(program.isEmpty, s"expected no program, got: $program")
      assert(errors.exists(_.contains("already bound")), s"expected a duplicate error, got: $errors")
    }

    "surface a slot error while still building the clean part" in {
      val stmts = List(
        BbinStmt.LetStmt(BbinLet("good", setOf("User"), emptySet)),
        BbinStmt.LetStmt(BbinLet("bad", plain("User"), BbinExpr.Ident("ghost"))),
      )
      val (program, errors) = assemble(stmts)
      assert(errors.exists(e => e.contains("ghost")), s"expected the unknown ident, got: $errors")
      val prog = programOf((program, errors))
      assert(prog.sets.map(_.name).contains("good"), s"expected the clean set, got: ${prog.sets}")
    }

    "surface a seal-pin error for empty usings" in {
      val stmts = List(BbinStmt.LetStmt(BbinLet("good", setOf("User"), emptySet)))
      val (program, errors) = assemble(stmts, usings = Nil)
      assert(errors.exists(_.contains("no using pin")), s"expected a seal-pin error, got: $errors")
      val prog = programOf((program, errors))
      assert(prog.sets.map(_.name).contains("good"), s"expected IR despite the pin error, got: ${prog.sets}")
    }
  }

  "IRExpr classification" should {
    "mark a plain literal assignment Eager" in {
      val value  = BbinExpr.IntLit(7L, "7")
      val prog   = programOf(assemble(accBinding :: assignTo("age", value) :: Nil))
      val assign = firstAssign(prog)
      assert(assign.value == IRExpr.Eager(value), s"expected Eager($value), got: ${assign.value}")
    }

    "mark a SchemaPath-bearing assignment Deferred" in {
      val value  = BbinExpr.SchemaPath(path("schema", field("users")))
      val prog   = programOf(assemble(accBinding :: assignTo("age", value) :: Nil))
      val assign = firstAssign(prog)
      assert(assign.value == IRExpr.Deferred(value), s"expected Deferred($value), got: ${assign.value}")
    }

    "mark Deferred transitively through a BinOp" in {
      val value = BbinExpr.BinOp(
        BbinBinOp.Eq,
        BbinExpr.IntLit(1L, "1"),
        BbinExpr.SchemaPath(path("schema", field("users"))),
      )
      val prog   = programOf(assemble(accBinding :: assignTo("age", value) :: Nil))
      val assign = firstAssign(prog)
      assert(assign.value == IRExpr.Deferred(value), s"expected Deferred($value), got: ${assign.value}")
    }

    "mark a StructLit with a Path field value Deferred" in {
      val value = BbinExpr.StructLit(
        BbinRow(RawTypeName("User"), List(BbinField("name", BbinFieldValue.Path(List(field("users")))))),
      )
      val prog   = programOf(assemble(accBinding :: assignTo("name", value) :: Nil))
      val assign = firstAssign(prog)
      assert(assign.value == IRExpr.Deferred(value), s"expected Deferred($value), got: ${assign.value}")
    }

    "mark a schema-path-free for-set Eager" in {
      val opsLet  = BbinStmt.LetStmt(BbinLet("ops", setOf("User"), loader("ops.csv", setOf("User"))))
      val forStmt = BbinStmt.ForStmt(BbinFor("i", "r", BbinExpr.Ident("ops"), Nil))
      val prog    = programOf(assemble(List(opsLet, forStmt)))
      val loop    = prog.stmts.collectFirst { case f: IRFor => f }.getOrElse(fail(s"expected an IRFor, got: ${prog.stmts}"))
      assert(loop.set == IRExpr.Eager(BbinExpr.Ident("ops")), s"expected an Eager for-set, got: ${loop.set}")
    }
  }

  "IRSlot provenance" should {
    "tag slots assigned inside a with body as ambient" in {
      val withStmt = BbinStmt.WithStmt(
        BbinWith(List(BbinField("src", BbinFieldValue.Str("csv"))), List(seedLet())),
      )
      val prog = programOf(assemble(List(withStmt)))
      assert(
        prog.rows.contains(IRRow("User", List(IRSlot("name", Some("ambient"))))),
        s"expected ambient-tagged slots, got: ${prog.rows}",
      )
    }

    "tag leaf override slots as explicit" in {
      val base = BbinExpr.StructLit(
        BbinRow(RawTypeName("User"), List(BbinField("age", BbinFieldValue.Int(36L, "36")))),
      )
      val leafLet = BbinStmt.LetStmt(
        BbinLet("u", plain("User"), BbinExpr.Leaf(base, List(BbinField("name", BbinFieldValue.Str("ada"))))),
      )
      val prog = programOf(assemble(List(leafLet)))
      assert(
        prog.rows.contains(IRRow("User", List(IRSlot("age", None), IRSlot("name", Some("explicit"))))),
        s"expected base slot untagged and override slot explicit, got: ${prog.rows}",
      )
    }

    "tag withProvenance chain result slots as explicit" in {
      val chain = BbinExpr.Chain(
        BbinExpr.SetLit(
          BbinSet(
            List(BbinSetEntry(None, BbinRow(RawTypeName("User"), List(BbinField("name", BbinFieldValue.Str("ada")))))),
          ),
        ),
        List(BbinChainOp.WithProvenance(List(BbinField("src", BbinFieldValue.Str("csv"))))),
      )
      val stmts = List(BbinStmt.LetStmt(BbinLet("seed", setOf("User"), chain)))
      val prog  = programOf(assemble(stmts))
      assert(
        prog.rows.contains(IRRow("User", List(IRSlot("name", Some("explicit"))))),
        s"expected explicit chain-result slots, got: ${prog.rows}",
      )
    }

    "leave plain row literal slots untagged" in {
      val prog = programOf(assemble(List(seedLet())))
      assert(
        prog.rows.contains(IRRow("User", List(IRSlot("name", None)))),
        s"expected untagged slots, got: ${prog.rows}",
      )
    }
  }
}
