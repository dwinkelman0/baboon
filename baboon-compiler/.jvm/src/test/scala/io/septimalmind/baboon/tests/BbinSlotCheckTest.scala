package io.septimalmind.baboon.tests

import io.septimalmind.baboon.bbin.model.*
import io.septimalmind.baboon.bbin.typer.*
import io.septimalmind.baboon.parser.model.{FSPath, RawTypeName, RawTypeRef}
import izumi.fundamentals.collections.nonempty.NEString
import org.scalatest.wordspec.AnyWordSpec

class BbinSlotCheckTest extends AnyWordSpec {

  private val known = Set("User")

  private def ref(n: String): RawTypeRef.Simple =
    RawTypeRef.Simple(RawTypeName(n), Nil)

  private def plain(n: String): BbinType = BbinType.Plain(ref(n))

  private def setOf(n: String): BbinType = BbinType.SetOf(ref(n))

  private def emptySet: BbinExpr = BbinExpr.SetLit(BbinSet(Nil))

  private def raw(stmts: BbinStmt*): RawBbin =
    RawBbin(FSPath.parse(NEString.unsafeFrom("x.bbin")), Nil, Nil, stmts.toList)

  private def scopeOf(r: RawBbin): Scope = {
    val (scope, errs) = BbinScopeBuilder.build(r, Nil)
    assert(errs.isEmpty, s"scope build failed: ${errs.mkString(", ")}")
    scope
  }

  private def usersLet: BbinStmt.LetStmt =
    BbinStmt.LetStmt(BbinLet("users", setOf("User"), emptySet))

  private def chainLet(name: String, op: BbinChainOp): BbinStmt.LetStmt =
    BbinStmt.LetStmt(
      BbinLet(name, setOf("User"), BbinExpr.Chain(BbinExpr.Ident("users"), List(op))),
    )

  private def indexed(idx: String, idxType: String, elem: String): BbinType =
    BbinType.IndexedSet(idx, ref(idxType), ref(elem))

  private def prefixedRef(n: String, prefix: String): RawTypeRef.Simple =
    RawTypeRef.Simple(RawTypeName(n), List(RawTypeName(prefix)))

  private def segs(names: String*): List[BbinPathSeg] =
    names.toList.map(BbinPathSeg.Field.apply)

  private def fieldPath(names: String*): BbinExpr.SchemaPath =
    BbinExpr.SchemaPath(BbinPath(segs(names*)))

  private def pathValue(names: String*): BbinFieldValue.Path =
    BbinFieldValue.Path(segs(names*))

  private def mapArgPath(names: String*): BbinMapArg.Path =
    BbinMapArg.Path(segs(names*))

  private def csvLoader: BbinExpr.Loader =
    BbinExpr.Loader(BbinLoaderCall(BbinLoaderKind.Csv, plain("User"), "users.csv", Nil))

  private def assertClean(all: List[BbinStmt]): Unit = {
    val scope = scopeOf(raw(all*))
    all.foreach { s =>
      val errs = BbinSlotCheck.check(scope, known, s)
      assert(errs.isEmpty, s"expected no errors, got: $errs")
    }
  }

  "BbinSlotCheck" should {
    "reject a let whose row type is not in knownTypes" in {
      val stmt  = BbinStmt.LetStmt(BbinLet("mystery", plain("Mystery"), emptySet))
      val scope = scopeOf(raw(stmt))
      val errs  = BbinSlotCheck.check(scope, known, stmt)
      assert(errs.exists(_.contains("Mystery")), s"expected unknown row type error, got: $errs")
    }

    "reject a set literal whose row type is not in knownTypes" in {
      val row   = BbinRow(RawTypeName("Ghost"), Nil)
      val entry = BbinSetEntry(None, row)
      val stmt  = BbinStmt.LetStmt(BbinLet("g", setOf("User"), BbinExpr.SetLit(BbinSet(List(entry)))))
      val scope = scopeOf(raw(stmt))
      val errs  = BbinSlotCheck.check(scope, known, stmt)
      assert(errs.exists(_.contains("Ghost")), s"expected unknown row type error, got: $errs")
    }

    "reject withProvenance with no fields" in {
      val stmt  = chainLet("w", BbinChainOp.WithProvenance(Nil))
      val scope = scopeOf(raw(usersLet, stmt))
      val errs  = BbinSlotCheck.check(scope, known, stmt)
      assert(errs.exists(_.contains("rovenance")), s"expected withProvenance error, got: $errs")
    }

    "reject a filter with an undefined predicate" in {
      val stmt  = chainLet("f", BbinChainOp.Filter(BbinExpr.UndefinedLit))
      val scope = scopeOf(raw(usersLet, stmt))
      val errs  = BbinSlotCheck.check(scope, known, stmt)
      assert(errs.exists(_.contains("filter")), s"expected filter error, got: $errs")
    }

    "reject a cross with an undefined other" in {
      val stmt  = chainLet("c", BbinChainOp.Cross(BbinExpr.UndefinedLit))
      val scope = scopeOf(raw(usersLet, stmt))
      val errs  = BbinSlotCheck.check(scope, known, stmt)
      assert(errs.exists(_.contains("cross")), s"expected cross error, got: $errs")
    }

    "reject a map with no arguments" in {
      val stmt  = chainLet("m", BbinChainOp.Map(Nil))
      val scope = scopeOf(raw(usersLet, stmt))
      val errs  = BbinSlotCheck.check(scope, known, stmt)
      assert(errs.exists(_.contains("map")), s"expected map error, got: $errs")
    }

    "reject writes to read-only for binders" in {
      val body = List(
        BbinStmt.AssignStmt(
          BbinAssign(
            BbinPath(List(BbinPathSeg.Field("r"))),
            BbinAssignOp.Set,
            BbinExpr.Ident("users"),
          ),
        ),
      )
      val loop  = BbinStmt.ForStmt(BbinFor("i", "r", BbinExpr.Ident("users"), body))
      val scope = scopeOf(raw(usersLet, loop))
      val errs  = BbinSlotCheck.check(scope, known, loop)
      assert(
        errs.exists(e => e.contains("read-only") && e.contains("'r'")),
        s"expected read-only write error, got: $errs",
      )
    }

    "reject reads of unknown identifiers" in {
      val stmt  = BbinStmt.LetStmt(BbinLet("a", plain("User"), BbinExpr.Ident("nope")))
      val scope = scopeOf(raw(stmt))
      val errs  = BbinSlotCheck.check(scope, known, stmt)
      assert(errs.exists(_.contains("nope")), s"expected unknown identifier error, got: $errs")
    }

    "accept a legal let/assign/chain/for/with mix" in {
      // `.map` is loader-only: the column-mapping chain rides a loader base,
      // while the set chain keeps filter/with_provenance only.
      val srcLet = BbinStmt.LetStmt(BbinLet("src", setOf("User"), csvLoader))
      val mapped = BbinStmt.LetStmt(
        BbinLet(
          "mapped",
          setOf("User"),
          BbinExpr.Chain(
            csvLoader,
            List(BbinChainOp.Map(List(BbinMapArg.Str("name"), mapArgPath("fullName")))),
          ),
        ),
      )
      val adults = BbinStmt.LetStmt(
        BbinLet(
          "adults",
          setOf("User"),
          BbinExpr.Chain(
            BbinExpr.Ident("users"),
            List(
              BbinChainOp.Filter(
                BbinExpr.BinOp(BbinBinOp.Gt, BbinExpr.Ident("self"), BbinExpr.IntLit(18L, "18")),
              ),
              BbinChainOp.WithProvenance(List(BbinField("src", BbinFieldValue.Str("csv")))),
            ),
          ),
        ),
      )
      def assignToUsers(value: BbinExpr): BbinStmt.AssignStmt =
        BbinStmt.AssignStmt(
          BbinAssign(BbinPath(List(BbinPathSeg.Field("users"))), BbinAssignOp.Set, value),
        )
      val bump = BbinStmt.AssignStmt(
        BbinAssign(
          BbinPath(List(BbinPathSeg.Field("users"))),
          BbinAssignOp.Add,
          BbinExpr.Ident("users"),
        ),
      )
      val loop = BbinStmt.ForStmt(
        BbinFor("i", "r", BbinExpr.Ident("users"), List(assignToUsers(BbinExpr.Ident("r")))),
      )
      val withBlock = BbinStmt.WithStmt(
        BbinWith(
          List(BbinField("src", BbinFieldValue.Str("csv"))),
          List(assignToUsers(BbinExpr.Ident("users"))),
        ),
      )
      val all = List(usersLet, srcLet, mapped, adults, bump, loop, withBlock)
      assertClean(all)
    }

    "accept indexed sets with str and unsigned-int index types" in {
      List("str", "u08", "u16", "u32", "u64", "usize").foreach { idxTpe =>
        val stmt  = BbinStmt.LetStmt(BbinLet("s", indexed("k", idxTpe, "User"), emptySet))
        val scope = scopeOf(raw(usersLet, stmt))
        val errs  = BbinSlotCheck.check(scope, known, stmt)
        assert(errs.isEmpty, s"expected no errors for idx type '$idxTpe', got: $errs")
      }
    }

    "accept schema paths in deferred positions (filter, assign, cross mapper, leaf args, for body)" in {
      val filtered = chainLet(
        "f",
        BbinChainOp.Filter(
          BbinExpr.BinOp(BbinBinOp.Gt, fieldPath("op", "voltage"), BbinExpr.FloatLit(0.8, "0.8")),
        ),
      )
      val assign = BbinStmt.AssignStmt(
        BbinAssign(
          BbinPath(List(BbinPathSeg.Field("users"), BbinPathSeg.Field("op"), BbinPathSeg.Field("voltage"))),
          BbinAssignOp.Set,
          fieldPath("op", "voltage"),
        ),
      )
      val crossed = chainLet(
        "c",
        BbinChainOp.Cross(
          BbinExpr.StructLit(BbinRow(RawTypeName("User"), List(BbinField("name", pathValue("op", "voltage"))))),
        ),
      )
      val leafLet = BbinStmt.LetStmt(
        BbinLet(
          "t",
          plain("User"),
          BbinExpr.Leaf(BbinExpr.FloatLit(340.0, "340.0"), List(BbinField("src", pathValue("op", "voltage")))),
        ),
      )
      val loop = BbinStmt.ForStmt(
        BbinFor(
          "i",
          "r",
          BbinExpr.Ident("users"),
          List(BbinStmt.LetStmt(BbinLet("v", plain("User"), fieldPath("op", "voltage")))),
        ),
      )
      assertClean(List(usersLet, filtered, assign, crossed, leafLet, loop))
    }

    "accept loader .map column declarations with schema paths (folded and chained)" in {
      // Mirrors golden.bbin: `CsvLoader(T, rel:'…').map("V", .voltage)` — the
      // parser folds loader maps into BbinLoaderCall.maps; both shapes stay legal.
      val folded = BbinStmt.LetStmt(
        BbinLet(
          "pts",
          setOf("User"),
          BbinExpr.Loader(
            BbinLoaderCall(
              BbinLoaderKind.Csv,
              plain("User"),
              "users.csv",
              List(BbinMapCall(List(BbinMapArg.Str("voltage"), mapArgPath("op", "voltage")))),
            ),
          ),
        ),
      )
      val chained = BbinStmt.LetStmt(
        BbinLet(
          "mapped",
          setOf("User"),
          BbinExpr.Chain(csvLoader, List(BbinChainOp.Map(List(BbinMapArg.Str("V"), mapArgPath("op", "voltage"))))),
        ),
      )
      assertClean(List(folded, chained))
    }

    "reject an indexed set whose index type is not a primitive leaf" in {
      val rowStmt = BbinStmt.LetStmt(BbinLet("s", indexed("k", "User", "User"), emptySet))
      val rowErrs = BbinSlotCheck.check(scopeOf(raw(usersLet, rowStmt)), known, rowStmt)
      assert(
        rowErrs.exists(e => e.contains("index type") && e.contains("primitive leaf")),
        s"expected index-type error, got: $rowErrs",
      )

      val qualified = BbinStmt.LetStmt(
        BbinLet("q", BbinType.IndexedSet("k", prefixedRef("str", "power"), ref("User")), emptySet),
      )
      val qErrs = BbinSlotCheck.check(scopeOf(raw(usersLet, qualified)), known, qualified)
      assert(
        qErrs.exists(e => e.contains("index type") && e.contains("primitive leaf")),
        s"expected index-type error for a qualified primitive, got: $qErrs",
      )
    }

    "reject a schema path in an eager let RHS" in {
      val direct = BbinStmt.LetStmt(BbinLet("v", plain("User"), fieldPath("op", "voltage")))
      val dErrs  = BbinSlotCheck.check(scopeOf(raw(usersLet, direct)), known, direct)
      assert(
        dErrs.exists(_.contains("requires fn()/deferred row context")),
        s"expected deferred-context error, got: $dErrs",
      )

      val nested = BbinStmt.LetStmt(
        BbinLet(
          "b",
          plain("User"),
          BbinExpr.BinOp(BbinBinOp.Gt, fieldPath("op", "voltage"), BbinExpr.FloatLit(0.8, "0.8")),
        ),
      )
      val nErrs = BbinSlotCheck.check(scopeOf(raw(usersLet, nested)), known, nested)
      assert(
        nErrs.exists(_.contains("requires fn()/deferred row context")),
        s"expected deferred-context error, got: $nErrs",
      )
    }

    "reject a schema path in a set-literal field value" in {
      val row   = BbinRow(RawTypeName("User"), List(BbinField("name", pathValue("op", "voltage"))))
      val entry = BbinSetEntry(None, row)
      val stmt  = BbinStmt.LetStmt(BbinLet("s", setOf("User"), BbinExpr.SetLit(BbinSet(List(entry)))))
      val errs  = BbinSlotCheck.check(scopeOf(raw(usersLet, stmt)), known, stmt)
      assert(
        errs.exists(_.contains("requires fn()/deferred row context")),
        s"expected deferred-context error, got: $errs",
      )
    }

    "reject a map chain whose base is not a loader" in {
      val stmt = chainLet("m", BbinChainOp.Map(List(BbinMapArg.Str("a"))))
      val errs = BbinSlotCheck.check(scopeOf(raw(usersLet, stmt)), known, stmt)
      assert(
        errs.exists(_.contains("map is loader-only; iteration is native for")),
        s"expected loader-only map error, got: $errs",
      )
    }
  }
}
