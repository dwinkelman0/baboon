package io.septimalmind.baboon.tests

import java.io.File

import fastparse.*
import io.septimalmind.baboon.bbin.model.*
import io.septimalmind.baboon.bbin.parser.DefBbin
import io.septimalmind.baboon.parser.ParserContext
import io.septimalmind.baboon.parser.model.{FSPath, RawTypeName, RawTypeRef}
import izumi.fundamentals.collections.nonempty.NEString
import org.scalatest.wordspec.AnyWordSpec

/** Parser-only tests for the `.bbin` subset grammar (header, imports, lets,
  * assignments, for/with blocks, extended types and expressions):
  * - `using pkg @ver` header
  * - `import (a, b as c) from rel:'f.bbin'`
  * - statements: `let name: Type = EXPR`, `target.path (=|+=|*=) EXPR`,
  *   `for ((idx, row) in set) { stmts }`, `with provenance({...}) { stmts }`
  * - types: plain refs, `~T` partials, `{T}` sets, `{idx: idx_type > T}`
  *   indexed sets
  * - expressions: literals, idents, struct construction
  *   (`Type { f = v, ... }`, `=` delimiters, no enclosing braces), set
  *   literals (`{k: Row { ... }}` keyed or `{Row { ... }}` positional —
  *   never mixed; row fields use `=`, `:` delimits keyed entries only),
  *   Csv/JsonLoader calls with chained `.map(...)` (loader type args accept
  *   the same four `BbinType` forms as `let` annotations and import
  *   ascriptions), `rel:'...'`
  *   literals, `.a.b[0]` schema paths, comparisons/arithmetic, `leaf()`,
  *   `partial.seal()`, method chains (`.map/.filter/.cross/.innerJoin/
  *   .outerJoin/.leftJoin/.rightJoin/.zip/.concat/.reindex/
  *   .seal/.unseal/.with_provenance/.unwrap`)
  * - every out-of-subset form (mixed keyed/positional sets, colon-delimited
  *   fields, free `seal(...)` calls,
  *   single-binder `for`, binder shadowing or writes, bare `ops[0]`,
  *   sealed/partial concat mixing, unknown statements) must fail parsing
  *   cleanly.
  */
final class BbinSyntaxParserTest extends AnyWordSpec {

  private val dummyPath = FSPath.parse(NEString.unsafeFrom("bbin-syntax-parser-test.bbin"))

  private def parseBbin(source: String): RawBbin = {
    val c = ParserContext(dummyPath, source)
    val d = new DefBbin(c)
    fastparse.parse(source, d.bbinFile(_)) match {
      case Parsed.Success((usings, imports, stmts), idx) =>
        assert(idx == source.length, s"parser left unconsumed input: [${source.drop(idx)}]")
        RawBbin(dummyPath, usings, imports, stmts)
      case f: Parsed.Failure =>
        fail(s"expected parse success for [$source], got: ${f.msg}")
    }
  }

  private def onlyLet(source: String): BbinLet = {
    val bbin = parseBbin(source)
    assert(bbin.statements.size == 1, s"expected exactly one statement, got ${bbin.statements}")
    bbin.statements.head match {
      case BbinStmt.LetStmt(let) => let
      case other                  => fail(s"expected LetStmt, got $other")
    }
  }

  private def onlyStmt(source: String): BbinStmt = {
    val bbin = parseBbin(source)
    assert(bbin.statements.size == 1, s"expected exactly one statement, got ${bbin.statements}")
    bbin.statements.head
  }

  private def expectParseFailure(source: String): Unit = {
    val c = ParserContext(dummyPath, source)
    val d = new DefBbin(c)
    fastparse.parse(source, d.bbinFile(_)) match {
      case _: Parsed.Failure =>
        succeed
      case Parsed.Success(_, idx) if idx != source.length =>
        succeed
      case Parsed.Success(v, _) =>
        fail(s"expected parse failure for [$source], got success: $v")
    }
  }

  private def assertSimpleType(tpe: BbinType, name: String): Unit = {
    assert(tpe == BbinType.Plain(RawTypeRef.Simple(RawTypeName(name), Nil)), s"expected simple type $name, got $tpe")
  }

  private def repoRoot(): File = {
    var dir = new File(sys.props("user.dir")).getAbsoluteFile
    while (dir != null && !new File(dir, "studies/power_arch1/golden.bbin").isFile) {
      dir = dir.getParentFile
    }
    assert(dir != null, "could not locate repo root holding studies/power_arch1/golden.bbin")
    dir
  }

  private def parseExampleFile(name: String): RawBbin = {
    val file = new File(repoRoot(), s"studies/power_arch1/$name")
    assert(file.isFile, s"example study file is missing: $file")
    val source = scala.io.Source.fromFile(file)("UTF-8").mkString
    val c = ParserContext(dummyPath, source)
    val d = new DefBbin(c)
    fastparse.parse(source, d.bbinFile(_)) match {
      case Parsed.Success((usings, imports, stmts), idx) =>
        assert(idx == source.length, s"$name left unconsumed input: [${source.drop(idx)}]")
        RawBbin(dummyPath, usings, imports, stmts)
      case f: Parsed.Failure =>
        fail(s"expected parse success for $name, got: ${f.msg}")
    }
  }

  "bbin header" should {

    "parse a quoted-version using" in {
      val bbin = parseBbin("using my.pkg @\"1.0.0\"\nlet a: str = rel:'f'")
      assert(bbin.usings == List(BbinUsing("my.pkg", "1.0.0")))
    }

    "parse a bare-version using" in {
      val bbin = parseBbin("using my.pkg @1.2.3\nlet a: str = rel:'f'")
      assert(bbin.usings == List(BbinUsing("my.pkg", "1.2.3")))
    }

    "parse a single-quoted-version using" in {
      val bbin = parseBbin("using my.pkg @'2.0.0-SNAPSHOT'\nlet a: str = rel:'f'")
      assert(bbin.usings == List(BbinUsing("my.pkg", "2.0.0-SNAPSHOT")))
    }

    "reject a missing using header" in {
      expectParseFailure("let a: str = rel:'f'")
    }

  }

  "bbin imports" should {

    "parse names with an alias from a single-quoted rel" in {
      val bbin = parseBbin("using p @1\nimport (a, b as c) from rel:'f.bbin'\nlet x: str = rel:'g'")
      assert(bbin.imports == List(BbinImport(List(BbinImportItem("a", None, None), BbinImportItem("b", Some("c"), None)), "f.bbin")))
    }

    "parse a double-quoted rel import" in {
      val bbin = parseBbin("using p @1\nimport (a) from rel:\"g.bbin\"\nlet x: str = rel:'h'")
      assert(bbin.imports == List(BbinImport(List(BbinImportItem("a", None, None)), "g.bbin")))
    }

    "parse all four ascription forms" in {
      val bbin = parseBbin(
        "using p @1\nimport (a, b as c, d: SoC, e as f: ~SoC) from rel:'x.bbin'\nlet x: str = rel:'g'"
      )
      val soc = RawTypeRef.Simple(RawTypeName("SoC"), Nil)
      assert(bbin.imports == List(BbinImport(
        List(
          BbinImportItem("a", None, None),
          BbinImportItem("b", Some("c"), None),
          BbinImportItem("d", None, Some(BbinType.Plain(soc))),
          BbinImportItem("e", Some("f"), Some(BbinType.Partial(soc))),
        ),
        "x.bbin",
      )))
    }

  }

  "bbin set literals" should {

    "parse a keyed set" in {
      val let = onlyLet("using p @1\nlet s: Set[Row] = {k1: Row { name = \"a\", n = 1 }, k2: Row { name = \"b\", n = 2 }}")
      assert(let.name == "s")
      assert(let.expr == BbinExpr.SetLit(BbinSet(List(
        BbinSetEntry(Some("k1"), BbinRow(RawTypeName("Row"), List(
          BbinField("name", BbinFieldValue.Str("a")),
          BbinField("n", BbinFieldValue.Int(1L, "1")),
        ))),
        BbinSetEntry(Some("k2"), BbinRow(RawTypeName("Row"), List(
          BbinField("name", BbinFieldValue.Str("b")),
          BbinField("n", BbinFieldValue.Int(2L, "2")),
        ))),
      ))))
    }

    "parse a positional set" in {
      val let = onlyLet("using p @1\nlet s: Set[Row] = {Row { name = \"a\" }, Row { name = \"b\" }}")
      val rowA = BbinRow(RawTypeName("Row"), List(BbinField("name", BbinFieldValue.Str("a"))))
      val rowB = BbinRow(RawTypeName("Row"), List(BbinField("name", BbinFieldValue.Str("b"))))
      val expected = BbinExpr.SetLit(BbinSet(List(BbinSetEntry(None, rowA), BbinSetEntry(None, rowB))))
      assert(let.expr == expected)
    }

    "parse bool, rel and schema-path field values" in {
      val let = onlyLet("using p @1\nlet s: Set[Row] = {Row { flag = true, src = rel:'f.csv', ref = .a.b[0] }}")
      assert(let.expr == BbinExpr.SetLit(BbinSet(List(
        BbinSetEntry(None, BbinRow(RawTypeName("Row"), List(
          BbinField("flag", BbinFieldValue.Bool(true)),
          BbinField("src", BbinFieldValue.Rel("f.csv")),
          BbinField("ref", BbinFieldValue.Path(List(
            BbinPathSeg.Field("a"),
            BbinPathSeg.Field("b"),
            BbinPathSeg.Index(0),
          ))),
        ))),
      ))))
    }

    "parse float, None and undefined field values" in {
      val let = onlyLet("using p @1\nlet s: Set[Row] = {Row { v = 0.9, f = 2.8e9, n = None, u = undefined }}")
      assert(let.expr == BbinExpr.SetLit(BbinSet(List(
        BbinSetEntry(None, BbinRow(RawTypeName("Row"), List(
          BbinField("v", BbinFieldValue.Float(0.9, "0.9")),
          BbinField("f", BbinFieldValue.Float(2.8e9, "2.8e9")),
          BbinField("n", BbinFieldValue.NoneLit),
          BbinField("u", BbinFieldValue.Undefined),
        ))),
      ))))
    }

    "reject a mixed keyed/positional set" in {
      expectParseFailure("using p @1\nlet s: Set[Row] = {k: Row { name = \"a\" }, Row { name = \"b\" }}")
    }

    "reject mixing implicit and explicit set keys" in {
      expectParseFailure("using p @1\nlet s: Set[Row] = {Row { name = \"a\" }, k: Row { name = \"b\" }}")
    }

    "reject a colon-delimited bare row outside a set" in {
      expectParseFailure("using p @1\nlet s: Row = Row{name: \"a\"}")
    }

    "parse a brace-wrapped single row as a one-entry positional set" in {
      // `{Type{...}}` is never a struct: at parse level it is a positional
      // set with a single entry (matching it against a non-set let-type is
      // a backend concern, not a parse failure).
      val let = onlyLet("using p @1\nlet s: Set[OpPoint] = {OpPoint{voltage = 0.9}}")
      assert(let.expr == BbinExpr.SetLit(BbinSet(List(
        BbinSetEntry(None, BbinRow(RawTypeName("OpPoint"), List(
          BbinField("voltage", BbinFieldValue.Float(0.9, "0.9")),
        ))),
      ))))
    }

  }

  "bbin struct construction" should {

    "parse Type { f = v, ... } with literal values" in {
      val let = onlyLet("using p @1\nlet op: OpPoint = OpPoint { voltage = 0.9, freq = 2.8e9, temp = 340.0 }")
      assert(let.name == "op")
      assertSimpleType(let.tpe, "OpPoint")
      assert(let.expr == BbinExpr.StructLit(BbinRow(RawTypeName("OpPoint"), List(
        BbinField("voltage", BbinFieldValue.Float(0.9, "0.9")),
        BbinField("freq", BbinFieldValue.Float(2.8e9, "2.8e9")),
        BbinField("temp", BbinFieldValue.Float(340.0, "340.0")),
      ))))
    }

    "parse bare-identifier field values referencing earlier lets" in {
      val let = onlyLet("using p @1\nlet t: LeakTable = LeakTable { points = leakPts }")
      assert(let.expr == BbinExpr.StructLit(BbinRow(RawTypeName("LeakTable"), List(
        BbinField("points", BbinFieldValue.Ident("leakPts")),
      ))))
    }

    "parse mixed literal, ident, rel and schema-path field values" in {
      val let = onlyLet("using p @1\nlet s: SoC = SoC { op = op, n = 1, flag = true, src = rel:'f.csv', ref = .a.b }")
      assert(let.expr == BbinExpr.StructLit(BbinRow(RawTypeName("SoC"), List(
        BbinField("op", BbinFieldValue.Ident("op")),
        BbinField("n", BbinFieldValue.Int(1L, "1")),
        BbinField("flag", BbinFieldValue.Bool(true)),
        BbinField("src", BbinFieldValue.Rel("f.csv")),
        BbinField("ref", BbinFieldValue.Path(List(BbinPathSeg.Field("a"), BbinPathSeg.Field("b")))),
      ))))
    }

    "reject struct fields with `:` delimiters" in {
      expectParseFailure("using p @1\nlet op: OpPoint = OpPoint { voltage: 0.9 }")
    }

    "reject colon-delimited fields inside set rows" in {
      expectParseFailure("using p @1\nlet s: Set[Row] = {Row{name: \"a\"}}")
    }

    "reject colon-delimited fields inside keyed set rows" in {
      expectParseFailure("using p @1\nlet s: Set[Row] = {k: Row{name: \"a\"}}")
    }

  }

  "bbin loaders" should {

    "parse a CsvLoader with a chained map of string and schema-path args" in {
      val let = onlyLet("using p @1\nlet t: Table = CsvLoader(Row, rel:'f.csv').map(\"name\", .fullName)")
      assertSimpleType(let.tpe, "Table")
      val rowTpe = BbinType.Plain(RawTypeRef.Simple(RawTypeName("Row"), Nil))
      val mapArgs = List(BbinMapArg.Str("name"), BbinMapArg.Path(List(BbinPathSeg.Field("fullName"))))
      val expected = BbinExpr.Loader(BbinLoaderCall(BbinLoaderKind.Csv, rowTpe, "f.csv", List(BbinMapCall(mapArgs))))
      assert(let.expr == expected)
    }

    "parse a JsonLoader with the two-string map form" in {
      val let = onlyLet("using p @1\nlet t: Table = JsonLoader(Row, rel:'f.json').map(\"a.b\", \"c\")")
      val rowTpe = BbinType.Plain(RawTypeRef.Simple(RawTypeName("Row"), Nil))
      val mapArgs = List(BbinMapArg.Str("a.b"), BbinMapArg.Str("c"))
      val expected = BbinExpr.Loader(BbinLoaderCall(BbinLoaderKind.Json, rowTpe, "f.json", List(BbinMapCall(mapArgs))))
      assert(let.expr == expected)
    }

    "parse stacked map chains" in {
      val let = onlyLet("using p @1\nlet t: Table = CsvLoader(Row, rel:'f.csv').map(\"a\", .x).map(\"b\", .y)")
      let.expr match {
        case BbinExpr.Loader(call) =>
          assert(call.maps.size == 2)
          assert(call.maps.head == BbinMapCall(List(BbinMapArg.Str("a"), BbinMapArg.Path(List(BbinPathSeg.Field("x"))))))
          assert(call.maps.tail.head == BbinMapCall(List(BbinMapArg.Str("b"), BbinMapArg.Path(List(BbinPathSeg.Field("y"))))))
        case other => fail(s"expected Loader, got $other")
      }
    }

    "parse a loader without any map" in {
      val let = onlyLet("using p @1\nlet t: Table = CsvLoader(Row, rel:'f.csv')")
      assert(let.expr == BbinExpr.Loader(BbinLoaderCall(
        BbinLoaderKind.Csv,
        BbinType.Plain(RawTypeRef.Simple(RawTypeName("Row"), Nil)),
        "f.csv",
        Nil,
      )))
    }

    "parse loader type args in all four BbinType forms" in {
      val plain = onlyLet("using p @1\nlet t: Table = CsvLoader(Row, rel:'f.csv')")
      assert(plain.expr == BbinExpr.Loader(BbinLoaderCall(
        BbinLoaderKind.Csv,
        BbinType.Plain(RawTypeRef.Simple(RawTypeName("Row"), Nil)),
        "f.csv",
        Nil,
      )))
      val partial = onlyLet("using p @1\nlet t: Table = CsvLoader(~Row, rel:'f.csv')")
      assert(partial.expr == BbinExpr.Loader(BbinLoaderCall(
        BbinLoaderKind.Csv,
        BbinType.Partial(RawTypeRef.Simple(RawTypeName("Row"), Nil)),
        "f.csv",
        Nil,
      )))
      val set = onlyLet("using p @1\nlet t: Table = CsvLoader({Row}, rel:'f.csv')")
      assert(set.expr == BbinExpr.Loader(BbinLoaderCall(
        BbinLoaderKind.Csv,
        BbinType.SetOf(RawTypeRef.Simple(RawTypeName("Row"), Nil)),
        "f.csv",
        Nil,
      )))
      val indexed = onlyLet("using p @1\nlet t: Table = CsvLoader({k: str > Row}, rel:'f.csv')")
      assert(indexed.expr == BbinExpr.Loader(BbinLoaderCall(
        BbinLoaderKind.Csv,
        BbinType.IndexedSet("k", RawTypeRef.Simple(RawTypeName("str"), Nil), RawTypeRef.Simple(RawTypeName("Row"), Nil)),
        "f.csv",
        Nil,
      )))
    }

    "reject loader type args outside the four BbinType forms" in {
      expectParseFailure("using p @1\nlet t: Table = CsvLoader(~~Row, rel:'f.csv')")
      expectParseFailure("using p @1\nlet t: Table = CsvLoader(~{Row}, rel:'f.csv')")
      expectParseFailure("using p @1\nlet t: Table = CsvLoader({{Row}}, rel:'f.csv')")
    }

  }

  "bbin scalar expressions" should {

    "parse a rel literal" in {
      val let = onlyLet("using p @1\nlet r: File = rel:'data.csv'")
      assert(let.name == "r")
      assertSimpleType(let.tpe, "File")
      assert(let.expr == BbinExpr.Rel("data.csv"))
    }

    "parse a schema path" in {
      val let = onlyLet("using p @1\nlet p0: str = .a.b[0]")
      assert(let.expr == BbinExpr.SchemaPath(BbinPath(List(
        BbinPathSeg.Field("a"),
        BbinPathSeg.Field("b"),
        BbinPathSeg.Index(0),
      ))))
    }

    "parse identifier, literal and parenthesized expressions" in {
      assert(onlyLet("using p @1\nlet x: T = foo").expr == BbinExpr.Ident("foo"))
      assert(onlyLet("using p @1\nlet x: T = \"hi\"").expr == BbinExpr.StrLit("hi"))
      assert(onlyLet("using p @1\nlet x: T = 42").expr == BbinExpr.IntLit(42L, "42"))
      assert(onlyLet("using p @1\nlet x: T = 2.8e9").expr == BbinExpr.FloatLit(2.8e9, "2.8e9"))
      assert(onlyLet("using p @1\nlet x: T = true").expr == BbinExpr.BoolLit(true))
      assert(onlyLet("using p @1\nlet x: T = None").expr == BbinExpr.NoneLit)
      assert(onlyLet("using p @1\nlet x: T = undefined").expr == BbinExpr.UndefinedLit)
      assert(onlyLet("using p @1\nlet x: T = (a)").expr == BbinExpr.Ident("a"))
    }

    "parse binary comparisons and arithmetic with precedence" in {
      assert(
        onlyLet("using p @1\nlet x: T = a + b").expr ==
          BbinExpr.BinOp(BbinBinOp.Add, BbinExpr.Ident("a"), BbinExpr.Ident("b"))
      )
      assert(
        onlyLet("using p @1\nlet x: T = .freq > 2.0e9").expr ==
          BbinExpr.BinOp(
            BbinBinOp.Gt,
            BbinExpr.SchemaPath(BbinPath(List(BbinPathSeg.Field("freq")))),
            BbinExpr.FloatLit(2.0e9, "2.0e9"),
          )
      )
      assert(
        onlyLet("using p @1\nlet x: T = 1 + 2 * 3").expr ==
          BbinExpr.BinOp(
            BbinBinOp.Add,
            BbinExpr.IntLit(1L, "1"),
            BbinExpr.BinOp(BbinBinOp.Mul, BbinExpr.IntLit(2L, "2"), BbinExpr.IntLit(3L, "3")),
          )
      )
      assert(
        onlyLet("using p @1\nlet x: T = (1 + 2) * 3").expr ==
          BbinExpr.BinOp(
            BbinBinOp.Mul,
            BbinExpr.BinOp(BbinBinOp.Add, BbinExpr.IntLit(1L, "1"), BbinExpr.IntLit(2L, "2")),
            BbinExpr.IntLit(3L, "3"),
          )
      )
    }

    "parse a full file with imports and several lets" in {
      val bbin = parseBbin(
        """using shop @"3.1.0"
          |import (orders, customers as cs) from rel:'base.bbin'
          |let a: Set[Row] = {Row { x = "1" }}
          |let b: Table = CsvLoader(Row, rel:'f.csv').map("x", .x)
          |let c: File = rel:'g.csv'
          |""".stripMargin
      )
      assert(bbin.usings == List(BbinUsing("shop", "3.1.0")))
      assert(bbin.imports == List(BbinImport(List(BbinImportItem("orders", None, None), BbinImportItem("customers", Some("cs"), None)), "base.bbin")))
      assert(bbin.statements.size == 3)
    }

  }

  "bbin extended types" should {

    "parse a partial type" in {
      val let = onlyLet("using p @1\nlet p0: ~SoC = draft")
      assert(let.tpe == BbinType.Partial(RawTypeRef.Simple(RawTypeName("SoC"), Nil)))
      assert(let.expr == BbinExpr.Ident("draft"))
    }

    "parse a set type" in {
      val let = onlyLet("using p @1\nlet s: {Row} = {Row { v = 1 }}")
      assert(let.tpe == BbinType.SetOf(RawTypeRef.Simple(RawTypeName("Row"), Nil)))
    }

    "parse an indexed set type" in {
      val let = onlyLet("using p @1\nlet s: {k: str > Row} = {k: Row { v = 1 }}")
      assert(let.tpe == BbinType.IndexedSet(
        "k",
        RawTypeRef.Simple(RawTypeName("str"), Nil),
        RawTypeRef.Simple(RawTypeName("Row"), Nil),
      ))
    }

  }

  "bbin assignments" should {

    "parse a plain assignment to a dotted target" in {
      onlyStmt("using p @1\nscalars.ceffMult = 0.9") match {
        case BbinStmt.AssignStmt(a) =>
          assert(a.target == BbinPath(List(BbinPathSeg.Field("scalars"), BbinPathSeg.Field("ceffMult"))))
          assert(a.op == BbinAssignOp.Set)
          assert(a.value == BbinExpr.FloatLit(0.9, "0.9"))
        case other => fail(s"expected AssignStmt, got $other")
      }
    }

    "parse add-assign and mul-assign" in {
      onlyStmt("using p @1\nx += 1") match {
        case BbinStmt.AssignStmt(a) =>
          assert(a.target == BbinPath(List(BbinPathSeg.Field("x"))))
          assert(a.op == BbinAssignOp.Add)
        case other => fail(s"expected AssignStmt, got $other")
      }
      onlyStmt("using p @1\nx *= 2") match {
        case BbinStmt.AssignStmt(a) => assert(a.op == BbinAssignOp.Mul)
        case other                  => fail(s"expected AssignStmt, got $other")
      }
    }

    "parse an indexed assignment target" in {
      onlyStmt("using p @1\na.b[0] = 1") match {
        case BbinStmt.AssignStmt(a) =>
          assert(a.target == BbinPath(List(
            BbinPathSeg.Field("a"),
            BbinPathSeg.Field("b"),
            BbinPathSeg.Index(0),
          )))
        case other => fail(s"expected AssignStmt, got $other")
      }
    }

  }

  "bbin for loops" should {

    "parse a for loop with read-only binders" in {
      onlyStmt("using p @1\nfor ((idx, row) in pts) {\nlet f: VfPoint = row\n}") match {
        case BbinStmt.ForStmt(f) =>
          assert(f.idx == "idx")
          assert(f.row == "row")
          assert(f.set == BbinExpr.Ident("pts"))
          assert(f.body == List(
            BbinStmt.LetStmt(BbinLet(
              "f",
              BbinType.Plain(RawTypeRef.Simple(RawTypeName("VfPoint"), Nil)),
              BbinExpr.Ident("row"),
            ))
          ))
        case other => fail(s"expected ForStmt, got $other")
      }
    }

    "reject a for loop whose binders shadow each other" in {
      expectParseFailure("using p @1\nfor ((i, i) in s) {\nx = 1\n}")
    }

    "reject a for loop that writes to a read-only binder" in {
      expectParseFailure("using p @1\nfor ((i, r) in s) {\ni = 1\n}")
    }

    "reject a for loop that re-binds a binder with let" in {
      expectParseFailure("using p @1\nfor ((i, r) in s) {\nlet i: T = s\n}")
    }

  }

  "bbin with blocks" should {

    "parse a with provenance block" in {
      onlyStmt("using p @1\nwith provenance({tool = \"baboon\", n = 1}) {\nx = 1\n}") match {
        case BbinStmt.WithStmt(w) =>
          assert(w.provenance == List(
            BbinField("tool", BbinFieldValue.Str("baboon")),
            BbinField("n", BbinFieldValue.Int(1L, "1")),
          ))
          assert(w.body.size == 1)
        case other => fail(s"expected WithStmt, got $other")
      }
    }

    "reject a with block without the provenance marker" in {
      expectParseFailure("using p @1\nwith stuff({tool = \"b\"}) {\nx = 1\n}")
    }

  }

  "bbin leaf and seal" should {

    "parse a leaf with overrides" in {
      val let = onlyLet("using p @1\nlet h: OpPoint = leaf(.op, {freq = 3.4e9, voltage = 1.0})")
      assert(let.expr == BbinExpr.Leaf(
        BbinExpr.SchemaPath(BbinPath(List(BbinPathSeg.Field("op")))),
        List(
          BbinField("freq", BbinFieldValue.Float(3.4e9, "3.4e9")),
          BbinField("voltage", BbinFieldValue.Float(1.0, "1.0")),
        ),
      ))
    }

    "parse a primitive leaf with a float base" in {
      val let = onlyLet("using p @1\nlet t: f64 = leaf(340.0, {author = \"dw\"})")
      assert(let.expr == BbinExpr.Leaf(
        BbinExpr.FloatLit(340.0, "340.0"),
        List(BbinField("author", BbinFieldValue.Str("dw"))),
      ))
    }

    "parse partial.seal with and without a target" in {
      assert(onlyLet("using p @1\nlet c: SoC = partial.seal()").expr == BbinExpr.PartialSeal(None))
      assert(onlyLet("using p @1\nlet d: SoC = partial.seal(draft)").expr == BbinExpr.PartialSeal(Some(BbinExpr.Ident("draft"))))
    }

    "parse a method seal" in {
      assert(
        onlyLet("using p @1\nlet s: SoC = draft.seal()").expr ==
          BbinExpr.Chain(BbinExpr.Ident("draft"), List(BbinChainOp.Seal))
      )
    }

    "reject free seal() calls (seal is method-only)" in {
      expectParseFailure("using p @1\nlet a: SoC = seal()")
      expectParseFailure("using p @1\nlet b: SoC = seal(draft)")
    }

    "parse a chained seal" in {
      assert(
        onlyLet("using p @1\nlet e: T = x.concat(y).seal()").expr ==
          BbinExpr.Chain(BbinExpr.Ident("x"), List(BbinChainOp.Concat(BbinExpr.Ident("y")), BbinChainOp.Seal))
      )
      assert(
        onlyLet("using p @1\nlet f: T = x.seal()").expr ==
          BbinExpr.Chain(BbinExpr.Ident("x"), List(BbinChainOp.Seal))
      )
    }

  }

  "bbin method chains" should {

    "parse filter with a comparison predicate" in {
      val let = onlyLet("using p @1\nlet t: {Row} = pts.filter(.v > 0.8)")
      assert(let.expr == BbinExpr.Chain(
        BbinExpr.Ident("pts"),
        List(BbinChainOp.Filter(BbinExpr.BinOp(
          BbinBinOp.Gt,
          BbinExpr.SchemaPath(BbinPath(List(BbinPathSeg.Field("v")))),
          BbinExpr.FloatLit(0.8, "0.8"),
        ))),
      ))
    }

    "parse cross, zip and concat" in {
      assert(
        onlyLet("using p @1\nlet g: T = a.cross(b)").expr ==
          BbinExpr.Chain(BbinExpr.Ident("a"), List(BbinChainOp.Cross(BbinExpr.Ident("b"))))
      )
      assert(
        onlyLet("using p @1\nlet h: T = a.zip(b)").expr ==
          BbinExpr.Chain(BbinExpr.Ident("a"), List(BbinChainOp.Zip(BbinExpr.Ident("b"))))
      )
      assert(
        onlyLet("using p @1\nlet i: T = a.concat(b)").expr ==
          BbinExpr.Chain(BbinExpr.Ident("a"), List(BbinChainOp.Concat(BbinExpr.Ident("b"))))
      )
    }

    "parse inner, outer, left and right joins" in {
      assert(
        onlyLet("using p @1\nlet j: T = a.innerJoin(b)").expr ==
          BbinExpr.Chain(BbinExpr.Ident("a"), List(BbinChainOp.InnerJoin(BbinExpr.Ident("b"))))
      )
      assert(
        onlyLet("using p @1\nlet k: T = a.outerJoin(b)").expr ==
          BbinExpr.Chain(BbinExpr.Ident("a"), List(BbinChainOp.OuterJoin(BbinExpr.Ident("b"))))
      )
      assert(
        onlyLet("using p @1\nlet l: T = a.leftJoin(b)").expr ==
          BbinExpr.Chain(BbinExpr.Ident("a"), List(BbinChainOp.LeftJoin(BbinExpr.Ident("b"))))
      )
      assert(
        onlyLet("using p @1\nlet m: T = a.rightJoin(b)").expr ==
          BbinExpr.Chain(BbinExpr.Ident("a"), List(BbinChainOp.RightJoin(BbinExpr.Ident("b"))))
      )
    }

    "parse reindex" in {
      assert(
        onlyLet("using p @1\nlet r: T = a.reindex(b)").expr ==
          BbinExpr.Chain(BbinExpr.Ident("a"), List(BbinChainOp.Reindex(BbinExpr.Ident("b"))))
      )
    }

    "parse map, unseal, with_provenance and unwrap chains" in {
      assert(
        onlyLet("using p @1\nlet m: T = x.map(\"a\", .b)").expr ==
          BbinExpr.Chain(BbinExpr.Ident("x"), List(BbinChainOp.Map(List(
            BbinMapArg.Str("a"),
            BbinMapArg.Path(List(BbinPathSeg.Field("b"))),
          ))))
      )
      assert(
        onlyLet("using p @1\nlet u: T = x.unseal()").expr ==
          BbinExpr.Chain(BbinExpr.Ident("x"), List(BbinChainOp.Unseal))
      )
      assert(
        onlyLet("using p @1\nlet w: T = x.with_provenance({tool = \"t\"})").expr ==
          BbinExpr.Chain(BbinExpr.Ident("x"), List(BbinChainOp.WithProvenance(List(
            BbinField("tool", BbinFieldValue.Str("t")),
          ))))
      )
      assert(
        onlyLet("using p @1\nlet z: T = x.unwrap()").expr ==
          BbinExpr.Chain(BbinExpr.Ident("x"), List(BbinChainOp.Unwrap))
      )
    }

  }

  "deferred bbin forms" should {

    "reject for expressions with a single binder" in {
      expectParseFailure("using p @1\nlet x: T = for (a in b) { a }")
    }

    "reject top-level single-binder for statements" in {
      expectParseFailure("using p @1\nfor (a in b) { a }")
    }

    "reject with expressions" in {
      expectParseFailure("using p @1\nlet x: T = a with { b }")
    }

    "reject single-arg leaf calls" in {
      expectParseFailure("using p @1\nlet x: T = leaf(a)")
    }

    "reject cross calls" in {
      expectParseFailure("using p @1\nlet x: T = cross(a, b)")
    }

    "reject zip calls" in {
      expectParseFailure("using p @1\nlet x: T = zip(a, b)")
    }

    "reject concat calls" in {
      expectParseFailure("using p @1\nlet x: T = concat(a)")
    }

    "reject filter calls" in {
      expectParseFailure("using p @1\nlet x: T = filter(a)")
    }

    "reject ops[0] extraction without a leading dot" in {
      expectParseFailure("using p @1\nlet x: T = ops[0]")
    }

    "reject concat after a seal in one chain" in {
      expectParseFailure("using p @1\nlet s: T = x.seal().concat(y)")
    }

    "reject concat on a partial.seal base" in {
      expectParseFailure("using p @1\nlet s: T = partial.seal(x).concat(y)")
    }

    "reject unknown-statement garbage" in {
      expectParseFailure("using p @1\n???")
    }

    "reject a bare identifier statement" in {
      expectParseFailure("using p @1\nfoo")
    }

  }

  "example power_arch1 studies" should {

    "parse golden.bbin" in {
      val bbin = parseExampleFile("golden.bbin")
      assert(bbin.usings == List(BbinUsing("power.arch1", "2.0.0")))
      assert(bbin.imports.isEmpty)
      val lets = bbin.statements.collect { case BbinStmt.LetStmt(l) => l.name }
      assert(lets.contains("base"), s"expected a sealed base let, got $lets")
      assert(bbin.statements.exists(_.isInstanceOf[BbinStmt.WithStmt]), "expected a with provenance block")
      assert(
        bbin.statements.exists {
          case BbinStmt.LetStmt(l) => l.expr.toString.contains("Leaf")
          case _                   => false
        },
        "expected a leaf() example",
      )
    }

    "parse golden_derate.bbin" in {
      val bbin = parseExampleFile("golden_derate.bbin")
      assert(bbin.usings == List(BbinUsing("power.arch1", "2.0.0")))
      val socRef = RawTypeRef.Simple(RawTypeName("SoC"), Nil)
      assert(bbin.imports == List(BbinImport(List(BbinImportItem("base", Some("g"), Some(BbinType.Partial(socRef)))), "golden.bbin")))
      val withs = bbin.statements.collect { case BbinStmt.WithStmt(w) => w }
      assert(withs.size == 1, s"expected one with block, got ${bbin.statements}")
      val targets = withs.head.body.collect { case BbinStmt.AssignStmt(a) => a.target }
      assert(targets.exists(_.segments == List(BbinPathSeg.Field("g"), BbinPathSeg.Field("scalars"), BbinPathSeg.Field("ceffMult"))))
      assert(targets.exists(_.segments == List(BbinPathSeg.Field("g"), BbinPathSeg.Field("scalars"), BbinPathSeg.Field("leakMult"))))
      assert(
        bbin.statements.exists {
          case BbinStmt.LetStmt(l) if l.name == "soc" =>
            l.expr == BbinExpr.Chain(BbinExpr.Ident("g"), List(BbinChainOp.Seal))
          case _ => false
        },
        "expected `let soc: SoC = g.seal()`",
      )
    }

    "parse freq_sweep.bbin" in {
      val bbin = parseExampleFile("freq_sweep.bbin")
      assert(bbin.usings == List(BbinUsing("power.arch1", "2.0.0")))
      val socRef = RawTypeRef.Simple(RawTypeName("SoC"), Nil)
      assert(bbin.imports == List(BbinImport(List(BbinImportItem("base", Some("g"), Some(BbinType.Partial(socRef)))), "golden.bbin")))
      val lets = bbin.statements.collect { case BbinStmt.LetStmt(l) => l.name }
      assert(lets.contains("sweep"), s"expected a sweep set let, got $lets")
      assert(
        bbin.statements.exists {
          case BbinStmt.LetStmt(l) => l.expr.toString.contains("Loader")
          case _                   => false
        },
        "expected a Csv/JsonLoader table line",
      )
      assert(bbin.statements.exists(_.isInstanceOf[BbinStmt.WithStmt]), "expected a with provenance block")
      assert(
        bbin.statements.exists {
          case BbinStmt.ForStmt(f) =>
            f.set == BbinExpr.Ident("sweep") &&
              f.body.exists {
                case BbinStmt.LetStmt(l) if l.name == "sealed" =>
                  l.expr == BbinExpr.Chain(BbinExpr.Ident("point"), List(BbinChainOp.Seal))
                case _ => false
              }
          case _ => false
        },
        "expected `for ((idx, row) in sweep)` sealing `point.seal()`",
      )
    }

    "parse cores_plus1.bbin" in {
      val bbin = parseExampleFile("cores_plus1.bbin")
      assert(bbin.usings == List(BbinUsing("power.arch1", "2.0.0")))
      val socRef = RawTypeRef.Simple(RawTypeName("SoC"), Nil)
      assert(bbin.imports == List(BbinImport(List(BbinImportItem("base", Some("g"), Some(BbinType.Partial(socRef)))), "golden.bbin")))
      val lets = bbin.statements.collect { case BbinStmt.LetStmt(l) => l.name }
      assert(lets.contains("arches"), s"expected an arches set let, got $lets")
      val withs = bbin.statements.collect { case BbinStmt.WithStmt(w) => w }
      assert(withs.size == 1, s"expected one with block, got ${bbin.statements}")
      val targets = withs.head.body.collect { case BbinStmt.AssignStmt(a) => a.target }
      assert(targets.exists(_.segments == List(BbinPathSeg.Field("g"), BbinPathSeg.Field("op"), BbinPathSeg.Field("voltage"))))
      assert(targets.exists(_.segments == List(BbinPathSeg.Field("g"), BbinPathSeg.Field("op"), BbinPathSeg.Field("freq"))))
      assert(
        bbin.statements.exists {
          case BbinStmt.ForStmt(f) =>
            f.set == BbinExpr.Ident("arches") &&
              f.body.exists {
                case BbinStmt.LetStmt(l) if l.name == "sealed" =>
                  l.expr == BbinExpr.Chain(BbinExpr.Ident("point"), List(BbinChainOp.Seal))
                case _ => false
              }
          case _ => false
        },
        "expected `for ((idx, row) in arches)` sealing `point.seal()`",
      )
    }

    "parse derate.bbin" in {
      val bbin = parseExampleFile("derate.bbin")
      assert(bbin.usings == List(BbinUsing("power.arch1", "2.0.0")))
      val socRef = RawTypeRef.Simple(RawTypeName("SoC"), Nil)
      assert(bbin.imports == List(BbinImport(List(BbinImportItem("base", Some("g"), Some(BbinType.Partial(socRef)))), "golden.bbin")))
      val lets = bbin.statements.collect { case BbinStmt.LetStmt(l) => l.name }
      assert(lets.contains("derates"), s"expected a derates set let, got $lets")
      val withs = bbin.statements.collect { case BbinStmt.WithStmt(w) => w }
      assert(withs.size == 1, s"expected one with block, got ${bbin.statements}")
      val targets = withs.head.body.collect { case BbinStmt.AssignStmt(a) => a.target }
      assert(targets.exists(_.segments == List(BbinPathSeg.Field("g"), BbinPathSeg.Field("scalars"), BbinPathSeg.Field("perfAdj"))))
      assert(
        bbin.statements.exists {
          case BbinStmt.ForStmt(f) =>
            f.set == BbinExpr.Ident("derates") &&
              f.body.exists {
                case BbinStmt.LetStmt(l) if l.name == "sealed" =>
                  l.expr == BbinExpr.Chain(BbinExpr.Ident("point"), List(BbinChainOp.Seal))
                case _ => false
              }
          case _ => false
        },
        "expected `for ((idx, row) in derates)` sealing `point.seal()`",
      )
    }

  }
}
