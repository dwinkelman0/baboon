package io.septimalmind.baboon.tests

import fastparse.*
import io.septimalmind.baboon.parser.ParserContext
import io.septimalmind.baboon.parser.model.{BodyExpr, BodyLiteral, BodyStmt, FSPath, MatchPattern, RawCaptureStep, RawDefaultValue, RawDto, RawDtoMember, RawFieldName, RawFn, RawLocatorSeg, RawMigrate, RawStarCapture, RawTypeName, RawTypeRef}
import izumi.fundamentals.collections.nonempty.NEString
import org.scalatest.wordspec.AnyWordSpec

/** Parser-only tests for Phase A context-function syntax:
  * - `@default(fn[...](...) { ... })`
  * - `@normalize(fn[...](...) { ... })`
  * - `@migrate(fn[...](...) { ... }, [sources])`
  * - read-only `fn` properties
  * - body expressions (path/index/call, literals, binops, unary, if/else, let)
  */
final class FnSyntaxParserTest extends AnyWordSpec {

  private val dummyPath = FSPath.parse(NEString.unsafeFrom("fn-syntax-parser-test.baboon"))

  private def ctx(input: String): ParserContext = ParserContext(dummyPath, input)

  private def parseDto(source: String): RawDto = {
    val c = ctx(source)
    fastparse.parse(c.content, c.defDto.dtoEnclosed(_)) match {
      case Parsed.Success(v, idx) =>
        assert(idx == source.length, s"parser left unconsumed input: [${source.drop(idx)}]")
        v
      case f: Parsed.Failure =>
        fail(s"expected parse success for [$source], got: ${f.msg}")
    }
  }

  private def onlyField(source: String): RawDtoMember.FieldDef = {
    val dto = parseDto(source)
    assert(dto.members.size == 1, s"expected exactly one member, got ${dto.members}")
    dto.members.head match {
      case f: RawDtoMember.FieldDef => f
      case other => fail(s"expected FieldDef, got $other")
    }
  }

  private def onlyProperty(source: String): RawDtoMember.FnProperty = {
    val dto = parseDto(source)
    assert(dto.members.size == 1, s"expected exactly one member, got ${dto.members}")
    dto.members.head match {
      case p: RawDtoMember.FnProperty => p
      case other => fail(s"expected FnProperty, got $other")
    }
  }

  private def unwrapFn(fn: Option[RawFn]): RawFn = {
    fn match {
      case Some(f) => f
      case other => fail(s"expected Some(RawFn), got $other")
    }
  }

  /** Type-erased view of top-level `let` bindings, derived from `stmts`
    * (`RawBody` no longer carries a separate `letBindings` list).
    */
  private def letsOf(fn: RawFn): List[(String, BodyExpr)] =
    fn.body.stmts.collect { case BodyStmt.Let(n, _, v) => (n, v) }

  private def assertSimpleType(tpe: RawTypeRef, name: String): Unit = {
    assert(tpe == RawTypeRef.Simple(RawTypeName(name), Nil), s"expected simple type $name, got $tpe")
  }

  private def expectParseFailure(source: String): Unit = {
    val c = ctx(source)
    try {
      fastparse.parse(c.content, c.defDto.dtoEnclosed(_)) match {
        case _: Parsed.Failure =>
          succeed
        case Parsed.Success(_, idx) if idx != source.length =>
          succeed // dto did not accept the whole source
        case Parsed.Success(v, _) =>
          fail(s"expected parse failure for [$source], got success: $v")
      }
    } catch {
      case _: IllegalArgumentException =>
        succeed // method-whitelist guard rejects from inside the parser
    }
  }

  "context-function syntax" should {

    "parse @default(fn[p: Parent]() { p.name })" in {
      val f = onlyField("data D { @default(fn[p: Parent]() { p.name }) x: str }")
      val fn = unwrapFn(f.field.fn)
      assert(fn.captures.size == 1)
      assert(fn.captures.head.name == "p")
      assertSimpleType(fn.captures.head.rootType, "Parent")
      assert(fn.params.isEmpty)
      assert(letsOf(fn).isEmpty)
      assert(fn.body.result == BodyExpr.Path("p", List(BodyExpr.Segment.Field("name"))))
    }

    "parse @default(fn[p: Parent](self) { self.x })" in {
      val f = onlyField("data D { @default(fn[p: Parent](self) { self.x }) x: str }")
      val fn = unwrapFn(f.field.fn)
      assert(fn.captures.size == 1)
      assert(fn.captures.head.name == "p")
      assert(fn.params.size == 1)
      assert(fn.params.head.name == "self")
      assertSimpleType(fn.params.head.tpe, "Self")
      assert(fn.body.result == BodyExpr.Path("self", List(BodyExpr.Segment.Field("x"))))
    }

    "parse @normalize(fn[t: Parent](self) { self.trim() })" in {
      val f = onlyField("data D { @normalize(fn[t: Parent](self) { self.trim() }) x: str }")
      val fn = f.field.normalize.getOrElse(fail("expected normalize fn"))
      assert(fn.captures.size == 1)
      assert(fn.captures.head.name == "t")
      assert(fn.params.size == 1)
      assert(fn.params.head.name == "self")
      assert(fn.body.result == BodyExpr.Call(
        BodyExpr.Path("self", Nil),
        "trim",
        Nil,
      ))
    }

    "parse @migrate with source list" in {
      val f = onlyField(
        "data D { @migrate(fn[old: MyStruct]() { old.first_name.concat(old.last_name) }, [MyStruct.first_name, MyStruct.last_name]) x: str }"
      )
      val m = f.field.migrate.getOrElse(fail("expected migrate"))
      assert(m.sources == List("MyStruct.first_name", "MyStruct.last_name"))
      val fn = m.fn
      assert(fn.captures.head.name == "old")
      assert(fn.body.result == BodyExpr.Call(
        BodyExpr.Path("old", List(BodyExpr.Segment.Field("first_name"))),
        "concat",
        List(BodyExpr.Path("old", List(BodyExpr.Segment.Field("last_name")))),
      ))
    }

    "parse a read-only fn property" in {
      val p = onlyProperty("data D { area: f32 = fn(self) { self.width * self.height } }")
      val fn = p.fn
      assert(p.field.name == RawFieldName("area"))
      assertSimpleType(p.field.tpe, "f32")
      assert(fn.params.size == 1)
      assert(fn.params.head.name == "self")
      assert(fn.body.result == BodyExpr.BinOp(
        "*",
        BodyExpr.Path("self", List(BodyExpr.Segment.Field("width"))),
        BodyExpr.Path("self", List(BodyExpr.Segment.Field("height"))),
      ))
    }

    "accept the legacy self: Self alias the same as bare self" in {
      val p = onlyProperty("data D { area: f32 = fn(self: Self) { self.width * self.height } }")
      val fn = p.fn
      assert(fn.params.size == 1)
      assert(fn.params.head.name == "self")
      assertSimpleType(fn.params.head.tpe, "Self")
      val bare = onlyProperty("data D { area: f32 = fn(self) { self.width * self.height } }")
      assert(bare.fn.params == fn.params)
    }

    "parse a computed fn whose capture situates the container" in {
      // `= fn` (FnProperty/computed) captures situate the enclosing container:
      // `s: MyStruct.member` binds `s` to that member (transparent through
      // opt/set, `.Some` implied), while `self` is the container
      // instance itself — contrast `@normalize` / `@default(fn)`, where
      // `self` is the field's own value. Parser-level only: roles resolve in P2.
      val p = onlyProperty("data D { area: f32 = fn[s: MyStruct.member](self) { self.width } }")
      val fn = p.fn
      assert(fn.captures.size == 1)
      assert(fn.captures.head.name == "s")
      assertSimpleType(fn.captures.head.rootType, "MyStruct")
      assert(fn.captures.head.path == List(RawCaptureStep.Single(RawLocatorSeg.Field("member"))))
      assert(fn.params.map(_.name) == List("self"))
      assertSimpleType(fn.params.head.tpe, "Self")
    }

    "parse a body with let bindings and method call" in {
      val f = onlyField("data D { @default(fn[self: Self]() { let a = self.x; let b = a.uppercase(); b }) x: str }")
      val fn = unwrapFn(f.field.fn)
      assert(letsOf(fn).size == 2)
      assert(letsOf(fn).head == ("a", BodyExpr.Path("self", List(BodyExpr.Segment.Field("x")))))
      assert(letsOf(fn).tail.head == ("b", BodyExpr.Call(
        BodyExpr.Path("a", Nil),
        "uppercase",
        Nil,
      )))
      assert(fn.body.result == BodyExpr.Path("b", Nil))
    }

    "parse a star capture" in {
      val f = onlyField("data D { @default(fn[s: MyStruct($chain* : .objs[$idx])]() { s }) x: str }")
      val fn = unwrapFn(f.field.fn)
      assert(fn.captures.size == 1)
      val cap = fn.captures.head
      assert(cap.name == "s")
      assert(cap.path == List(
        RawCaptureStep.Star(RawStarCapture("chain", List(
          RawLocatorSeg.Field("objs"),
          RawLocatorSeg.Key("idx"),
        ))),
      ))
    }

    "parse literal expressions" in {
      val f = onlyField("data D { @default(fn[self: Self]() { 42 + 3.14 == true && !false || null.concat(\"x\") }) x: str }")
      val fn = unwrapFn(f.field.fn)
      // Just ensure it parses to a non-trivial expression.
      assert(fn.body.result.isInstanceOf[BodyExpr.BinOp])
    }

    "parse an if/else expression" in {
      val f = onlyField("data D { @default(fn[self: Self]() { if self.x { 1 } else { 2 } }) x: str }")
      val fn = unwrapFn(f.field.fn)
      assert(fn.body.result.isInstanceOf[BodyExpr.If])
    }

    "reject multi-capture fn[a: A, b: B](x: X) { x }" in {
      expectParseFailure("data D { @default(fn[a: A, b: B](x: X) { x }) y: str }")
    }

    "reject !prev capture prefix fn[!old: T](x: X) { x }" in {
      expectParseFailure("data D { @default(fn[!old: T](x: X) { x }) y: str }")
    }

    "parse star capture with trailing locator" in {
      val f = onlyField("data D { @default(fn[s: MyStruct($chain* : .objs[$idx]).prop]() { s }) x: str }")
      val fn = unwrapFn(f.field.fn)
      val cap = fn.captures.head
      assert(cap.path == List(
        RawCaptureStep.Star(RawStarCapture("chain", List(
          RawLocatorSeg.Field("objs"),
          RawLocatorSeg.Key("idx"),
        ))),
        RawCaptureStep.Single(RawLocatorSeg.Field("prop")),
      ))
    }

    "parse nested method chains" in {
      val f = onlyField("data D { @default(fn[s: S]() { s.trim().uppercase() }) x: str }")
      val fn = unwrapFn(f.field.fn)
      assert(fn.body.result == BodyExpr.Call(
        BodyExpr.Call(BodyExpr.Path("s", Nil), "trim", Nil),
        "uppercase",
        Nil,
      ))
    }

    "parse operator precedence in 1 + 2 * 3" in {
      val f = onlyField("data D { @default(fn[self: Self]() { 1 + 2 * 3 }) x: str }")
      val fn = unwrapFn(f.field.fn)
      assert(fn.body.result == BodyExpr.BinOp(
        "+",
        BodyExpr.Lit(BodyLiteral.IntLit(1)),
        BodyExpr.BinOp(
          "*",
          BodyExpr.Lit(BodyLiteral.IntLit(2)),
          BodyExpr.Lit(BodyLiteral.IntLit(3)),
        ),
      ))
    }

    "parse unary with parens" in {
      val f = onlyField("data D { @default(fn[self: Self]() { -(1 + 2) }) x: str }")
      val fn = unwrapFn(f.field.fn)
      assert(fn.body.result == BodyExpr.UnaryOp(
        "-",
        BodyExpr.BinOp(
          "+",
          BodyExpr.Lit(BodyLiteral.IntLit(1)),
          BodyExpr.Lit(BodyLiteral.IntLit(2)),
        ),
      ))
    }

    "parse @normalize with let and if/else" in {
      val f = onlyField("data D { @normalize(fn[t: T](self) { let v = self.x; if v { 1 } else { 2 } }) x: str }")
      val fn = f.field.normalize.getOrElse(fail("expected normalize fn"))
      assert(letsOf(fn) == List(("v", BodyExpr.Path("self", List(BodyExpr.Segment.Field("x"))))))
      assert(fn.body.result.isInstanceOf[BodyExpr.If])
    }

    "parse fn property with captures" in {
      val p = onlyProperty("data D { area: f32 = fn[p: Parent](self) { self.width } }")
      val fn = p.fn
      assert(fn.captures.size == 1)
      assert(fn.captures.head.name == "p")
      assertSimpleType(fn.captures.head.rootType, "Parent")
      assert(fn.params.size == 1)
      assert(fn.params.head.name == "self")
      assert(fn.body.result == BodyExpr.Path("self", List(BodyExpr.Segment.Field("width"))))
    }

    "parse computed fn with external args" in {
      val p = onlyProperty("data D { curve: f32 = fn(self, t: f32) { self.base + t } }")
      val fn = p.fn
      assert(fn.params.size == 2)
      assert(fn.params.map(_.name) == List("self", "t"))
      assertSimpleType(fn.params(1).tpe, "f32")
    }

    "reject unclosed fn" in {
      expectParseFailure("data D { @default(fn[a: A](x: X { x }) y: str }")
    }

    "reject star with no steps" in {
      expectParseFailure("data D { @default(fn[s: T($c* :)]() { s }) x: str }")
    }

    "parse a non-whitelisted method call (rejection moved to the typer)" in {
      // DefDto.callSuffix no longer guards method names: user-defined
      // (computed-fn) methods like `self.table.lookup()` must parse. Unknown
      // methods are rejected later by FnBodyTyper ("unknown method: ..."),
      // covered by FnBodyTypingTest "reject an unknown method".
      val f = onlyField("data D { @default(fn[s: S]() { s.foo() }) x: str }")
      val fn = unwrapFn(f.field.fn)
      assert(fn.body.result == BodyExpr.Call(BodyExpr.Path("s", Nil), "foo", Nil))
    }

    "reject empty fn body" in {
      expectParseFailure("data D { @default(fn[a: A]() { }) x: str }")
    }

    "reject @migrate without source list brackets" in {
      expectParseFailure("data D { @migrate(fn[old: S]() { old }) x: str }")
    }

    "parse || binding looser than &&" in {
      val f = onlyField("data D { @default(fn[self: Self]() { a || b && c }) x: str }")
      val fn = unwrapFn(f.field.fn)
      assert(fn.body.result == BodyExpr.BinOp(
        "||",
        BodyExpr.Path("a", Nil),
        BodyExpr.BinOp(
          "&&",
          BodyExpr.Path("b", Nil),
          BodyExpr.Path("c", Nil),
        ),
      ))
    }

    "parse || as left-associative" in {
      val f = onlyField("data D { @default(fn[self: Self]() { a || b || c }) x: str }")
      val fn = unwrapFn(f.field.fn)
      assert(fn.body.result == BodyExpr.BinOp(
        "||",
        BodyExpr.BinOp("||", BodyExpr.Path("a", Nil), BodyExpr.Path("b", Nil)),
        BodyExpr.Path("c", Nil),
      ))
    }

    "parse == and != as left-associative" in {
      val f = onlyField("data D { @default(fn[self: Self]() { a == b != c }) x: str }")
      val fn = unwrapFn(f.field.fn)
      assert(fn.body.result == BodyExpr.BinOp(
        "!=",
        BodyExpr.BinOp("==", BodyExpr.Path("a", Nil), BodyExpr.Path("b", Nil)),
        BodyExpr.Path("c", Nil),
      ))
    }

    "parse comparison operators" in {
      val f = onlyField("data D { @default(fn[self: Self]() { a < b <= c }) x: str }")
      val fn = unwrapFn(f.field.fn)
      assert(fn.body.result == BodyExpr.BinOp(
        "<=",
        BodyExpr.BinOp("<", BodyExpr.Path("a", Nil), BodyExpr.Path("b", Nil)),
        BodyExpr.Path("c", Nil),
      ))
      val g = onlyField("data D { @default(fn[self: Self]() { x > y >= z }) x: str }")
      val gfn = unwrapFn(g.field.fn)
      assert(gfn.body.result == BodyExpr.BinOp(
        ">=",
        BodyExpr.BinOp(">", BodyExpr.Path("x", Nil), BodyExpr.Path("y", Nil)),
        BodyExpr.Path("z", Nil),
      ))
    }

    "parse + and - as left-associative (a-b-c)" in {
      val f = onlyField("data D { @default(fn[self: Self]() { a - b - c }) x: str }")
      val fn = unwrapFn(f.field.fn)
      assert(fn.body.result == BodyExpr.BinOp(
        "-",
        BodyExpr.BinOp("-", BodyExpr.Path("a", Nil), BodyExpr.Path("b", Nil)),
        BodyExpr.Path("c", Nil),
      ))
      val g = onlyField("data D { @default(fn[self: Self]() { a - b + c }) x: str }")
      val gfn = unwrapFn(g.field.fn)
      assert(gfn.body.result == BodyExpr.BinOp(
        "+",
        BodyExpr.BinOp("-", BodyExpr.Path("a", Nil), BodyExpr.Path("b", Nil)),
        BodyExpr.Path("c", Nil),
      ))
    }

    "parse *, / and % as left-associative" in {
      val f = onlyField("data D { @default(fn[self: Self]() { a * b / c % d }) x: str }")
      val fn = unwrapFn(f.field.fn)
      assert(fn.body.result == BodyExpr.BinOp(
        "%",
        BodyExpr.BinOp(
          "/",
          BodyExpr.BinOp("*", BodyExpr.Path("a", Nil), BodyExpr.Path("b", Nil)),
          BodyExpr.Path("c", Nil),
        ),
        BodyExpr.Path("d", Nil),
      ))
    }

    "parse mixed precedence in a + b * c - d" in {
      val f = onlyField("data D { @default(fn[self: Self]() { a + b * c - d }) x: str }")
      val fn = unwrapFn(f.field.fn)
      assert(fn.body.result == BodyExpr.BinOp(
        "-",
        BodyExpr.BinOp(
          "+",
          BodyExpr.Path("a", Nil),
          BodyExpr.BinOp("*", BodyExpr.Path("b", Nil), BodyExpr.Path("c", Nil)),
        ),
        BodyExpr.Path("d", Nil),
      ))
    }

    "parse nested parens ((a + b) * (c - d))" in {
      val f = onlyField("data D { @default(fn[self: Self]() { ((a + b) * (c - d)) }) x: str }")
      val fn = unwrapFn(f.field.fn)
      assert(fn.body.result == BodyExpr.BinOp(
        "*",
        BodyExpr.BinOp("+", BodyExpr.Path("a", Nil), BodyExpr.Path("b", Nil)),
        BodyExpr.BinOp("-", BodyExpr.Path("c", Nil), BodyExpr.Path("d", Nil)),
      ))
    }

    "parse unary !, - and doubled forms through parens" in {
      val f = onlyField("data D { @default(fn[self: Self]() { !flag }) x: str }")
      assert(unwrapFn(f.field.fn).body.result == BodyExpr.UnaryOp("!", BodyExpr.Path("flag", Nil)))
      val g = onlyField("data D { @default(fn[self: Self]() { -x }) x: str }")
      assert(unwrapFn(g.field.fn).body.result == BodyExpr.UnaryOp("-", BodyExpr.Path("x", Nil)))
      val h = onlyField("data D { @default(fn[self: Self]() { !(!a) }) x: str }")
      assert(unwrapFn(h.field.fn).body.result ==
        BodyExpr.UnaryOp("!", BodyExpr.UnaryOp("!", BodyExpr.Path("a", Nil))))
      val k = onlyField("data D { @default(fn[self: Self]() { -(-a) }) x: str }")
      assert(unwrapFn(k.field.fn).body.result ==
        BodyExpr.UnaryOp("-", BodyExpr.UnaryOp("-", BodyExpr.Path("a", Nil))))
    }

    "parse nested if/else" in {
      val f = onlyField("data D { @default(fn[self: Self]() { if a { if b { 1 } else { 2 } } else { 3 } }) x: str }")
      val fn = unwrapFn(f.field.fn)
      assert(fn.body.result == BodyExpr.If(
        BodyExpr.Path("a", Nil),
        BodyExpr.If(
          BodyExpr.Path("b", Nil),
          BodyExpr.Lit(BodyLiteral.IntLit(1)),
          BodyExpr.Lit(BodyLiteral.IntLit(2)),
        ),
        BodyExpr.Lit(BodyLiteral.IntLit(3)),
      ))
    }

    "parse every literal kind, escapes and == on strings" in {
      def resultOf(body: String): BodyExpr = {
        val f = onlyField(s"data D { @default(fn[self: Self]() { $body }) x: str }")
        unwrapFn(f.field.fn).body.result
      }
      assert(resultOf("42") == BodyExpr.Lit(BodyLiteral.IntLit(42)))
      assert(resultOf("3.14") == BodyExpr.Lit(BodyLiteral.FloatLit(3.14)))
      assert(resultOf("true") == BodyExpr.Lit(BodyLiteral.BoolLit(true)))
      assert(resultOf("false") == BodyExpr.Lit(BodyLiteral.BoolLit(false)))
      assert(resultOf("null") == BodyExpr.Lit(BodyLiteral.NullLit))
      assert(resultOf("\"hi\"") == BodyExpr.Lit(BodyLiteral.StrLit("hi")))
      // escaped newline / quote must survive the string literal parser
      resultOf("\"a\\nb\"") match {
        case BodyExpr.Lit(BodyLiteral.StrLit(v)) => assert(v.contains("a") && v.contains("b"))
        case other => fail(s"expected StrLit, got $other")
      }
      resultOf("\"q\\\"q\"") match {
        case BodyExpr.Lit(BodyLiteral.StrLit(v)) => assert(v.contains("q"))
        case other => fail(s"expected StrLit, got $other")
      }
      assert(resultOf("\"a\" == \"b\"") == BodyExpr.BinOp(
        "==",
        BodyExpr.Lit(BodyLiteral.StrLit("a")),
        BodyExpr.Lit(BodyLiteral.StrLit("b")),
      ))
    }

    "parse every method-whitelist entry" in {
      def callOf(body: String): BodyExpr.Call = {
        val f = onlyField(s"data D { @default(fn[s: S]() { $body }) x: str }")
        unwrapFn(f.field.fn).body.result match {
          case c: BodyExpr.Call => c
          case other => fail(s"expected Call for [$body], got $other")
        }
      }
      List("uppercase", "lowercase", "capitalize", "trim", "toString", "length").foreach { m =>
        val c = callOf(s"s.$m()")
        assert(c.method == m && c.args.isEmpty, s"no-arg call $m parsed as $c")
      }
      List("split", "join", "contains", "startsWith", "endsWith").foreach { m =>
        val c = callOf(s"s.$m(\"x\")")
        assert(c.method == m && c.args.size == 1, s"one-arg call $m parsed as $c")
      }
      val concat = callOf("s.concat(t)")
      assert(concat.method == "concat" && concat.args == List(BodyExpr.Path("t", Nil)))
      List("abs", "floor", "ceil", "round").foreach { m =>
        val c = callOf(s"n.$m()")
        assert(c.method == m && c.args.isEmpty, s"numeric call $m parsed as $c")
      }
      List("min", "max").foreach { m =>
        val c = callOf(s"a.$m(b)")
        assert(c.method == m && c.args == List(BodyExpr.Path("b", Nil)), s"numeric call $m parsed as $c")
      }
    }

    "parse dotted field chains" in {
      val f = onlyField("data D { @default(fn[self: Self]() { a.b.c.d }) x: str }")
      val fn = unwrapFn(f.field.fn)
      assert(fn.body.result == BodyExpr.Path("a", List(
        BodyExpr.Segment.Field("b"),
        BodyExpr.Segment.Field("c"),
        BodyExpr.Segment.Field("d"),
      )))
    }

    "parse index segments with int, var and expr bodies" in {
      val i = onlyField("data D { @default(fn[self: Self]() { a[0] }) x: str }")
      assert(unwrapFn(i.field.fn).body.result ==
        BodyExpr.Path("a", List(BodyExpr.Segment.Index(BodyExpr.Lit(BodyLiteral.IntLit(0))))))
      val v = onlyField("data D { @default(fn[self: Self]() { a[idx] }) x: str }")
      assert(unwrapFn(v.field.fn).body.result ==
        BodyExpr.Path("a", List(BodyExpr.Segment.Index(BodyExpr.Path("idx", Nil)))))
      val e = onlyField("data D { @default(fn[self: Self]() { a[idx + 1] }) x: str }")
      assert(unwrapFn(e.field.fn).body.result == BodyExpr.Path("a", List(
        BodyExpr.Segment.Index(BodyExpr.BinOp(
          "+",
          BodyExpr.Path("idx", Nil),
          BodyExpr.Lit(BodyLiteral.IntLit(1)),
        )),
      )))
    }

    "parse mixed a.b[0].c and method call after index" in {
      val f = onlyField("data D { @default(fn[self: Self]() { a.b[0].c }) x: str }")
      assert(unwrapFn(f.field.fn).body.result == BodyExpr.Path("a", List(
        BodyExpr.Segment.Field("b"),
        BodyExpr.Segment.Index(BodyExpr.Lit(BodyLiteral.IntLit(0))),
        BodyExpr.Segment.Field("c"),
      )))
      val g = onlyField("data D { @default(fn[self: Self]() { a[0].trim() }) x: str }")
      assert(unwrapFn(g.field.fn).body.result == BodyExpr.Call(
        BodyExpr.Path("a", List(BodyExpr.Segment.Index(BodyExpr.Lit(BodyLiteral.IntLit(0))))),
        "trim",
        Nil,
      ))
    }

    "parse empty, params-only and captures-only fn headers" in {
      val e = onlyField("data D { @default(fn() { 1 }) x: str }")
      val efn = unwrapFn(e.field.fn)
      assert(efn.captures.isEmpty && efn.params.isEmpty)
      assert(efn.body.result == BodyExpr.Lit(BodyLiteral.IntLit(1)))
      val p = onlyField("data D { @default(fn(self) { self.x }) x: str }")
      val pfn = unwrapFn(p.field.fn)
      assert(pfn.captures.isEmpty && pfn.params.size == 1)
      assert(pfn.params.head.name == "self")
      val c = onlyField("data D { @default(fn[q: Q]() { q }) x: str }")
      val cfn = unwrapFn(c.field.fn)
      assert(cfn.captures.size == 1 && cfn.params.isEmpty)
      assert(cfn.captures.head.name == "q")
    }

    "parse rootType with type args" in {
      val f = onlyField("data D { @default(fn[b: Box[T]]() { b }) x: str }")
      unwrapFn(f.field.fn).captures.head.rootType match {
        case RawTypeRef.Constructor(name, args, _) =>
          assert(name.name == "Box")
          assert(args.toList == List(RawTypeRef.Simple(RawTypeName("T"), Nil)))
        case other => fail(s"expected Constructor Box[T], got $other")
      }
      val g = onlyField("data D { @default(fn[m: Map[K, V]]() { m }) x: str }")
      unwrapFn(g.field.fn).captures.head.rootType match {
        case RawTypeRef.Constructor(name, args, _) =>
          assert(name.name == "Map")
          assert(args.toList.size == 2)
        case other => fail(s"expected Constructor Map[K, V], got $other")
      }
    }

    "parse multi-segment locator with multiple keys" in {
      val f = onlyField("data D { @default(fn[p: Parent.a.b[$k1].c[$k2].d]() { p }) x: str }")
      val fn = unwrapFn(f.field.fn)
      assert(fn.captures.head.path == List(
        RawCaptureStep.Single(RawLocatorSeg.Field("a")),
        RawCaptureStep.Single(RawLocatorSeg.Field("b")),
        RawCaptureStep.Single(RawLocatorSeg.Key("k1")),
        RawCaptureStep.Single(RawLocatorSeg.Field("c")),
        RawCaptureStep.Single(RawLocatorSeg.Key("k2")),
        RawCaptureStep.Single(RawLocatorSeg.Field("d")),
      ))
    }

    "parse multi-step star captures with chainName variations" in {
      val f = onlyField("data D { @default(fn[s: MyStruct($items* : .groups[$g].members[$m])]() { s }) x: str }")
      assert(unwrapFn(f.field.fn).captures.head.path == List(
        RawCaptureStep.Star(RawStarCapture("items", List(
          RawLocatorSeg.Field("groups"),
          RawLocatorSeg.Key("g"),
          RawLocatorSeg.Field("members"),
          RawLocatorSeg.Key("m"),
        ))),
      ))
      val g = onlyField("data D { @default(fn[s: MyStruct($row* : .objs[$idx])]() { s }) x: str }")
      assert(unwrapFn(g.field.fn).captures.head.path == List(
        RawCaptureStep.Star(RawStarCapture("row", List(
          RawLocatorSeg.Field("objs"),
          RawLocatorSeg.Key("idx"),
        ))),
      ))
    }

    "parse @default literal, lone @normalize and @migrate arities" in {
      val lit = parseDto("data D { @default(\"lit\") a: str\n @default(42) b: i32 }")
      assert(lit.members.size == 2)
      lit.members.head match {
        case RawDtoMember.FieldDef(field, _) =>
          assert(field.default.contains(RawDefaultValue.StrLit("lit")))
        case other => fail(s"expected FieldDef, got $other")
      }
      lit.members.tail.head match {
        case RawDtoMember.FieldDef(field, _) =>
          assert(field.default.contains(RawDefaultValue.IntLit(42, "42")))
        case other => fail(s"expected FieldDef, got $other")
      }
      val n = onlyField("data D { @normalize(fn[t: T](self) { self }) x: str }")
      assert(n.field.normalize.isDefined)
      assert(n.field.default.isEmpty)
      val zero = onlyField("data D { @migrate(fn[old: S]() { old }, []) x: str }")
      assert(zero.field.migrate.getOrElse(fail("expected migrate")).sources.isEmpty)
      val one = onlyField("data D { @migrate(fn[old: S]() { old }, [S.field]) x: str }")
      assert(one.field.migrate.getOrElse(fail("expected migrate")).sources == List("S.field"))
      val many = onlyField("data D { @migrate(fn[old: S]() { old }, [\n S.a,\n S.b.c,\n S.d\n ]) x: str }")
      assert(many.field.migrate.getOrElse(fail("expected migrate")).sources == List("S.a", "S.b.c", "S.d"))
    }

    "parse @normalize coexisting with an = fn property" in {
      val p = onlyProperty("data D { @normalize(fn[t: T](self) { self }) area: f32 = fn(self) { self.width } }")
      assert(p.field.normalize.isDefined)
      val fn = p.fn
      assert(fn.params.size == 1)
      assert(fn.body.result == BodyExpr.Path("self", List(BodyExpr.Segment.Field("width"))))
    }

    "parse multiline fn bodies and let shadowing" in {
      val f = onlyField("data D {\n @default(fn[p: Parent]() {\n let a = p.name;\n a.uppercase()\n }) x: str\n }")
      val fn = unwrapFn(f.field.fn)
      assert(letsOf(fn).size == 1)
      assert(fn.body.result == BodyExpr.Call(
        BodyExpr.Path("a", Nil),
        "uppercase",
        Nil,
      ))
      val s = onlyField("data D { @default(fn[self: Self]() { let x = 1; let x = 2; x }) x: str }")
      val sfn = unwrapFn(s.field.fn)
      assert(letsOf(sfn) == List(
        ("x", BodyExpr.Lit(BodyLiteral.IntLit(1))),
        ("x", BodyExpr.Lit(BodyLiteral.IntLit(2))),
      ))
      assert(sfn.body.result == BodyExpr.Path("x", Nil))
    }

    "parse suffix //! docs after an fn field" in {
      val f = onlyField("data D { @default(fn[a: A]() { a }) x: str //! some docs\n }")
      assert(f.field.name == RawFieldName("x"))
      assertSimpleType(f.field.tpe, "str")
    }

    "reject if without else and binop conditions" in {
      expectParseFailure("data D { @default(fn[self: Self]() { if a { 1 } }) x: str }")
      expectParseFailure("data D { @default(fn[self: Self]() { if a == b { 1 } else { 2 } }) x: str }")
    }

    "reject let without semicolon and trailing semicolon on result" in {
      expectParseFailure("data D { @default(fn[self: Self]() { let a = 1 a }) x: str }")
      expectParseFailure("data D { @default(fn[self: Self]() { 1; }) x: str }")
      expectParseFailure("data D { @default(fn[self: Self]() { let a = 1;; a }) x: str }")
    }

    "reject unmatched delimiters, unclosed fn[ and bad captures" in {
      expectParseFailure("data D { @default(fn[a: A]() { (a }) x: str }")
      expectParseFailure("data D { @default(fn[a: A]() { a } x: str }")
      expectParseFailure("data D { @default(fn[a: A]() { a[0 }) x: str }")
      expectParseFailure("data D { @default(fn[a: A(x: X) { x }) y: str }")
      expectParseFailure("data D { @default(fn[s: T($* : .a)]() { s }) x: str }")
      expectParseFailure("data D { @default(fn[: T]() { 1 }) x: str }")
      expectParseFailure("data D { @default(fn[s: T($c* :)]() { s }) x: str }")
    }

    "reject single = typo and bare double unary" in {
      expectParseFailure("data D { @default(fn[self: Self]() { a = b }) x: str }")
      expectParseFailure("data D { @default(fn[self: Self]() { !!a }) x: str }")
    }

    "parse two stars separated by a field, preserving order" in {
      val f = onlyField("data D { @default(fn[a: T($c1* : .x).b($c2* : .y)]() { a }) x: str }")
      val fn = unwrapFn(f.field.fn)
      assert(fn.captures.head.path == List(
        RawCaptureStep.Star(RawStarCapture("c1", List(RawLocatorSeg.Field("x")))),
        RawCaptureStep.Single(RawLocatorSeg.Field("b")),
        RawCaptureStep.Star(RawStarCapture("c2", List(RawLocatorSeg.Field("y")))),
      ))
    }

    "parse leading field then star then trailing key locator" in {
      val f = onlyField("data D { @default(fn[a: T.root($c* : .items[$i]).tail]() { a }) x: str }")
      val fn = unwrapFn(f.field.fn)
      assert(fn.captures.head.path == List(
        RawCaptureStep.Single(RawLocatorSeg.Field("root")),
        RawCaptureStep.Star(RawStarCapture("c", List(
          RawLocatorSeg.Field("items"),
          RawLocatorSeg.Key("i"),
        ))),
        RawCaptureStep.Single(RawLocatorSeg.Field("tail")),
      ))
    }

    "parse canonical opt-transparent situating without .Some" in {
      // Canonical opt/set-transparent situating: Child.data inside MyStruct.member —
      // collections are transparent, so traversal is ordinary .field steps.
      val f = onlyField("data D { @default(fn[s: MyStruct.member.data]() { s }) x: str }")
      val fn = unwrapFn(f.field.fn)
      assert(fn.captures.head.path == List(
        RawCaptureStep.Single(RawLocatorSeg.Field("member")),
        RawCaptureStep.Single(RawLocatorSeg.Field("data")),
      ))
    }

    "parse explicit .Some as Field (allowed but non-canonical)" in {
      // Allowed but non-canonical: .Some is implied when descending into the opt
      // payload, so it parses as Field("Some") at AST level (never Variant).
      val f = onlyField("data D { @default(fn[s: MyStruct.member.Some.data]() { s }) x: str }")
      val fn = unwrapFn(f.field.fn)
      assert(fn.captures.head.path == List(
        RawCaptureStep.Single(RawLocatorSeg.Field("member")),
        RawCaptureStep.Single(RawLocatorSeg.Field("Some")),
        RawCaptureStep.Single(RawLocatorSeg.Field("data")),
      ))
    }

    "parse instance-style key through collection member" in {
      // Instance-style, not required for schema situating: [$i] keys still parse.
      val f = onlyField("data D { @default(fn[s: MyStruct.member[$i].data]() { s }) x: str }")
      val fn = unwrapFn(f.field.fn)
      assert(fn.captures.head.path == List(
        RawCaptureStep.Single(RawLocatorSeg.Field("member")),
        RawCaptureStep.Single(RawLocatorSeg.Key("i")),
        RawCaptureStep.Single(RawLocatorSeg.Field("data")),
      ))
    }

    "parse star through collection with trailing field" in {
      // Star expansion through a transparent collection: members iterated, .data situated.
      val f = onlyField("data D { @default(fn[s: MyStruct.members($c* : .data)]() { s }) x: str }")
      val fn = unwrapFn(f.field.fn)
      assert(fn.captures.head.path == List(
        RawCaptureStep.Single(RawLocatorSeg.Field("members")),
        RawCaptureStep.Star(RawStarCapture("c", List(RawLocatorSeg.Field("data")))),
      ))
    }

    "parse assignment to an indexed map target" in {
      val f = onlyField("data D { @default(fn[self: Self]() { self.map[key] = value; value }) x: str }")
      val fn = unwrapFn(f.field.fn)
      assert(fn.body.stmts.size == 1)
      fn.body.stmts.head match {
        case BodyStmt.Effect(BodyExpr.Assign(target, value)) =>
          assert(target == BodyExpr.Path("self", List(
            BodyExpr.Segment.Field("map"),
            BodyExpr.Segment.Index(BodyExpr.Path("key", Nil)),
          )))
          assert(value == BodyExpr.Path("value", Nil))
        case other => fail(s"expected assign effect, got $other")
      }
      assert(fn.body.result == BodyExpr.Path("value", Nil))
    }

    "parse assignment to a plain variable" in {
      val f = onlyField("data D { @default(fn[self: Self]() { let x = 1; x = 2; x }) x: str }")
      val fn = unwrapFn(f.field.fn)
      assert(fn.body.stmts.size == 2)
      fn.body.stmts.tail.head match {
        case BodyStmt.Effect(BodyExpr.Assign(target, value)) =>
          assert(target == BodyExpr.Path("x", Nil))
          assert(value == BodyExpr.Lit(BodyLiteral.IntLit(2)))
        case other => fail(s"expected assign effect, got $other")
      }
      assert(fn.body.result == BodyExpr.Path("x", Nil))
    }

    "parse let with a type annotation" in {
      val f = onlyField("data D { @default(fn[self: Self]() { let x: u32 = 42; x }) x: str }")
      val fn = unwrapFn(f.field.fn)
      assert(fn.body.stmts == List(BodyStmt.Let(
        "x",
        Some(RawTypeRef.Simple(RawTypeName("u32"), Nil)),
        BodyExpr.Lit(BodyLiteral.IntLit(42)),
      )))
      // legacy type-erased view still exposes the binding
      assert(letsOf(fn) == List(("x", BodyExpr.Lit(BodyLiteral.IntLit(42)))))
      assert(fn.body.result == BodyExpr.Path("x", Nil))
    }

    "parse for-in loop over a map" in {
      val f = onlyField("data D { @default(fn[self: Self]() { for (key in self.map) { key } key }) x: str }")
      val fn = unwrapFn(f.field.fn)
      assert(fn.body.stmts.size == 1)
      fn.body.stmts.head match {
        case BodyStmt.Effect(BodyExpr.ForIn(varName, iterable, body)) =>
          assert(varName == "key")
          assert(iterable == BodyExpr.Path("self", List(BodyExpr.Segment.Field("map"))))
          assert(body == BodyExpr.Block(Nil, Some(BodyExpr.Path("key", Nil))))
        case other => fail(s"expected for-in, got $other")
      }
      assert(fn.body.result == BodyExpr.Path("key", Nil))
    }

    "parse C-style for loop with += step" in {
      val f = onlyField("data D { @default(fn[self: Self]() { for (i: u32 = 0; i < 10; i += 1) { i } i }) x: str }")
      val fn = unwrapFn(f.field.fn)
      assert(fn.body.stmts.size == 1)
      fn.body.stmts.head match {
        case BodyStmt.Effect(BodyExpr.ForC(init, cond, step, body)) =>
          assert(init.name == "i")
          assert(init.tpe == Some(RawTypeRef.Simple(RawTypeName("u32"), Nil)))
          assert(init.value == BodyExpr.Lit(BodyLiteral.IntLit(0)))
          assert(cond == BodyExpr.BinOp("<", BodyExpr.Path("i", Nil), BodyExpr.Lit(BodyLiteral.IntLit(10))))
          assert(step == BodyExpr.Assign(
            BodyExpr.Path("i", Nil),
            BodyExpr.BinOp("+", BodyExpr.Path("i", Nil), BodyExpr.Lit(BodyLiteral.IntLit(1))),
          ))
          assert(body == BodyExpr.Block(Nil, Some(BodyExpr.Path("i", Nil))))
        case other => fail(s"expected C-style for, got $other")
      }
    }

    "parse C-style for loop with assign step and untyped init" in {
      val f = onlyField("data D { @default(fn[self: Self]() { for (i = 0; i < 10; i = i + 1) { i } i }) x: str }")
      val fn = unwrapFn(f.field.fn)
      fn.body.stmts.head match {
        case BodyStmt.Effect(BodyExpr.ForC(init, cond, step, _)) =>
          assert(init.name == "i")
          assert(init.tpe.isEmpty)
          assert(init.value == BodyExpr.Lit(BodyLiteral.IntLit(0)))
          assert(cond == BodyExpr.BinOp("<", BodyExpr.Path("i", Nil), BodyExpr.Lit(BodyLiteral.IntLit(10))))
          assert(step == BodyExpr.Assign(
            BodyExpr.Path("i", Nil),
            BodyExpr.BinOp("+", BodyExpr.Path("i", Nil), BodyExpr.Lit(BodyLiteral.IntLit(1))),
          ))
        case other => fail(s"expected C-style for, got $other")
      }
    }

    "parse Rust-style match with variant and wildcard arms" in {
      val f = onlyField("data D { @default(fn[self: Self]() { match (v) { Some(x) => { x } _ => { 0 } } v }) x: str }")
      val fn = unwrapFn(f.field.fn)
      assert(fn.body.stmts.size == 1)
      fn.body.stmts.head match {
        case BodyStmt.Effect(BodyExpr.Match(scrutinee, arms)) =>
          assert(scrutinee == BodyExpr.Path("v", Nil))
          assert(arms.size == 2)
          assert(arms.head.pattern == MatchPattern.Variant("Some", Some("x")))
          assert(arms.head.body == BodyExpr.Block(Nil, Some(BodyExpr.Path("x", Nil))))
          assert(arms.tail.head.pattern == MatchPattern.Wildcard)
          assert(arms.tail.head.body == BodyExpr.Block(Nil, Some(BodyExpr.Lit(BodyLiteral.IntLit(0)))))
        case other => fail(s"expected match, got $other")
      }
      assert(fn.body.result == BodyExpr.Path("v", Nil))
    }

    "parse match with a bare variant pattern" in {
      val f = onlyField("data D { @default(fn[self: Self]() { match (self.opt) { None => { 0 } _ => { 1 } } self }) x: str }")
      val fn = unwrapFn(f.field.fn)
      fn.body.stmts.head match {
        case BodyStmt.Effect(BodyExpr.Match(scrutinee, arms)) =>
          assert(scrutinee == BodyExpr.Path("self", List(BodyExpr.Segment.Field("opt"))))
          assert(arms.head.pattern == MatchPattern.Variant("None", None))
          assert(arms.tail.head.pattern == MatchPattern.Wildcard)
        case other => fail(s"expected match, got $other")
      }
    }

    "parse loop body with statements and no trailing value" in {
      val f = onlyField("data D { @default(fn[self: Self]() { for (k in self.map) { self.map[k] = k; } self }) x: str }")
      val fn = unwrapFn(f.field.fn)
      fn.body.stmts.head match {
        case BodyStmt.Effect(BodyExpr.ForIn(varName, _, body)) =>
          assert(varName == "k")
          assert(body.value.isEmpty)
          assert(body.stmts.size == 1)
          body.stmts.head match {
            case BodyStmt.Effect(BodyExpr.Assign(target, value)) =>
              assert(target == BodyExpr.Path("self", List(
                BodyExpr.Segment.Field("map"),
                BodyExpr.Segment.Index(BodyExpr.Path("k", Nil)),
              )))
              assert(value == BodyExpr.Path("k", Nil))
            case other => fail(s"expected assign in loop body, got $other")
          }
        case other => fail(s"expected for-in, got $other")
      }
    }

    "parse match arm bodies with statements before the value" in {
      val f = onlyField("data D { @default(fn[self: Self]() { match (v) { Some(x) => { let y = x; y } _ => { 0 } } v }) x: str }")
      val fn = unwrapFn(f.field.fn)
      fn.body.stmts.head match {
        case BodyStmt.Effect(BodyExpr.Match(_, arms)) =>
          assert(arms.head.body == BodyExpr.Block(
            List(BodyStmt.Let("y", None, BodyExpr.Path("x", Nil))),
            Some(BodyExpr.Path("y", Nil)),
          ))
        case other => fail(s"expected match, got $other")
      }
    }

    "parse mixed lets, assigns, loops and match in one body" in {
      val f = onlyField("data D { @default(fn[self: Self]() { let n: u32 = 0; n = 1; for (k in self.map) { k } match (n) { _ => { n } } n }) x: str }")
      val fn = unwrapFn(f.field.fn)
      assert(fn.body.stmts.size == 4)
      assert(letsOf(fn) == List(("n", BodyExpr.Lit(BodyLiteral.IntLit(0)))))
      assert(fn.body.stmts.head.isInstanceOf[BodyStmt.Let])
      assert(fn.body.stmts(1).isInstanceOf[BodyStmt.Effect])
      assert(fn.body.result == BodyExpr.Path("n", Nil))
    }

    "reject missing semicolon on assign and typed let" in {
      expectParseFailure("data D { @default(fn[self: Self]() { self.map[key] = value value }) x: str }")
      expectParseFailure("data D { @default(fn[self: Self]() { let x: u32 = 1 x }) x: str }")
    }

    "reject bad for headers" in {
      expectParseFailure("data D { @default(fn[self: Self]() { for (key self.map) { key } key }) x: str }")
      expectParseFailure("data D { @default(fn[self: Self]() { for key in self.map { key } key }) x: str }")
    }

    "reject match without arms" in {
      expectParseFailure("data D { @default(fn[self: Self]() { match (v) { } v }) x: str }")
    }

    "reject single = in for condition" in {
      expectParseFailure("data D { @default(fn[self: Self]() { for (i: u32 = 0; i = 10; i += 1) { i } i }) x: str }")
    }

    "parse assert without message" in {
      val f = onlyField("data D { @default(fn[self: Self]() { assert(self.flag); self }) x: str }")
      val fn = unwrapFn(f.field.fn)
      assert(fn.body.stmts == List(BodyStmt.Assert(
        BodyExpr.Path("self", List(BodyExpr.Segment.Field("flag"))),
        None,
      )))
    }

    "parse assert with message" in {
      val f = onlyField("data D { @default(fn[self: Self]() { assert(self.count > 0, \"must be positive\"); self }) x: str }")
      val fn = unwrapFn(f.field.fn)
      assert(fn.body.stmts == List(BodyStmt.Assert(
        BodyExpr.BinOp(">", BodyExpr.Path("self", List(BodyExpr.Segment.Field("count"))), BodyExpr.Lit(BodyLiteral.IntLit(0))),
        Some(BodyExpr.Lit(BodyLiteral.StrLit("must be positive"))),
      )))
    }

    "parse assert inside for body" in {
      val f = onlyField("data D { @default(fn[self: Self]() { for (k in self.map) { assert(k != \"\"); } self }) x: str }")
      val fn = unwrapFn(f.field.fn)
      fn.body.stmts.head match {
        case BodyStmt.Effect(BodyExpr.ForIn(_, _, body)) =>
          assert(body.stmts == List(BodyStmt.Assert(
            BodyExpr.BinOp("!=", BodyExpr.Path("k", Nil), BodyExpr.Lit(BodyLiteral.StrLit(""))),
            None,
          )))
        case other => fail(s"expected for-in, got $other")
      }
    }

    "parse assert inside match arm block" in {
      val f = onlyField("data D { @default(fn[self: Self]() { match (v) { Some(x) => { assert(x != \"\"); x } _ => { \"\" } } v }) x: str }")
      val fn = unwrapFn(f.field.fn)
      fn.body.stmts.head match {
        case BodyStmt.Effect(BodyExpr.Match(_, arms)) =>
          assert(arms.head.body.stmts == List(BodyStmt.Assert(
            BodyExpr.BinOp("!=", BodyExpr.Path("x", Nil), BodyExpr.Lit(BodyLiteral.StrLit(""))),
            None,
          )))
        case other => fail(s"expected match, got $other")
      }
    }

    "reject assert without semicolon" in {
      expectParseFailure("data D { @default(fn[self: Self]() { assert(self.flag) self }) x: str }")
    }

  }

  "node-level @capture" should {

    "parse @capture(s: Shop.basket.items[$k]) data Child" in {
      val dto = parseDto("@capture(s: Shop.basket.items[$k]) data Child { count: i32 }")
      val cap = dto.capture.getOrElse(fail("expected node @capture"))
      assert(cap.name == "s")
      assertSimpleType(cap.rootType, "Shop")
      assert(
        cap.path == List(
          RawCaptureStep.Single(RawLocatorSeg.Field("basket")),
          RawCaptureStep.Single(RawLocatorSeg.Field("items")),
          RawCaptureStep.Single(RawLocatorSeg.Key("k")),
        ),
        s"unexpected node capture path: ${cap.path}",
      )
    }

    "parse a node without @capture as capture-less" in {
      val dto = parseDto("data Child { count: i32 }")
      assert(dto.capture.isEmpty, s"expected no node capture, got ${dto.capture}")
    }

  }
}
