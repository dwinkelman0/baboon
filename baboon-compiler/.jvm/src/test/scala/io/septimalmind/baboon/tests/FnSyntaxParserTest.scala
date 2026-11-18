package io.septimalmind.baboon.tests

import fastparse.*
import io.septimalmind.baboon.parser.ParserContext
import io.septimalmind.baboon.parser.model.{BodyExpr, BodyLiteral, FSPath, RawDefaultValue, RawDto, RawDtoMember, RawFieldName, RawFn, RawLocatorSeg, RawMigrate, RawTypeName, RawTypeRef}
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

  private def unwrapFn(default: Option[RawDefaultValue]): RawFn = {
    default match {
      case Some(RawDefaultValue.Fn(fn)) => fn
      case other => fail(s"expected RawDefaultValue.Fn, got $other")
    }
  }

  private def assertSimpleType(tpe: RawTypeRef, name: String): Unit = {
    assert(tpe == RawTypeRef.Simple(RawTypeName(name), Nil), s"expected simple type $name, got $tpe")
  }

  "context-function syntax" should {

    "parse @default(fn[p: Parent]() { p.name })" in {
      val f = onlyField("data D { @default(fn[p: Parent]() { p.name }) x: str }")
      val fn = unwrapFn(f.field.default)
      assert(fn.captures.size == 1)
      assert(fn.captures.head.name == "p")
      assertSimpleType(fn.captures.head.rootType, "Parent")
      assert(fn.params.isEmpty)
      assert(fn.body.letBindings.isEmpty)
      assert(fn.body.result == BodyExpr.Path("p", List(BodyExpr.Segment.Field("name"))))
    }

    "parse @default(fn[p: Parent](self: Self) { self.x })" in {
      val f = onlyField("data D { @default(fn[p: Parent](self: Self) { self.x }) x: str }")
      val fn = unwrapFn(f.field.default)
      assert(fn.captures.size == 1)
      assert(fn.captures.head.name == "p")
      assert(fn.params.size == 1)
      assert(fn.params.head.name == "self")
      assertSimpleType(fn.params.head.tpe, "Self")
      assert(fn.body.result == BodyExpr.Path("self", List(BodyExpr.Segment.Field("x"))))
    }

    "parse @normalize(fn[t: Parent](self: Self) { self.trim() })" in {
      val f = onlyField("data D { @normalize(fn[t: Parent](self: Self) { self.trim() }) x: str }")
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
      val p = onlyProperty("data D { area: f32 = fn(self: Self) { self.width * self.height } }")
      val fn = unwrapFn(p.field.default)
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

    "parse a body with let bindings and method call" in {
      val f = onlyField("data D { @default(fn[self: Self]() { let a = self.x; let b = a.uppercase(); b }) x: str }")
      val fn = unwrapFn(f.field.default)
      assert(fn.body.letBindings.size == 2)
      assert(fn.body.letBindings.head == ("a", BodyExpr.Path("self", List(BodyExpr.Segment.Field("x")))))
      assert(fn.body.letBindings.tail.head == ("b", BodyExpr.Call(
        BodyExpr.Path("a", Nil),
        "uppercase",
        Nil,
      )))
      assert(fn.body.result == BodyExpr.Path("b", Nil))
    }

    "parse a star capture" in {
      val f = onlyField("data D { @default(fn[s: MyStruct($chain* : .objs[$idx])]() { s }) x: str }")
      val fn = unwrapFn(f.field.default)
      assert(fn.captures.size == 1)
      val cap = fn.captures.head
      assert(cap.name == "s")
      val star = cap.star.getOrElse(fail("expected star capture"))
      assert(star.chainName == "chain")
      assert(star.steps == List(
        RawLocatorSeg.Field("objs"),
        RawLocatorSeg.Key("idx"),
      ))
    }

    "parse a locator with a key segment" in {
      val f = onlyField("data D { @default(fn[p: Parent.entries[$k].prop]() { p }) x: str }")
      val fn = unwrapFn(f.field.default)
      assert(fn.captures.head.locator == List(
        RawLocatorSeg.Field("entries"),
        RawLocatorSeg.Key("k"),
        RawLocatorSeg.Field("prop"),
      ))
    }

    "parse literal expressions" in {
      val f = onlyField("data D { @default(fn[self: Self]() { 42 + 3.14 == true && !false || null.concat(\"x\") }) x: str }")
      val fn = unwrapFn(f.field.default)
      // Just ensure it parses to a non-trivial expression.
      assert(fn.body.result.isInstanceOf[BodyExpr.BinOp])
    }

    "parse an if/else expression" in {
      val f = onlyField("data D { @default(fn[self: Self]() { if self.x { 1 } else { 2 } }) x: str }")
      val fn = unwrapFn(f.field.default)
      assert(fn.body.result.isInstanceOf[BodyExpr.If])
    }

  }
}
