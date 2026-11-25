package io.septimalmind.baboon.tests

import io.septimalmind.baboon.parser.model._
import io.septimalmind.baboon.typer.{FnBodyTyper, FnCaptureResolver, FnReadGate, FnSiteExtractor}
import io.septimalmind.baboon.typer.model._
import io.septimalmind.baboon.typer.model.FnTypes.{ctor, scalar}
import izumi.fundamentals.collections.nonempty.NEList
import org.scalatest.wordspec.AnyWordSpec

/** Direct unit tests for the P1/P2/P3 fn pipeline passes with a tiny
  * hand-built schema (no loader needed).
  */
final class FnBodyTypingTest extends AnyWordSpec {

  private val i32 = scalar("i32")
  private val str = scalar("str")
  private val bit = scalar("bit")
  private val f64 = scalar("f64")
  private val lst = TypeId.Builtins.lst
  private val set = TypeId.Builtins.set
  private val map = TypeId.Builtins.map
  private val opt = TypeId.Builtins.opt

  private def uid(name: String): TypeId.User =
    TypeId.User(Pkg(NEList.unsafeFrom(List("t"))), Owner.Toplevel, TypeName(name))
  private def uref(name: String): TypeRef = TypeRef.Scalar(uid(name))
  private def field(name: String, tpe: TypeRef): Field = Field(FieldName(name), tpe, None)

  private val itemDto = Typedef.Dto(
    uid("Item"),
    List(field("label", str), field("qty", i32)),
    Nil,
  )
  private val bundleDto = Typedef.Dto(
    uid("Bundle"),
    List(field("sku", str)),
    Nil,
  )
  private val basketDto = Typedef.Dto(
    uid("Basket"),
    List(
      field("items", ctor(lst, List(uref("Item")))),
      field("bundles", ctor(set, List(uref("Bundle")))),
      field("sel", ctor(opt, List(uref("Item")))),
      field("byName", ctor(map, List(str, uref("Item")))),
      field("matrix", ctor(map, List(str, ctor(map, List(str, i32))))),
    ),
    Nil,
  )
  private val childDto = Typedef.Dto(
    uid("Child"),
    List(
      field("data", str),
      field("tags", ctor(lst, List(str))),
      field("scores", ctor(map, List(str, i32))),
      field("maybe", ctor(opt, List(str))),
      field("count", i32),
      field("flag", bit),
    ),
    Nil,
  )
  private val shapeAdt = Typedef.Adt(uid("Shape"), NEList.unsafeFrom(List(uid("Circle"))), Nil, Nil)

  private val schema = FnSchema(
    Map(
      "Item"   -> itemDto,
      "Bundle" -> bundleDto,
      "Basket" -> basketDto,
      "Child"  -> childDto,
      "Shape"  -> shapeAdt,
    ),
    Map("Shape" -> List(FnAdtVariant("Circle", Some(f64)), FnAdtVariant("Dot", None))),
  )

  // -- small AST builders -----------------------------------------------------

  private def rt(name: String): RawTypeRef = RawTypeRef.Simple(RawTypeName(name), Nil)
  private def intLit(v: Long): BodyExpr = BodyExpr.Lit(BodyLiteral.IntLit(v))
  private def strLit(v: String): BodyExpr = BodyExpr.Lit(BodyLiteral.StrLit(v))
  private def path(root: String, segs: BodyExpr.Segment*): BodyExpr = BodyExpr.Path(root, segs.toList)
  private def fld(name: String): BodyExpr.Segment = BodyExpr.Segment.Field(name)
  private def letS(name: String, t: String, v: BodyExpr): BodyStmt = BodyStmt.Let(name, Some(rt(t)), v)
  private def eff(e: BodyExpr): BodyStmt = BodyStmt.Effect(e)

  private def rawBody(stmts: List[BodyStmt], result: BodyExpr): RawBody =
    RawBody(result, stmts)

  private def check(
    stmts: List[BodyStmt],
    result: BodyExpr,
    scope: ScopeTable,
    expected: TypeRef,
  ): Either[List[String], TypedBody] =
    FnBodyTyper.typeBody(rawBody(stmts, result), scope, schema, expected)

  private def childScope(extra: (String, ScopeEntry)*): ScopeTable =
    FnBodyTyper.seedScope(List("self" -> uref("Child")), Nil) ++ extra

  private def mustType(
    stmts: List[BodyStmt],
    result: BodyExpr,
    scope: ScopeTable,
    expected: TypeRef,
  ): TypedBody =
    check(stmts, result, scope, expected) match {
      case Right(b)  => b
      case Left(err) => fail(s"expected success, got errors: ${err.mkString("; ")}")
    }

  /** Display shape of a captured assert path: `self.count`, `a`, `self.tags[_]`. */
  private def showVar(p: TypedExpr.Path): String =
    p.root + p.segments.map {
      case TypedPathSeg.Field(n, _) => s".$n"
      case _: TypedPathSeg.Index   => "[_]"
    }.mkString

  private def mustFail(    stmts: List[BodyStmt],
    result: BodyExpr,
    scope: ScopeTable,
    expected: TypeRef,
  ): List[String] =
    check(stmts, result, scope, expected) match {
      case Left(err) => err
      case Right(b)  => fail(s"expected failure, got typed body with tpe ${b.tpe}")
    }

  "P1 site checks" should {
    def fn(
      caps: List[RawCapture] = List(RawCapture("s", rt("Basket"), Nil)),
      params: List[RawFnParam] = Nil,
      stmts: List[BodyStmt] = Nil,
    ): RawFn = RawFn(caps, params, rawBody(stmts, path("s")))

    "accept a well-formed single-capture fn" in {
      assert(FnSiteExtractor.check(fn()).isRight)
    }

    "reject two captures (defense in depth)" in {
      val two = fn(caps = List(RawCapture("a", rt("Basket"), Nil), RawCapture("b", rt("Basket"), Nil)))
      assert(FnSiteExtractor.check(two).isLeft)
    }

    "reject duplicate param names" in {
      val dup = fn(params = List(RawFnParam("x", rt("i32")), RawFnParam("x", rt("str"))))
      val err = FnSiteExtractor.check(dup)
      assert(err.isLeft && err.left.toOption.get.mkString.contains("duplicate param"))
    }

    "reject a param shadowing the capture name" in {
      val sh = fn(params = List(RawFnParam("s", rt("i32"))))
      assert(FnSiteExtractor.check(sh).isLeft)
    }

    "reject a top-level let shadowing the capture name" in {
      val sh = fn(stmts = List(BodyStmt.Let("s", Some(rt("i32")), intLit(1))))
      assert(FnSiteExtractor.check(sh).isLeft)
    }
  }

  "P2 capture resolution" should {
    def cap(name: String, root: String, steps: RawCaptureStep*): RawCapture =
      RawCapture(name, rt(root), steps.toList)
    def single(seg: RawLocatorSeg): RawCaptureStep = RawCaptureStep.Single(seg)
    def fldS(name: String): RawLocatorSeg = RawLocatorSeg.Field(name)
    def keyS(name: String): RawLocatorSeg = RawLocatorSeg.Key(name)

    "resolve an ordered field+key+star path with endpoint and binders" in {
      val c = cap(
        "b",
        "Basket",
        single(fldS("items")),
        RawCaptureStep.Star(RawStarCapture("c", List(fldS("label")))),
      )
      FnCaptureResolver.resolveCapture(c, schema) match {
        case Right((typed, binds)) =>
          assert(typed.endpoint == str, s"endpoint was ${typed.endpoint}")
          assert(typed.path.size == 2)
          assert(binds.toMap.get("c").contains(ScopeEntry.StarChain(str)))
        case Left(err) => fail(s"expected success: ${err.mkString("; ")}")
      }
    }

    "resolve a key through a map value" in {
      val c = cap("b", "Basket", single(fldS("byName")), single(keyS("k")))
      FnCaptureResolver.resolveCapture(c, schema) match {
        case Right((typed, binds)) =>
          assert(typed.endpoint == uref("Item"))
          assert(typed.path.last == TypedCaptureStep.KeyStep("k", str))
          assert(binds.toMap.get("k").contains(ScopeEntry.KeyVar(str)))
        case Left(err) => fail(s"expected success: ${err.mkString("; ")}")
      }
    }

    "treat opt payload as transparent" in {
      val c = cap("b", "Basket", single(fldS("sel")), single(fldS("label")))
      FnCaptureResolver.resolveCapture(c, schema) match {
        case Right((typed, _)) => assert(typed.endpoint == str)
        case Left(err)         => fail(s"expected success: ${err.mkString("; ")}")
      }
    }

    "canonicalize an explicit .Some away" in {
      val c = cap("b", "Basket", single(fldS("sel")), single(fldS("Some")), single(fldS("label")))
      FnCaptureResolver.resolveCapture(c, schema) match {
        case Right((typed, _)) =>
          assert(typed.endpoint == str)
          assert(!typed.path.exists {
            case TypedCaptureStep.FieldStep("Some", _, _) => true
            case _                                        => false
          })
        case Left(err) => fail(s"expected success: ${err.mkString("; ")}")
      }
    }

    "traverse a set transparently" in {
      val c = cap("b", "Basket", single(fldS("bundles")), single(fldS("sku")))
      FnCaptureResolver.resolveCapture(c, schema) match {
        case Right((typed, _)) => assert(typed.endpoint == str)
        case Left(err)         => fail(s"expected success: ${err.mkString("; ")}")
      }
    }

    "reject an unknown root type" in {
      val c = cap("b", "Nope", single(fldS("x")))
      assert(FnCaptureResolver.resolveCapture(c, schema).isLeft)
    }

    "reject an unknown field" in {
      val c = cap("b", "Basket", single(fldS("nope")))
      assert(FnCaptureResolver.resolveCapture(c, schema).isLeft)
    }

    "reject a variant segment" in {
      val c = cap("b", "Basket", single(RawLocatorSeg.Field("Circle")))
      assert(FnCaptureResolver.resolveCapture(c, schema).isLeft)
    }

    "reject a star rooted in a scalar" in {
      val c = cap(
        "p",
        "Child",
        single(fldS("data")),
        RawCaptureStep.Star(RawStarCapture("c", List(fldS("length")))),
      )
      assert(FnCaptureResolver.resolveCapture(c, schema).isLeft)
    }

    "reject duplicate binders" in {
      val c = cap("b", "Basket", single(fldS("matrix")), single(keyS("k")), single(keyS("k")))
      FnCaptureResolver.resolveCapture(c, schema) match {
        case Left(err) => assert(err.mkString.contains("duplicate capture binder"))
        case Right(_)  => fail("expected duplicate binder failure")
      }
    }

    "situate a capture path ending at the container itself" in {
      val c = cap("s", "Child")
      FnCaptureResolver.resolveCapture(c, schema) match {
        case Right((typed, binds)) =>
          assert(typed.endpoint == uref("Child"))
          assert(typed.path.isEmpty)
          assert(binds.toMap.get("s").contains(ScopeEntry.CaptureRoot(uref("Child"))))
        case Left(err) => fail(s"expected success: ${err.mkString("; ")}")
      }
    }

    "report a star sub-step failure with the same capture+segment text as a single step" in {
      // FnCaptureResolver.scala:75,82,97,103,188-196: star bodies reuse the
      // flat single-step home (resolveSeg), so the message shape matches.
      val singleCap = cap("b", "Basket", single(fldS("nope")))
      val singleMsg = FnCaptureResolver.resolveCapture(singleCap, schema) match {
        case Left(errs) => errs.mkString("; ")
        case Right(_)   => fail("expected single-step failure")
      }
      assert(singleMsg.contains("capture b"), s"single msg missing capture name: $singleMsg")
      assert(singleMsg.contains("`nope`"), s"single msg missing segment: $singleMsg")
      val star = cap(
        "b",
        "Basket",
        single(fldS("items")),
        RawCaptureStep.Star(RawStarCapture("c", List(fldS("nope")))),
      )
      FnCaptureResolver.resolveCapture(star, schema) match {
        case Left(errs) =>
          val msg = errs.mkString("; ")
          assert(msg.contains("capture b"), s"star msg missing capture name: $msg")
          assert(msg.contains("`nope`"), s"star msg missing segment: $msg")
        case Right(_) => fail("expected star sub-step failure")
      }
    }

    "reject a [$k] key on a non-collection" in {
      // Same unknown-style rejection family as unknown fields: key binding
      // requires lst/set/map (FnCaptureResolver.scala:82-94).
      val c = cap("d", "Child", single(fldS("data")), single(keyS("k")))
      FnCaptureResolver.resolveCapture(c, schema) match {
        case Left(errs) =>
          val msg = errs.mkString("; ")
          assert(msg.contains("capture d"), s"missing capture name: $msg")
          assert(msg.contains("non-collection"), s"expected non-collection rejection, got: $msg")
        case Right(_) => fail("expected key-on-scalar failure")
      }
    }
  }

  "P2 SchemaPath coherence" should {
    "reject a mid-path unknown field with its resolve string" in {
      // SchemaPath.scala:16,61: collections/opt transparent, fields resolved
      // per step; mid-path mismatch reports the offending segment.
      SchemaPath.resolve(uid("Child"), List("scores", "nope"), schema) match {
        case Left(msg) => assert(msg.contains("`nope`"), s"expected segment in message, got: $msg")
        case Right(p)  => fail(s"expected mid-path failure, got $p")
      }
    }

    "gate a hand-built unknown path with the same unknown-field text" in {
      // SchemaPath coherence via FnReadGate.scala:34: a hand-built leaf path
      // that never went through resolve is still gated with the identical
      // unknown-field message (FnOrderingTest:176 covers resolve-side only).
      val handBuilt = SchemaPath(
        uid("Child"),
        List(SchemaStep.FieldStep(FieldName("nope"), uid("Child"), str)),
      )
      val body = mustType(Nil, intLit(1), childScope(), i32)
      val site = FnSite(
        FnSiteId("Child", "count", FnRole.Default),
        RawFn(Nil, Nil, rawBody(Nil, intLit(1))),
      )
      val sens = Sensitivity(Set(handBuilt), Set.empty)
      val input = FnReadGate.Input(site, body, sens, Sensitivity.empty, Set.empty)
      FnReadGate.gate(List(input), schema) match {
        case Left(errs) =>
          val msg = errs.mkString("; ")
          assert(msg.contains("`nope`"), s"expected unknown-field text, got: $msg")
        case Right(_) => fail("expected gate to reject hand-built unknown path")
      }
    }
  }

  "P3 body typing (positives)" should {
    "type an int literal result" in {
      val b = mustType(Nil, intLit(1), childScope(), i32)
      assert(b.tpe == i32)
    }

    "type == as bit" in {
      val b = mustType(Nil, BodyExpr.BinOp("==", strLit("a"), strLit("b")), childScope(), bit)
      assert(b.tpe == bit)
    }

    "join 1 + 2.0 to f64" in {
      val b = mustType(
        Nil,
        BodyExpr.BinOp("+", intLit(1), BodyExpr.Lit(BodyLiteral.FloatLit(2.0))),
        childScope(),
        f64,
      )
      assert(b.tpe == f64)
    }

    "type a typed let and use it" in {
      val b = mustType(List(letS("x", "i32", intLit(42))), path("x"), childScope(), i32)
      assert(b.tpe == i32)
    }

    "assign to a field" in {
      val target = path("self", fld("count"))
      val b = mustType(List(eff(BodyExpr.Assign(target, intLit(1)))), path("self", fld("count")), childScope(), i32)
      assert(b.tpe == i32)
    }

    "assign to a map index" in {
      val sc = childScope("k" -> ScopeEntry.Param(str))
      val target = path("self", fld("scores"), BodyExpr.Segment.Index(path("k")))
      val b = mustType(List(eff(BodyExpr.Assign(target, intLit(7)))), path("k"), sc, str)
      assert(b.tpe == str)
    }

    "type a for-in loop over a list" in {
      val sc = childScope("basket" -> ScopeEntry.Param(uref("Basket")))
      val loop = BodyExpr.ForIn(
        "item",
        path("basket", fld("items")),
        BodyExpr.Block(Nil, Some(path("item", fld("label")))),
      )
      val b = mustType(List(eff(loop)), strLit("done"), sc, str)
      assert(b.tpe == str)
    }

    "type a C-style for loop with += step" in {
      val loop = BodyExpr.ForC(
        ForInit("i", Some(rt("i32")), intLit(0)),
        BodyExpr.BinOp("<", path("i"), intLit(10)),
        BodyExpr.Assign(path("i"), BodyExpr.BinOp("+", path("i"), intLit(1))),
        BodyExpr.Block(
          List(eff(BodyExpr.Assign(path("total"), BodyExpr.BinOp("+", path("total"), path("i"))))),
          None,
        ),
      )
      val b = mustType(List(letS("total", "i32", intLit(0)), eff(loop)), path("total"), childScope(), i32)
      assert(b.tpe == i32)
    }

    "type a match with variant binding and wildcard" in {
      val sc = childScope("s" -> ScopeEntry.Param(uref("Shape")))
      val m = BodyExpr.Match(
        path("s"),
        List(
          MatchArm(MatchPattern.Variant("Circle", Some("r")), BodyExpr.Block(Nil, Some(path("r")))),
          MatchArm(MatchPattern.Wildcard, BodyExpr.Block(Nil, Some(BodyExpr.Lit(BodyLiteral.FloatLit(0.0))))),
        ),
      )
      val b = mustType(List(eff(m)), BodyExpr.Lit(BodyLiteral.FloatLit(1.0)), sc, f64)
      assert(b.tpe == f64)
    }

    "type a match on opt Some/None" in {
      val sc = childScope("o" -> ScopeEntry.Param(ctor(opt, List(str))))
      val m = BodyExpr.Match(
        path("o"),
        List(
          MatchArm(MatchPattern.Variant("Some", Some("x")), BodyExpr.Block(Nil, Some(path("x")))),
          MatchArm(MatchPattern.Variant("None", None), BodyExpr.Block(Nil, Some(strLit("")))),
        ),
      )
      val b = mustType(Nil, m, sc, str)
      assert(b.tpe == str)
    }

    "type a block value" in {
      val blk = BodyExpr.Block(List(letS("x", "i32", intLit(3))), Some(BodyExpr.BinOp("+", path("x"), intLit(4))))
      val b = mustType(Nil, blk, childScope(), i32)
      assert(b.tpe == i32)
    }

    "resolve a path through opt-transparent payload" in {
      val sc = childScope("basket" -> ScopeEntry.Param(uref("Basket")))
      val b = mustType(Nil, path("basket", fld("sel"), fld("label")), sc, str)
      assert(b.tpe == str)
    }

    "resolve a path through a set transparently" in {
      val sc = childScope("basket" -> ScopeEntry.Param(uref("Basket")))
      val b = mustType(Nil, path("basket", fld("bundles"), fld("sku")), sc, str)
      assert(b.tpe == str)
    }

    "read sibling fields through a container self" in {
      val b = mustType(Nil, path("self", fld("count")), childScope(), i32)
      assert(b.tpe == i32)
      val b2 = mustType(Nil, path("self", fld("data")), childScope(), str)
      assert(b2.tpe == str)
    }

    "apply field-value ops through a field-typed self" in {
      val sc = FnBodyTyper.seedScope(List("self" -> str), Nil)
      val b = mustType(Nil, BodyExpr.Call(path("self"), "trim", Nil), sc, str)
      assert(b.tpe == str)
    }

    "type string and numeric method calls" in {
      val sc = childScope("n" -> ScopeEntry.Param(i32))
      val up = mustType(Nil, BodyExpr.Call(path("self", fld("data")), "uppercase", Nil), childScope(), str)
      assert(up.tpe == str)
      val mn = mustType(Nil, BodyExpr.Call(path("n"), "min", List(intLit(2))), sc, i32)
      assert(mn.tpe == i32)
    }

    "type if/else with a joined branch" in {
      val b = mustType(
        Nil,
        BodyExpr.If(path("self", fld("flag")), intLit(1), intLit(2)),
        childScope(),
        i32,
      )
      assert(b.tpe == i32)
    }
  }

  "P3 body typing (negatives)" should {
    "reject a let without an annotation" in {
      val errs = mustFail(
        List(BodyStmt.Let("x", None, intLit(1))),
        path("x"),
        childScope(),
        i32,
      )
      assert(errs.mkString.contains("annotation"))
    }

    "reject an unknown path root" in {
      val errs = mustFail(Nil, path("nope"), childScope(), i32)
      assert(errs.mkString.contains("unknown path root"))
    }

    "reject an unknown method" in {
      val errs = mustFail(
        Nil,
        BodyExpr.Call(path("self", fld("data")), "frobnicate", Nil),
        childScope(),
        str,
      )
      assert(errs.mkString.contains("unknown method"))
    }

    "reject method arity mismatch" in {
      val errs = mustFail(
        Nil,
        BodyExpr.Call(path("self", fld("data")), "trim", List(strLit("x"))),
        childScope(),
        str,
      )
      assert(errs.mkString.contains("takes 0 args"))
    }

    "reject a non-bit if condition" in {
      val errs = mustFail(Nil, BodyExpr.If(intLit(1), intLit(2), intLit(3)), childScope(), i32)
      assert(errs.mkString.contains("must be bit"))
    }

    "reject an assign type mismatch" in {
      val errs = mustFail(
        List(letS("x", "str", strLit("a")), eff(BodyExpr.Assign(path("x"), intLit(1)))),
        path("x"),
        childScope(),
        str,
      )
      assert(errs.mkString.contains("assign type mismatch"))
    }

    "reject for-in over a non-collection" in {
      val loop = BodyExpr.ForIn("k", intLit(1), BodyExpr.Block(Nil, Some(path("k"))))
      val errs = mustFail(List(eff(loop)), intLit(0), childScope(), i32)
      assert(errs.mkString.contains("non-collection"))
    }

    "reject a match on an unknown variant" in {
      val sc = childScope("s" -> ScopeEntry.Param(uref("Shape")))
      val m = BodyExpr.Match(
        path("s"),
        List(MatchArm(MatchPattern.Variant("Triangle", None), BodyExpr.Block(Nil, Some(intLit(1))))),
      )
      val errs = mustFail(Nil, m, sc, i32)
      assert(errs.mkString.contains("unknown variant"))
    }

    "reject match arms that disagree" in {
      val sc = childScope("s" -> ScopeEntry.Param(uref("Shape")))
      val m = BodyExpr.Match(
        path("s"),
        List(
          MatchArm(MatchPattern.Variant("Circle", Some("r")), BodyExpr.Block(Nil, Some(path("r")))),
          MatchArm(MatchPattern.Wildcard, BodyExpr.Block(Nil, Some(strLit("x")))),
        ),
      )
      val errs = mustFail(Nil, m, sc, f64)
      assert(errs.mkString.contains("disagree"))
    }

    "reject a result that does not conform to the field type" in {
      val errs = mustFail(Nil, strLit("x"), childScope(), i32)
      assert(errs.mkString.contains("does not conform"))
    }

    "type assert without message and collect vars" in {
      val cond = BodyExpr.BinOp(">", path("self", fld("count")), intLit(0))
      val b = mustType(List(BodyStmt.Assert(cond, None)), path("self", fld("count")), childScope(), i32)
      assert(b.assertions.size == 1)
      assert(b.assertions.head.capturedVars.map(showVar) == List("self.count"))
      assert(b.assertions.head.msg.isEmpty)
      assert(b.assertions.head.trace.isEmpty)
      b.stmts.head match {
        case TypedStmt.Assert(a) =>
          assert(a.cond.tpe == bit)
          assert(FnTypes.isVoid(a.cond.tpe) == false)
        case other => fail(s"expected typed assert, got $other")
      }
    }

    "type assert with message and operator-result == as bool" in {
      val cond = BodyExpr.BinOp("==", path("self", fld("data")), strLit("x"))
      val b = mustType(
        List(BodyStmt.Assert(cond, Some(strLit("bad")))),
        path("self", fld("data")),
        childScope(),
        str,
      )
      assert(b.assertions.size == 1)
      assert(b.assertions.head.capturedVars.map(showVar) == List("self.data"))
      assert(b.assertions.head.msg.map(_.tpe) == Some(str))
    }

    "type assert collecting multiple vars in order" in {
      val sc = childScope("a" -> ScopeEntry.Param(bit), "b" -> ScopeEntry.Param(bit))
      val cond = BodyExpr.BinOp("&&", path("a"), BodyExpr.BinOp("&&", path("b"), path("a")))
      val b = mustType(List(BodyStmt.Assert(cond, None)), intLit(1), sc, i32)
      assert(b.assertions.head.capturedVars.map(showVar) == List("a", "b"))
    }

    "type assert with an indexed path collects the full path and the index vars" in {
      val sc = childScope("i" -> ScopeEntry.Param(i32))
      val cond = BodyExpr.BinOp("!=", path("self", fld("tags"), BodyExpr.Segment.Index(path("i"))), strLit(""))
      val b = mustType(List(BodyStmt.Assert(cond, None)), strLit("x"), sc, str)
      assert(b.assertions.size == 1)
      assert(b.assertions.head.capturedVars.map(showVar) == List("i", "self.tags[_]"))
    }

    "type assert inside a for body and surface it in TypedBody.assertions" in {
      val loop = BodyExpr.ForIn(
        "k",
        path("self", fld("scores")),
        BodyExpr.Block(List(BodyStmt.Assert(BodyExpr.BinOp("!=", path("k"), strLit("")), None)), None),
      )
      val b = mustType(List(eff(loop)), path("self", fld("count")), childScope(), i32)
      assert(b.assertions.size == 1)
      assert(b.assertions.head.capturedVars.map(showVar) == List("k"))
    }

    "type assert inside match arm blocks" in {
      val sc = childScope("o" -> ScopeEntry.Param(ctor(opt, List(str))))
      val m = BodyExpr.Match(
        path("o"),
        List(
          MatchArm(
            MatchPattern.Variant("Some", Some("x")),
            BodyExpr.Block(
              List(BodyStmt.Assert(BodyExpr.BinOp("!=", path("x"), strLit("")), Some(strLit("empty")))),
              Some(path("x")),
            ),
          ),
          MatchArm(MatchPattern.Variant("None", None), BodyExpr.Block(Nil, Some(strLit("")))),
        ),
      )
      val b = mustType(Nil, m, sc, str)
      assert(b.tpe == str)
      // nested asserts are reachable via the typed match arm blocks
      val nested = b.result match {
        case TypedExpr.Match(_, arms, _) =>
          arms.flatMap(a => FnBodyTyper.collectAsserts(a.body.stmts))
        case other => fail(s"expected typed match, got $other")
      }
      assert(nested.size == 1)
      assert(nested.head.capturedVars.map(showVar) == List("x"))
    }

    "loops and valueless blocks carry void" in {
      val loop = BodyExpr.ForIn("k", path("self", fld("tags")), BodyExpr.Block(Nil, Some(path("k"))))
      val sc = childScope()
      FnBodyTyper.typeExpr(loop, sc, schema) match {
        case Right(t: TypedExpr.ForIn) => assert(FnTypes.isVoid(t.tpe))
        case Right(other)               => fail(s"expected ForIn, got $other")
        case Left(err)                  => fail(s"expected success: ${err.mkString("; ")}")
      }
      FnBodyTyper.typeExpr(BodyExpr.Block(Nil, None), sc, schema) match {
        case Right(t: TypedExpr.Block) => assert(FnTypes.isVoid(t.tpe))
        case Right(other)              => fail(s"expected Block, got $other")
        case Left(err)                 => fail(s"expected success: ${err.mkString("; ")}")
      }
    }
  }

  "P3 body typing (assert negatives)" should {
    "reject a non-bool assert condition" in {
      val errs = mustFail(
        List(BodyStmt.Assert(intLit(1), None)),
        intLit(0),
        childScope(),
        i32,
      )
      assert(errs.mkString.contains("assert condition must be bit"))
    }

    "reject a non-str assert message" in {
      val errs = mustFail(
        List(BodyStmt.Assert(BodyExpr.Lit(BodyLiteral.BoolLit(true)), Some(intLit(1)))),
        intLit(0),
        childScope(),
        i32,
      )
      assert(errs.mkString.contains("assert message must be str"))
    }

    "reject an unknown var in assert condition" in {
      val errs = mustFail(
        List(BodyStmt.Assert(path("nope"), None)),
        intLit(0),
        childScope(),
        i32,
      )
      assert(errs.mkString.contains("unknown path root"))
    }

    "reject assert used as trailing value is a parse-level absence (modelled as no Assert expr)" in {
      // `BodyStmt.Assert` is statement-only: there is no `BodyExpr.Assert`, so
      // a trailing `assert(...)` cannot be constructed as a value. Typing the
      // equivalent bool expression as the result must still succeed, proving
      // asserts do not masquerade as values.
      val b = mustType(Nil, BodyExpr.Lit(BodyLiteral.BoolLit(true)), childScope(), bit)
      assert(b.assertions.isEmpty)
      assert(b.tpe == bit)
    }
  }
}
