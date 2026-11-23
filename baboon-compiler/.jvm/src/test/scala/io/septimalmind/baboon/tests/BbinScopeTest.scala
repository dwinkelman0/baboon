package io.septimalmind.baboon.tests

import io.septimalmind.baboon.bbin.model.*
import io.septimalmind.baboon.bbin.parser.BbinImportResolver
import io.septimalmind.baboon.bbin.parser.BbinImportResolver.ResolvedImport
import io.septimalmind.baboon.bbin.typer.{BbinScopeBuilder, ScopeOrigin}
import io.septimalmind.baboon.parser.model.{FSPath, RawTypeName, RawTypeRef}
import izumi.fundamentals.collections.nonempty.NEString
import org.scalatest.wordspec.AnyWordSpec

/** Phase 2 checks for `.bbin` scope + index table ([[BbinScopeBuilder]]):
  * duplicate/shadowing rejection, the workspace-wide index name→type
  * invariant, read-only `for`/`fn` binders, and import passthrough.
  */
final class BbinScopeTest extends AnyWordSpec {

  private val dummyPath = FSPath.parse(NEString.unsafeFrom("scope-test.bbin"))

  private def ref(name: String): RawTypeRef.Simple =
    RawTypeRef.Simple(RawTypeName(name), Nil)

  private def raw(stmts: BbinStmt*): RawBbin =
    RawBbin(dummyPath, List(BbinUsing("p", "1")), Nil, stmts.toList)

  private def letOf(name: String, tpe: BbinType): BbinStmt =
    BbinStmt.LetStmt(BbinLet(name, tpe, BbinExpr.StrLit("x")))

  private def indexed(idx: String, idxType: String, elem: String): BbinType =
    BbinType.IndexedSet(idx, ref(idxType), ref(elem))

  private def resolved(
    name: String,
    tpe: BbinType,
    alias: Option[String] = None,
  ): ResolvedImport =
    ResolvedImport(
      name,
      alias,
      tpe,
      tpe,
      BbinImportResolver.Coercion.Same,
      "rel",
      "t.bbin",
    )

  "BbinScopeBuilder" should {

    "reject duplicate let names" in {
      val (scope, errors) = BbinScopeBuilder.build(
        raw(letOf("a", BbinType.Plain(ref("SoC"))), letOf("a", BbinType.Plain(ref("SoC")))),
        Nil,
      )
      assert(errors.size == 1, s"expected one error, got: $errors")
      assert(errors.head.contains("a"), s"unexpected error: ${errors.head}")
      assert(scope.typeOf("a").contains(BbinType.Plain(ref("SoC"))))
    }

    "reject for binders shadowing outer names" in {
      val loop = BbinStmt.ForStmt(
        BbinFor("s", "row", BbinExpr.Ident("s"), Nil)
      )
      val (_, errors) = BbinScopeBuilder.build(
        raw(letOf("s", indexed("k", "Key", "SoC")), loop),
        Nil,
      )
      assert(errors.size == 1, s"expected one error, got: $errors")
      assert(errors.head.contains("s"), s"unexpected error: ${errors.head}")
    }

    "reject an index name used with two different idx_types" in {
      val (_, errors) = BbinScopeBuilder.build(
        raw(letOf("a", indexed("k", "KeyA", "SoC")), letOf("b", indexed("k", "KeyB", "SoC"))),
        Nil,
      )
      assert(errors.size == 1, s"expected one error, got: $errors")
      assert(errors.head.contains("k"), s"unexpected error: ${errors.head}")
    }

    "accept an index name reused with the same idx_type" in {
      val (scope, errors) = BbinScopeBuilder.build(
        raw(letOf("a", indexed("k", "Key", "SoC")), letOf("b", indexed("k", "Key", "Other"))),
        Nil,
      )
      assert(errors.isEmpty, s"expected no errors, got: $errors")
      assert(scope.indexTypeOf("k").contains("Key"), s"unexpected index table: ${scope.indexTypes}")
    }

    "reject a cross-file index conflict via a resolved import" in {
      val imp = resolved("imp", indexed("k", "KeyA", "SoC"))
      val (_, errors) = BbinScopeBuilder.build(
        raw(letOf("b", indexed("k", "KeyB", "SoC"))),
        List(imp),
      )
      assert(errors.size == 1, s"expected one error, got: $errors")
      assert(errors.head.contains("k"), s"unexpected error: ${errors.head}")
    }

    "mark for binders read-only with types derived from the iterated set" in {
      val loop = BbinStmt.ForStmt(
        BbinFor("i", "r", BbinExpr.Ident("s"), Nil)
      )
      val (scope, errors) = BbinScopeBuilder.build(
        raw(letOf("s", indexed("k", "Key", "SoC")), loop),
        Nil,
      )
      assert(errors.isEmpty, s"expected no errors, got: $errors")
      assert(scope.isReadOnly("i"), "expected for idx binder to be read-only")
      assert(scope.isReadOnly("r"), "expected for row binder to be read-only")
      assert(!scope.isReadOnly("s"), "expected let binding to stay writable")
      assert(scope.typeOf("i").contains(BbinType.Plain(ref("Key"))))
      assert(scope.typeOf("r").contains(BbinType.Plain(ref("SoC"))))
      assert(scope.get("i").map(_.origin).contains(ScopeOrigin.ForBinder))
    }

    "mark fn params read-only and reject shadowing" in {
      val (scope0, errors0) = BbinScopeBuilder.build(
        raw(letOf("a", BbinType.Plain(ref("SoC")))),
        Nil,
      )
      assert(errors0.isEmpty, s"expected no errors, got: $errors0")

      val (scope1, errors1) =
        BbinScopeBuilder.extendReadOnly(scope0, List(("p", BbinType.Plain(ref("SoC")))))
      assert(errors1.isEmpty, s"expected no errors, got: $errors1")
      assert(scope1.isReadOnly("p"), "expected fn param to be read-only")
      assert(scope1.get("p").map(_.origin).contains(ScopeOrigin.FnParam))

      val (_, errors2) =
        BbinScopeBuilder.extendReadOnly(scope0, List(("a", BbinType.Plain(ref("SoC")))))
      assert(errors2.size == 1, s"expected one error, got: $errors2")
      assert(errors2.head.contains("a"), s"unexpected error: ${errors2.head}")
    }

    "land no-ascription imports in scope with the target's type" in {
      val base = resolved("base", BbinType.Plain(ref("SoC")))
      val part = resolved("part", BbinType.Partial(ref("SoC")), alias = Some("q"))
      val (scope, errors) = BbinScopeBuilder.build(raw(), List(base, part))
      assert(errors.isEmpty, s"expected no errors, got: $errors")
      assert(scope.typeOf("base").contains(BbinType.Plain(ref("SoC"))))
      assert(scope.typeOf("q").contains(BbinType.Partial(ref("SoC"))))
      assert(scope.typeOf("part").isEmpty, "expected aliased import under its alias only")
      assert(scope.get("base").map(_.origin).contains(ScopeOrigin.Import))
    }
  }
}
