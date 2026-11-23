package io.septimalmind.baboon.tests

import java.io.File

import fastparse.*
import io.septimalmind.baboon.bbin.model.*
import io.septimalmind.baboon.bbin.parser.{BbinImportResolver, DefBbin}
import io.septimalmind.baboon.parser.ParserContext
import io.septimalmind.baboon.parser.model.{FSPath, RawTypeName, RawTypeRef}
import izumi.fundamentals.collections.nonempty.NEString
import org.scalatest.wordspec.AnyWordSpec

/** AST-level checks for typed `.bbin` imports ([[BbinImportResolver]]):
  * the ascribed type must be compatible with the actual declared type of
  * the exported `let` in the referenced file.
  */
final class BbinImportResolverTest extends AnyWordSpec {

  private val dummyPath = FSPath.parse(NEString.unsafeFrom("resolver-test.bbin"))

  private def parseBbin(source: String, path: FSPath = dummyPath): RawBbin = {
    val c = ParserContext(path, source)
    val d = new DefBbin(c)
    fastparse.parse(source, d.bbinFile(_)) match {
      case Parsed.Success((usings, imports, stmts), idx) =>
        assert(idx == source.length, s"parser left unconsumed input: [${source.drop(idx)}]")
        RawBbin(path, usings, imports, stmts)
      case f: Parsed.Failure =>
        fail(s"expected parse success for [$source], got: ${f.msg}")
    }
  }

  private def socRef(name: String): RawTypeRef.Simple =
    RawTypeRef.Simple(RawTypeName(name), Nil)

  private def repoRoot(): File = {
    var dir = new File(sys.props("user.dir")).getAbsoluteFile
    while (dir != null && !new File(dir, "studies/power_arch1/golden.bbin").isFile) {
      dir = dir.getParentFile
    }
    assert(dir != null, "could not locate repo root holding studies/power_arch1/golden.bbin")
    dir
  }

  private def readFile(name: String): String = {
    val file = new File(repoRoot(), s"studies/power_arch1/$name")
    assert(file.isFile, s"example study file is missing: $file")
    scala.io.Source.fromFile(file)("UTF-8").mkString
  }

  "BbinImportResolver" should {

    "resolve golden_derate against golden.bbin with no errors" in {
      val goldenSource = readFile("golden.bbin")
      val derateSource = readFile("golden_derate.bbin")
      val importerPath = "studies/power_arch1/golden_derate.bbin"
      val resolved     = BbinImportResolver.resolveRel(importerPath, "golden.bbin")
      val importer     = parseBbin(derateSource)
      val files        = Map(resolved -> goldenSource)
      val errors       = BbinImportResolver.resolveAndCheck(importerPath, importer, files.get)
      assert(errors.isEmpty, s"expected no import errors, got: $errors")
    }

    "accept sealed->sealed, sealed->partial and partial->partial with Same/Unseal coercions" in {
      val target =
        "using p @1\nlet base: SoC = draft\nlet part: ~SoC = draft\n"
      val importerSealed =
        "using p @1\nimport (base: SoC) from rel:'t.bbin'\nlet x: str = rel:'g'"
      val importerUnsealed =
        "using p @1\nimport (base as g: ~SoC) from rel:'t.bbin'\nlet x: str = rel:'g'"
      val importerPartial =
        "using p @1\nimport (part as q: ~SoC) from rel:'t.bbin'\nlet x: str = rel:'g'"
      val files = Map("t.bbin" -> target)
      assert(BbinImportResolver.resolveAndCheck("t2.bbin", parseBbin(importerSealed), files.get).isEmpty)
      assert(BbinImportResolver.resolveAndCheck("t2.bbin", parseBbin(importerUnsealed), files.get).isEmpty)
      assert(BbinImportResolver.resolveAndCheck("t2.bbin", parseBbin(importerPartial), files.get).isEmpty)

      val (sealedOk, _) = BbinImportResolver.resolve("t2.bbin", parseBbin(importerSealed), files.get)
      assert(sealedOk.map(_.coercion) == List(BbinImportResolver.Coercion.Same))

      val (unsealedOk, _) = BbinImportResolver.resolve("t2.bbin", parseBbin(importerUnsealed), files.get)
      assert(unsealedOk.map(_.coercion) == List(BbinImportResolver.Coercion.Unseal))

      val (partialOk, _) = BbinImportResolver.resolve("t2.bbin", parseBbin(importerPartial), files.get)
      assert(partialOk.map(_.coercion) == List(BbinImportResolver.Coercion.Same))
    }

    "pass through the declared type when there is no ascription" in {
      val target   =
        "using p @1\nlet base: SoC = draft\nlet part: ~SoC = draft\n"
      val importer =
        "using p @1\nimport (base, part as q) from rel:'t.bbin'\nlet x: str = rel:'g'"
      val files = Map("t.bbin" -> target)
      val (resolved, errors) =
        BbinImportResolver.resolve("i.bbin", parseBbin(importer), files.get)
      assert(errors.isEmpty, s"expected no import errors, got: $errors")
      assert(resolved.size == 2, s"expected two resolved imports, got: $resolved")
      assert(resolved.map(_.coercion) == List(
        BbinImportResolver.Coercion.Same,
        BbinImportResolver.Coercion.Same,
      ))
      assert(resolved.map(_.resolvedType) == resolved.map(_.declared))
      assert(resolved.map(_.name) == List("base", "part"))
      assert(resolved.map(_.alias) == List(None, Some("q")))
    }

    "accept a partial->sealed ascription as a Seal coercion" in {
      val target   = "using p @1\nlet part: ~SoC = draft\n"
      val importer = "using p @1\nimport (part as q: SoC) from rel:'t.bbin'\nlet x: str = rel:'g'"
      val files    = Map("t.bbin" -> target)
      val (resolved, errors) =
        BbinImportResolver.resolve("i.bbin", parseBbin(importer), files.get)
      assert(errors.isEmpty, s"expected no import errors, got: $errors")
      assert(resolved.size == 1, s"expected one resolved import, got: $resolved")
      assert(resolved.head.coercion == BbinImportResolver.Coercion.Seal)
      assert(resolved.head.declared == BbinType.Partial(socRef("SoC")))
      assert(resolved.head.resolvedType == BbinType.Plain(socRef("SoC")))
    }

    "reject a row-type mismatch" in {
      val target   = "using p @1\nlet base: SoC = draft\n"
      val importer = "using p @1\nimport (base as g: ~Other) from rel:'t.bbin'\nlet x: str = rel:'g'"
      val errors   = BbinImportResolver.resolveAndCheck("i.bbin", parseBbin(importer), Map("t.bbin" -> target).get)
      assert(errors.size == 1, s"expected one error, got $errors")
      assert(errors.head.contains("incompatible"), s"unexpected error: ${errors.head}")
    }

    "reject a partial->sealed set-shape mismatch" in {
      val target   = "using p @1\nlet part: {SoC} = draft\n"
      val importer = "using p @1\nimport (part as q: SoC) from rel:'t.bbin'\nlet x: str = rel:'g'"
      val errors   = BbinImportResolver.resolveAndCheck("i.bbin", parseBbin(importer), Map("t.bbin" -> target).get)
      assert(errors.size == 1, s"expected one error, got $errors")
      assert(errors.head.contains("incompatible"), s"unexpected error: ${errors.head}")
    }

    "reject a missing binding" in {
      val target   = "using p @1\nlet base: SoC = draft\n"
      val importer = "using p @1\nimport (nope as q: ~SoC) from rel:'t.bbin'\nlet x: str = rel:'g'"
      val errors   = BbinImportResolver.resolveAndCheck("i.bbin", parseBbin(importer), Map("t.bbin" -> target).get)
      assert(errors.size == 1, s"expected one error, got $errors")
      assert(errors.head.contains("missing binding"), s"unexpected error: ${errors.head}")
    }

    "reject a missing file" in {
      val importer = "using p @1\nimport (base as g: ~SoC) from rel:'gone.bbin'\nlet x: str = rel:'g'"
      val errors   = BbinImportResolver.resolveAndCheck("dir/i.bbin", parseBbin(importer), Map.empty.get)
      assert(errors.size == 1, s"expected one error, got $errors")
      assert(errors.head.contains("missing file"), s"unexpected error: ${errors.head}")
    }

    "resolve rel: against the importing file's directory" in {
      assert(BbinImportResolver.resolveRel("a/b/i.bbin", "t.bbin") == "a/b/t.bbin")
      assert(BbinImportResolver.resolveRel("i.bbin", "t.bbin") == "t.bbin")
      assert(BbinImportResolver.resolveRel("a/b/i.bbin", "./t.bbin") == "a/b/t.bbin")
    }
  }
}
