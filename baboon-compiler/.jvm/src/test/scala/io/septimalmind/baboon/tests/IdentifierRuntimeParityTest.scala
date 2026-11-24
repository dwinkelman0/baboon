package io.septimalmind.baboon.tests

import io.septimalmind.baboon.translator.BaboonRuntimeResources
import org.scalatest.funsuite.AnyFunSuite

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

class IdentifierRuntimeParityTest extends AnyFunSuite {
  test("compile-side and embedded Scala IdentifierRepr implementations remain identical") {
    val relative = Paths.get("baboon-compiler/src/main/scala/baboon/runtime/shared/BaboonRuntimeShared.scala")
    val cwd      = Paths.get(System.getProperty("user.dir")).toAbsolutePath
    // Under Bazel the test cwd is the runfiles tree and the compile-side source
    // rides along as `data`; resolve it via $TEST_SRCDIR/$TEST_WORKSPACE first.
    // Under sbt TEST_SRCDIR is unset, so fall back to the cwd walk.
    val runfiles = for {
      srcdir <- sys.env.get("TEST_SRCDIR")
      ws     <- sys.env.get("TEST_WORKSPACE")
      p       = Paths.get(srcdir, ws, relative.toString)
      if Files.isRegularFile(p)
    } yield p
    val source = runfiles.orElse(
      Iterator
        .iterate[Path](cwd)(_.getParent).takeWhile(_ != null)
        .map(_.resolve(relative)).find(Files.isRegularFile(_))
    ).getOrElse(fail(s"Cannot locate compile-side identifier source from $cwd"))
    val mirror   = new String(Files.readAllBytes(source), StandardCharsets.UTF_8)
    val embedded = BaboonRuntimeResources.read("baboon-runtime/scala/BaboonRuntimeShared.scala")
    assert(identifierObject(mirror) == identifierObject(embedded))
  }

  private def identifierObject(source: String): String = {
    val declaration = "  object IdentifierRepr {"
    val start       = source.indexOf(declaration)
    assert(start >= 0, "IdentifierRepr declaration is missing")
    val end = source.indexOf("\n  }", start)
    assert(end > start, "IdentifierRepr closing delimiter is missing")
    source.substring(start, end + "\n  }".length)
  }
}
