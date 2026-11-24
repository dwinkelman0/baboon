package io.septimalmind.baboon.tests

import izumi.fundamentals.platform.resources.IzResources

import java.io.File
import java.nio.file.{FileSystems, Files, Path, StandardCopyOption}

/** Resolves test fixtures to real filesystem locations.
  *
  * Under sbt, test resources are loose files, so `IzResources.getPath(...).path` lives on the default filesystem and
  * `.toFile` works directly. Under Bazel `resources = glob(...)` packs fixtures into the test jar, so the path is a
  * ZipPath and `.toFile` throws `UnsupportedOperationException`.
  *
  * This helper keeps the fast path for plain files and otherwise copies the tree to a temp dir, preserving relative
  * paths. Uses only JDK APIs plus izumi (already on the test classpath); no new dependencies.
  */
object TestResources {

  /** Resolve a directory fixture to a real `File`, materializing it out of the jar when needed. */
  def fixtureDir(resource: String): File =
    fixturePath(resource).toFile

  /** Resolve a fixture to a real-filesystem `Path`, materializing it out of the jar when needed. */
  def fixturePath(resource: String): Path = {
    // NOTE: this IzResources lookup is the primitive every call site funnels through; do NOT rewrite it.
    val path = IzResources.getPath(resource).get.asInstanceOf[IzResources.LoadablePathReference].path
    materialize(path)
  }

  /** Return `path` itself when it lives on the default filesystem, else copy it (file or tree) to a temp dir. */
  def materialize(path: Path): Path = {
    if (path.getFileSystem == FileSystems.getDefault) {
      path
    } else if (Files.isDirectory(path)) {
      val tmp = Files.createTempDirectory("baboon-fixture-")
      tmp.toFile.deleteOnExit()
      val stream = Files.walk(path)
      try {
        stream.forEach {
          p =>
            if (p != path) {
              val target = tmp.resolve(path.relativize(p).toString)
              if (Files.isDirectory(p)) {
                Files.createDirectories(target)
              } else {
                val parent = target.getParent
                if (parent != null) {
                  Files.createDirectories(parent)
                }
                Files.copy(p, target, StandardCopyOption.REPLACE_EXISTING)
              }
            }
        }
      } finally {
        stream.close()
      }
      tmp
    } else {
      val tmp    = Files.createTempDirectory("baboon-fixture-")
      tmp.toFile.deleteOnExit()
      val target = tmp.resolve(path.getFileName.toString)
      Files.copy(path, target, StandardCopyOption.REPLACE_EXISTING)
      target
    }
  }
}
