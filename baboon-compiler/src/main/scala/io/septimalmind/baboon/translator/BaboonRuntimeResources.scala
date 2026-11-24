package io.septimalmind.baboon.translator

import izumi.fundamentals.platform.resources.PortableResource

object BaboonRuntimeResources {
  // Macro picks up resource files at compile time; the embedded map goes stale
  // when files are added under `baboon-compiler/src/main/resources/baboon-runtime/<lang>/`
  // without a recompile of this unit.
  private val resources: Map[String, String] = PortableResource.embedSources("baboon-compiler/src/main/resources/{baboon-runtime/**}")

  def read(path: String): String = {
    resources.getOrElse(path, throw new RuntimeException(s"Runtime resource not found: $path, available: ${resources.keys.mkString(", ")}"))
  }
}
