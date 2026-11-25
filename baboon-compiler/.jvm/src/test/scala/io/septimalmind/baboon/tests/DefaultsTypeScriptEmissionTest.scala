package io.septimalmind.baboon.tests

import distage.plugins.PluginBase
import io.septimalmind.baboon.*
import io.septimalmind.baboon.CompilerTarget.TsTarget
import io.septimalmind.baboon.parser.model.FSPath
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.tests.BaboonTest.BaboonTestModule
import io.septimalmind.baboon.translator.BaboonAbstractTranslator
import io.septimalmind.baboon.typer.model.BaboonFamily
import izumi.distage.plugins.PluginConfig
import izumi.distage.testkit.model.TestConfig
import izumi.functional.bio.Error2
import izumi.functional.bio.unsafe.UnsafeInstances
import izumi.fundamentals.collections.nonempty.{NEList, NEString}
import izumi.fundamentals.platform.files.IzFiles
import izumi.fundamentals.platform.resources.IzResources
import izumi.reflect.TagKK

/** TypeScript @default emission test: verifies that `TsDefnTranslator` renders
  * TypeScript-appropriate default literals for leaf scalars, enum, and special floats.
  * Pins the `Status.Active`-style short-name enum default (cf. DefaultLiteralSupport
  * `enumTpe.name` fix covered on the Py side by DefaultsPythonEmissionTest).
  */
final class DefaultsTypeScriptEmissionTest extends DefaultsTypeScriptEmissionTestBase[Either]

abstract class DefaultsTypeScriptEmissionTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private val tsTarget: TsTarget = TsTarget(
    id = "TypeScript",
    output = OutputOptions(
      safeToRemoveExtensions = Set.empty,
      runtime                = RuntimeGenOpt.With,
      generateConversions    = true,
      output                 = FSPath.parse(NEString.unsafeFrom("./target/baboon-scalatests-ts-defaults/")),
      fixturesOutput         = None,
      testsOutput            = None,
    ),
    generic = GenericOptions(
      codecTestIterations = 0
    ),
    language = TsOptions(
      writeEvolutionDict          = false,
      wrappedAdtBranchCodecs      = false,
      importSuffix                = "",
      generateJsonCodecs          = true,
      generateUebaCodecs          = true,
      generateJsonCodecsByDefault = true,
      generateUebaCodecsByDefault = true,
      generateDomainFacade        = false,
      serviceResult               = ServiceResultConfig.typescriptDefault,
      serviceContext              = ServiceContextConfig.default,
      pragmas                     = Map.empty,
      asyncServices               = false,
      bareServiceSymbols          = false,
      mapsAsRecords               = false,
      timestampsUtcMode           = "wrapper",
      timestampsOffsetMode        = "wrapper",
      enumLowercaseValues         = false,
      generateMcpServer           = false,
    ),
  )

  private val baseModule: distage.Module =
    new BaboonModuleJvm[Either](
      CompilerOptions(
        debug                    = false,
        individualInputs         = Set.empty,
        directoryInputs          = Set(FSPath.parse(NEString.unsafeFrom("./baboon-compiler/src/test/resources/defaults-ok"))),
        metaWriteEvolutionJsonTo = None,
        lockfile                 = Some(FSPath.parse(NEString.unsafeFrom("./target/baboon-defaults-ts.lock"))),
        emitOnly                 = None,
        targets                  = Seq(tsTarget),
      ),
      UnsafeInstances.Lawless_ParallelErrorAccumulatingOpsEither,
    )

  private val translatorModule: distage.Module = new BaboonJvmTsModule[Either](tsTarget)
  private val combinedModule: distage.Module   = baseModule overriddenBy translatorModule

  override protected def config: TestConfig = super.config.copy(
    pluginConfig = PluginConfig.const(combinedModule.morph[PluginBase]),
    activation   = super.config.activation + BaboonModeAxis.Compiler,
  )

  private def loadDefaultsFamily(loader: BaboonLoader[F]): F[NEList[BaboonIssue], BaboonFamily] = {
    val root = IzResources
      .getPath("defaults-ok/v1.baboon")
      .getOrElse(throw new AssertionError("defaults-ok fixture not found"))
      .asInstanceOf[IzResources.LoadablePathReference]
      .path
    val baboons =
      if (root.toFile.isDirectory)
        IzFiles.walk(root.toFile).toList.filter(p => p.toFile.isFile && p.toFile.getName.endsWith(".baboon"))
      else List(root)
    loader.load(baboons)
  }

  "defaults-ok fixture, TypeScript target" should {

    "emit TypeScript default literals for leaf kinds and enum short name" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadDefaultsFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.map { case (path, of) => (path, of.content) }.toList

          val defaultsRendering = all.collectFirst {
            case (path, content) if path.endsWith("Defaults.ts") => content
          }.getOrElse(fail(s"Defaults.ts not found. Paths: ${all.map(_._1)}"))

          assert(
            defaultsRendering.contains("Status.Active"),
            s"Expected enum default short name Status.Active. Source:\n$defaultsRendering",
          )
          assert(
            !defaultsRendering.contains("PyType") && !defaultsRendering.contains("Status(\"Active\")"),
            s"Must NOT emit fully-qualified/wrapped enum default. Source:\n$defaultsRendering",
          )
          assert(
            defaultsRendering.contains("42"),
            s"Expected i32 default literal. Source:\n$defaultsRendering",
          )
          assert(
            defaultsRendering.contains("1.5"),
            s"Expected f64 default literal. Source:\n$defaultsRendering",
          )
          assert(
            defaultsRendering.contains("true"),
            s"Expected bit default literal. Source:\n$defaultsRendering",
          )
          assert(
            defaultsRendering.contains("\"hello\""),
            s"Expected str default literal. Source:\n$defaultsRendering",
          )
          assert(
            defaultsRendering.contains("null"),
            s"Expected opt[str] null default. Source:\n$defaultsRendering",
          )
          assert(
            defaultsRendering.contains("NaN"),
            s"Expected NaN default. Source:\n$defaultsRendering",
          )
          assert(
            defaultsRendering.contains("Infinity") && defaultsRendering.contains("-Infinity"),
            s"Expected +/-Infinity defaults. Source:\n$defaultsRendering",
          )
        }
    }
  }
}
