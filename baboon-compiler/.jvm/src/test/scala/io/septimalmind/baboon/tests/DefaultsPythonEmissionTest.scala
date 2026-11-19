package io.septimalmind.baboon.tests

import distage.plugins.PluginBase
import io.septimalmind.baboon.*
import io.septimalmind.baboon.CompilerTarget.PyTarget
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

/** Python @default emission test: verifies that `PyDefnTranslator` renders
  * Python-appropriate default literals and `Field(default=...)` annotations
  * for every leaf scalar kind, enum, and special float.
  */
final class DefaultsPythonEmissionTest extends DefaultsPythonEmissionTestBase[Either]

abstract class DefaultsPythonEmissionTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private val pyTarget: PyTarget = PyTarget(
    id = "Python",
    output = OutputOptions(
      safeToRemoveExtensions = Set.empty,
      runtime                = RuntimeGenOpt.With,
      generateConversions    = true,
      output                 = FSPath.parse(NEString.unsafeFrom("./target/baboon-scalatests-py-defaults/")),
      fixturesOutput         = None,
      testsOutput            = None,
    ),
    generic = GenericOptions(
      codecTestIterations = 0
    ),
    language = PyOptions(
      writeEvolutionDict          = true,
      wrappedAdtBranchCodecs      = false,
      generateJsonCodecs          = true,
      generateUebaCodecs          = true,
      generateJsonCodecsByDefault = true,
      generateUebaCodecsByDefault = true,
      enableDeprecatedEncoders    = false,
      serviceResult               = ServiceResultConfig.pythonDefault,
      serviceContext              = ServiceContextConfig.default,
      pragmas                     = Map.empty,
      generateDomainFacade        = false,
      asyncServices               = false,
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
        lockfile                 = Some(FSPath.parse(NEString.unsafeFrom("./target/baboon-defaults.lock"))),
        emitOnly                 = None,
        targets                  = Seq(pyTarget),
      ),
      UnsafeInstances.Lawless_ParallelErrorAccumulatingOpsEither,
    )

  private val translatorModule: distage.Module = new BaboonJvmPyModule[Either](pyTarget)
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

  "defaults-ok fixture, Python target" should {

    "emit Python default literals for all leaf kinds and enum" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadDefaultsFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.map { case (path, of) => (path, of.content) }.toList

          val defaultsRendering = all.collectFirst {
            case (path, content) if path.endsWith("Defaults.py") => content
          }.getOrElse(fail(s"Defaults.py not found. Paths: ${all.map(_._1)}"))

          assert(
            defaultsRendering.contains("intField: int = 42"),
            s"Expected i32 default literal. Source:\n$defaultsRendering",
          )
          assert(
            defaultsRendering.contains("floatField: float = 1.5"),
            s"Expected f64 default literal. Source:\n$defaultsRendering",
          )
          assert(
            defaultsRendering.contains("boolField: bool = True"),
            s"Expected bit default literal. Source:\n$defaultsRendering",
          )
          assert(
            defaultsRendering.contains("strField: str = \"hello\""),
            s"Expected str default literal. Source:\n$defaultsRendering",
          )
          assert(
            defaultsRendering.contains("optField: Optional[str] = None"),
            s"Expected opt[str] null default. Source:\n$defaultsRendering",
          )
          assert(
            defaultsRendering.contains("enumField: Status = Status.Active"),
            s"Expected enum default reference. Source:\n$defaultsRendering",
          )
          assert(
            defaultsRendering.contains("nanField: float = float('nan')"),
            s"Expected NaN default. Source:\n$defaultsRendering",
          )
          assert(
            defaultsRendering.contains("posInfField: float = float('inf')"),
            s"Expected +inf default. Source:\n$defaultsRendering",
          )
          assert(
            defaultsRendering.contains("negInfField: float = float('-inf')"),
            s"Expected -inf default. Source:\n$defaultsRendering",
          )
          assert(
            defaultsRendering.contains("fnIntField: int = (lambda: (40 + 2))()"),
            s"Expected fn int default callable. Source:\n$defaultsRendering",
          )
          assert(
            defaultsRendering.contains("fnStrField: str = (lambda: \"hello\".upper())()"),
            s"Expected fn str default callable. Source:\n$defaultsRendering",
          )
          assert(
            defaultsRendering.contains("fnBoolField: bool = (lambda: ((1 < 2) and (3 < 4)))()"),
            s"Expected fn bool default callable. Source:\n$defaultsRendering",
          )
          assert(
            defaultsRendering.contains("\"yes\" if (1 < 2) else \"no\""),
            s"Expected fn if default callable. Source:\n$defaultsRendering",
          )
          assert(
            defaultsRendering.contains("assert_trace((1 < 2), {}, \"one is less than two\")"),
            s"Expected fn assert_trace call. Source:\n$defaultsRendering",
          )
        }
    }
  }
}
