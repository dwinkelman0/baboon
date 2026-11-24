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
    val root = TestResources.fixturePath("defaults-ok/v1.baboon")
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
          assert(
            defaultsRendering.contains("def _baboon_default_Defaults_letField() -> int:"),
            s"Expected let default helper. Source:\n$defaultsRendering",
          )
          assert(
            defaultsRendering.contains("default_factory=_baboon_default_Defaults_letField"),
            s"Expected let default_factory. Source:\n$defaultsRendering",
          )
          assert(
            defaultsRendering.contains("n = (n + 1)"),
            s"Expected let-assign lowering. Source:\n$defaultsRendering",
          )
          assert(
            defaultsRendering.contains("def _baboon_default_Defaults_forField() -> int:"),
            s"Expected for-loop default helper. Source:\n$defaultsRendering",
          )
          assert(
            defaultsRendering.contains("while (i < 10):"),
            s"Expected for-C while lowering. Source:\n$defaultsRendering",
          )
          assert(
            defaultsRendering.contains("is not None"),
            s"Expected match Some check. Source:\n$defaultsRendering",
          )
          assert(
            defaultsRendering.contains("is None"),
            s"Expected match None check. Source:\n$defaultsRendering",
          )
          assert(
            defaultsRendering.contains("_baboon_normalize_normField"),
            s"Expected normalize validator. Source:\n$defaultsRendering",
          )
          assert(
            defaultsRendering.contains("object.__setattr__(self, 'normField'"),
            s"Expected normalize setattr. Source:\n$defaultsRendering",
          )
          assert(
            defaultsRendering.contains("def shouty(self)"),
            s"Expected computed property. Source:\n$defaultsRendering",
          )
          assert(
            defaultsRendering.contains("def _baboon_default_Defaults_letField() -> int:"),
            s"Expected let-default helper. Source:\n$defaultsRendering",
          )
          assert(
            defaultsRendering.contains("letField: int = Field(default_factory=_baboon_default_Defaults_letField)"),
            s"Expected let-default factory field. Source:\n$defaultsRendering",
          )
          assert(
            defaultsRendering.contains("n = (n + 1)"),
            s"Expected assign lowering in default helper. Source:\n$defaultsRendering",
          )
          assert(
            defaultsRendering.contains("forField: int = Field(default_factory=_baboon_default_Defaults_forField)"),
            s"Expected for-default factory field. Source:\n$defaultsRendering",
          )
          assert(
            defaultsRendering.contains("while (i < 10):") && defaultsRendering.contains("total = (total + i)"),
            s"Expected C-style for lowering to while loop. Source:\n$defaultsRendering",
          )
          assert(
            defaultsRendering.contains("matchField: str = Field(default_factory=_baboon_default_Defaults_matchField)") &&
              defaultsRendering.contains("if _baboon_match_0 is not None:") &&
              defaultsRendering.contains("elif _baboon_match_0 is None:") &&
              defaultsRendering.contains("r = x"),
            s"Expected match lowering to if/elif on opt variants. Source:\n$defaultsRendering",
          )
          assert(
            defaultsRendering.contains("@model_validator(mode=\"after\")") &&
              defaultsRendering.contains("def _baboon_normalize_normField(self):") &&
              defaultsRendering.contains("object.__setattr__(self, 'normField',"),
            s"Expected normalize post-init hook. Source:\n$defaultsRendering",
          )
          assert(
            defaultsRendering.contains("def migrate_Defaults_migField(old) -> str:") &&
              defaultsRendering.contains("return old.strip()"),
            s"Expected migrate version function. Source:\n$defaultsRendering",
          )
          assert(
            defaultsRendering.contains("def shouty(self) -> str:") &&
              defaultsRendering.contains("return \"hey\".upper()"),
            s"Expected computed property getter. Source:\n$defaultsRendering",
          )
          assert(
            defaultsRendering.contains("def curve(self, t: float) -> float:") &&
              !defaultsRendering.contains("@property\n    def curve"),
            s"Expected parameterized computed method (no @property). Source:\n$defaultsRendering",
          )
        }
    }
  }
}
