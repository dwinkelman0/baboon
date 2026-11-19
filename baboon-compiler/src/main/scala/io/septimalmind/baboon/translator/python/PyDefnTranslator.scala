package io.septimalmind.baboon.translator.python

import io.septimalmind.baboon.CompilerProduct
import io.septimalmind.baboon.CompilerTarget.PyTarget
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.IdentifierFieldKind
import io.septimalmind.baboon.translator.{ResolvedServiceContext, ServiceContextResolver, ServiceResultResolver}
import io.septimalmind.baboon.translator.python.PyKeywords.escapePyKeyword
import io.septimalmind.baboon.translator.python.PyTypes.*
import io.septimalmind.baboon.translator.python.PyValue.{PyModuleId, PyType}
import io.septimalmind.baboon.typer.{BaboonEnquiries, EnumWireStyle}
import io.septimalmind.baboon.typer.model.*
import izumi.functional.bio.{Applicative2, F}
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.Quote
import io.septimalmind.baboon.translator.DomainEnquiries

trait PyDefnTranslator[F[+_, +_]] {
  def translate(defn: DomainMember.User): F[NEList[BaboonIssue], List[PyDefnTranslator.Output]]
  def translateFixtures(defn: DomainMember.User): F[NEList[BaboonIssue], List[PyDefnTranslator.Output]]
  def translateTests(defn: DomainMember.User): F[NEList[BaboonIssue], List[PyDefnTranslator.Output]]
  def translateServiceRt(): F[NEList[BaboonIssue], List[PyDefnTranslator.Output]]
}

object PyDefnTranslator {
  final case class CodecReg(
    typeId: TypeId,
    tpe: PyType,
    tpeKeepForeigns: PyType,
    tpeId: TextTree[PyValue],
    trees: Map[String, TextTree[PyValue]],
  )

  final case class Output(
    path: String,
    tree: TextTree[PyValue],
    module: PyValue.PyModuleId,
    product: CompilerProduct,
    codecReg: List[(String, List[TextTree[PyValue]])] = List.empty,
  )

  final case class PyDefnRepr(
    defn: TextTree[PyValue],
    codecs: List[CodecReg],
  )

  final class PyDefnTranslatorImpl[F[+_, +_]: Applicative2](
    target: PyTarget,
    codecsFixture: PyCodecFixtureTranslator,
    codecsTests: PyCodecTestTranslator,
    domainTypes: PyDomainTypes,
    domainEnquiries: DomainEnquiries,
    codecs: Set[PyCodecTranslator],
    pyDomTrees: PyDomainTreeTools,
    evolution: BaboonEvolution,
    fileTools: PyFileTools,
    domain: Domain,
    wiringTranslator: PyServiceWiringTranslator,
    pyTreeTools: PyTreeTools,
  ) extends PyDefnTranslator[F] {
    override def translate(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      defn.id.owner match {
        case Owner.Adt(_) => F.pure(List.empty)
        case _            => doTranslate(defn)
      }
    }

    override def translateFixtures(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      defn.id.owner match {
        case Owner.Adt(_) => F.pure(List.empty)
        case _            => doTranslateFixtures(defn)
      }
    }

    override def translateTests(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      defn.id.owner match {
        case Owner.Adt(_) => F.pure(List.empty)
        case _            => doTranslateTests(defn)
      }
    }

    private def doTranslateTests(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      val codecsTestsOut = codecsTests
        .translate(
          defn,
          domainTypes.asPyType(defn.id, fileTools.definitionsBasePkg),
          domainTypes.asPyTypeKeepForeigns(defn.id, fileTools.definitionsBasePkg),
        ).map(
          codecsTest =>
            Output(
              getOutputPath(defn, prefix = Some("test_")),
              codecsTest,
              domainTypes.toPyModule(defn.id, fileTools.testsBasePkg),
              CompilerProduct.Test,
            )
        )

      F.pure(codecsTestsOut.toList)
    }

    override def translateServiceRt(): F[NEList[BaboonIssue], List[Output]] = {
      val rtTree = wiringTranslator.translateServiceRt()
      val result = rtTree.map {
        tree =>
          val fbase = fileTools.basename(domain, evolution)
          val serviceRtModule = {
            val pathToModule = PyFileTools.domainPath(domain.id)
            val fullPath     = fileTools.definitionsBasePkg ++ pathToModule ++ List("BaboonServiceRt")
            PyModuleId(NEList.unsafeFrom(fullPath))
          }
          Output(
            s"$fbase/BaboonServiceRt.py",
            tree,
            serviceRtModule,
            CompilerProduct.Definition,
          )
      }.toList
      F.pure(result)
    }

    private def doTranslateFixtures(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      val fixtureTree = codecsFixture
        .translate(defn).map(
          fixture =>
            Output(
              getOutputPath(defn, suffix = Some("_Fixture")),
              fixture,
              codecsFixture.fixtureType(defn.id).moduleId,
              CompilerProduct.Fixture,
            )
        )
      F.pure(fixtureTree.toList)
    }

    private def doTranslate(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      val repr = makeFullRepr(defn)

      val regsPerCodec =
        codecs.toList.map(codecTranslator => (codecTranslator.id, repr.codecs.flatMap(reg => reg.trees.get(codecTranslator.id).map(expr => q"${reg.tpeId}, $expr"))))

      val mainOutput = Output(
        getOutputPath(defn),
        repr.defn,
        domainTypes.toPyModule(defn.id, fileTools.definitionsBasePkg),
        CompilerProduct.Definition,
        codecReg = regsPerCodec,
      )

      val wiringOutput = wiringTranslator
        .translate(defn).map {
          wiringTree =>
            val wiringModule = domainTypes.toPyModule(defn.id, fileTools.definitionsBasePkg)
              .withModuleName(s"${defn.id.name.name}_Wiring")
            Output(
              getOutputPath(defn, suffix = Some("_Wiring")),
              wiringTree,
              wiringModule,
              CompilerProduct.Definition,
            )
        }.toList

      val clientOutput = wiringTranslator
        .translateClient(defn).map {
          clientTree =>
            val clientModule = domainTypes.toPyModule(defn.id, fileTools.definitionsBasePkg)
              .withModuleName(s"${defn.id.name.name}_Client")
            Output(
              getOutputPath(defn, suffix = Some("_Client")),
              clientTree,
              clientModule,
              CompilerProduct.Definition,
            )
        }.toList

      F.pure(mainOutput :: (wiringOutput ++ clientOutput))
    }

    private def makeFullRepr(defn: DomainMember.User): PyDefnRepr = {
      val isLatestVersion = domain.version == evolution.latest

      def obsoletePrevious(tree: TextTree[PyValue]): TextTree[PyValue] = {
        if (isLatestVersion || tree.isEmpty) tree
        else {
          q"""@$deprecated("Version ${domain.version.toString} is obsolete, you should migrate to ${evolution.latest.toString}")
             |$tree""".stripMargin
        }
      }

      val pyRef  = domainTypes.asPyType(defn.id, fileTools.definitionsBasePkg)
      val srcRef = domainTypes.asPyTypeKeepForeigns(defn.id, fileTools.definitionsBasePkg)

      val repr       = mkRepr(defn, isLatestVersion)
      val defnRepr   = List(obsoletePrevious(repr.defn))
      val codecTrees = codecs.toList.flatMap(_.translate(defn, pyRef, srcRef)).map(obsoletePrevious)
      val allDefs    = (defnRepr ++ codecTrees).joinNN()

      val reg = defn.defn match {
        case _: Typedef.NonDataTypedef => List.empty[CodecReg]
        case d =>
          val codecsReg = codecs.toList
            .sortBy(_.getClass.getName)
            .flatMap {
              codec =>
                if (codec.isActive(d.id)) {
                  List(codec.id -> q"${codec.codecType(defn.id)}.instance")
                } else {
                  List.empty
                }
            }
          List(CodecReg(defn.id, pyRef, srcRef, q"\"${defn.id.toString}\"", codecsReg.toMap))
      }

      val allRegs = reg ++ repr.codecs

      assert(defn.id.pkg == domain.id)
      PyDefnRepr(allDefs, allRegs)
    }

    private def mkRepr(defn: DomainMember.User, isLatestVersion: Boolean): PyDefnRepr = {
      val genMarker       = if (isLatestVersion) baboonGeneratedLatest else baboonGenerated
      val codecMeta       = codecs.map(_.codecMeta(defn.id).member)
      val mainMeta        = pyDomTrees.makeDataMeta(defn) ++ codecMeta
      val jsonCodecActive = codecs.collectFirst { case jsonCodec: PyJsonCodecGenerator => jsonCodec }.exists(_.isActive(defn.id))
      defn.defn match {
        case dto: Typedef.Dto =>
          val dtoContracts     = dto.contracts
          val dtoContractsDefs = dtoContracts.flatMap(domain.defs.meta.nodes.get).collect { case DomainMember.User(_, c: Typedef.Contract, _, _) => c }
          val contractsFields  = dtoContractsDefs.flatMap(_.fields)

          val dtoLowering    = genDtoFields(dto, contractsFields.toSet)
          val dtoFieldsTrees = dtoLowering.fields
          val dtoProperties  = genDtoProperties(contractsFields)

          val contractParents = dto.contracts.toSeq
          val adtParent = dto.id.owner match {
            case Owner.Adt(id) => List(id)
            case _             => Nil
          }
          val directParentsDefs = (adtParent ++ contractParents).flatMap(domain.defs.meta.nodes.get).collect { case u: DomainMember.User => u }

          val superclasses        = domainEnquiries.collectParents( directParentsDefs).toSet
          val uniqueContracts     = dtoContracts.filterNot(c1 => superclasses.contains(c1))
          val genMarkerParent     = if (adtParent.nonEmpty || contractParents.nonEmpty) Nil else List(genMarker)
          val adtMemberMetaParent = if (adtParent.isEmpty) Nil else List(baboonAdtMemberMeta)

          val superclassesTypes = (adtParent ++ uniqueContracts).map(c => domainTypes.asPyType(c, fileTools.definitionsBasePkg))

          val parentTypes = superclassesTypes ++ genMarkerParent ++ adtMemberMetaParent :+ pydanticBaseModel

          val parents = mkParents(parentTypes)

          val modelConfig = genDtoPydanticModelConf(dto.fields, dtoContracts.nonEmpty, jsonCodecActive)

          // Identifier toString (`__repr__` / `__str__`) emission (PR-57d / spec:
          // docs/spec/identifier-repr.md). Emitted only when `dto.isIdentifier`.
          // The parser lives on a sibling `<TypeName>Codec` class
          // (Q-FU-4: keeps it discoverable but not as `MyId.parse_repr`).
          val identifierRepr: Option[TextTree[PyValue]] =
            if (dto.isIdentifier) Some(renderIdentifierRepr(dto)) else None
          val identifierCodec: Option[TextTree[PyValue]] =
            if (dto.isIdentifier) Some(renderIdentifierCodecClass(dto)) else None

          val classDocstring = pyTreeTools.renderClassDocstring(
            defn.docs,
            dto.fields.map(f => f.name.name -> f.docs),
            "    ",
          )
          val classDocTree: Option[TextTree[PyValue]] =
            if (classDocstring.isEmpty) None else Some(q"$classDocstring")

          // Statement-bodied fns (`let`/loops/`match` defaults, `@normalize`
          // validators, computed `@property`s) append extra class members here.
          // Empty in the common case so output for fn-free DTOs is unchanged.
          val fnMembers =
            if (dtoLowering.classMembers.isEmpty) None
            else Some(dtoLowering.classMembers.joinN())

          val members =
            List(
              classDocTree,
              Some(dtoFieldsTrees.joinN()),
              Some(modelConfig),
              dtoProperties.map(_.joinN()),
              fnMembers,
              Some(mainMeta.joinN()),
              identifierRepr,
            ).flatten

          val classTree =
            q"""class ${dto.id.name.name.capitalize}($parents):
               |    ${members.joinNN().shift(4).trim}
               |""".stripMargin

          // Module-level helpers for statement-bodied fns (`@default` helpers,
          // `@migrate` version functions) are emitted ahead of the class.
          // Empty in the common case so output for fn-free DTOs is unchanged.
          val withHelpers =
            if (dtoLowering.moduleHelpers.isEmpty) classTree
            else
              q"""${dtoLowering.moduleHelpers.joinNN()}
                 |
                 |$classTree
                 |""".stripMargin

          val combined = identifierCodec match {
            case Some(codecTree) =>
              q"""$withHelpers
                 |
                 |$codecTree
                 |""".stripMargin
            case None => withHelpers
          }

          PyDefnRepr(combined, List.empty)

        case enum: Typedef.Enum =>
          val branches = enum.members.map {
            m =>
              val wireName = EnumWireStyle.wireName(m.name)
              // Escape Python keywords in enum member identifiers. The value (wire-string) stays
              // as the original wire name; only the Python attribute name gets the trailing `_`.
              val memberPyIdent = escapePyKeyword(wireName)
              q"$memberPyIdent = \"$wireName\""
          }.toSeq
          val enumDocstring = pyTreeTools.renderClassDocstring(defn.docs, Seq.empty, "    ")
          val enumDocTree: Seq[TextTree[PyValue]] =
            if (enumDocstring.isEmpty) Seq.empty else Seq(q"$enumDocstring")
          val enumMembers = (enumDocTree ++ branches).joinN()
          PyDefnRepr(
            q"""|class ${enum.id.name.name.capitalize}($pyEnum):
                |    ${enumMembers.shift(4).trim}
                |""".stripMargin,
            List.empty,
          )
        case adt: Typedef.Adt =>
          val adtContractDefs    = adt.contracts.flatMap(domain.defs.meta.nodes.get).collect { case u: DomainMember.User => u }
          val adtSuperclasses    = domainEnquiries.collectParents( adtContractDefs).toSet
          val uniqueAdtContracts = adt.contracts.filterNot(c => adtSuperclasses.contains(c))
          val contracts          = uniqueAdtContracts.map(c => domainTypes.asPyType(c, fileTools.definitionsBasePkg))
          val defaultParents     = contracts ++ List(pydanticBaseModel)
          val genMarkerParent    = if (adt.contracts.isEmpty) List(genMarker) else Nil
          val allParents         = defaultParents ++ genMarkerParent
          val parents            = mkParents(allParents)

          val memberTrees = adt.members.map(
            mid =>
              domain.defs.meta.nodes.get(mid) match {
                case Some(mdefn: DomainMember.User) => makeFullRepr(mdefn)
                case m                              => throw new RuntimeException(s"BUG: missing/wrong adt member: $mid => $m")
              }
          )

          val branches = memberTrees
            .map(_.defn)
            .toSeq

          val jsonCodec = if (jsonCodecActive) {
            Some(q"""
                    |__registry__: dict[str, type] = $pyDefaultDict()
                    |
                    |def __init_subclass__(cls, **kwargs):
                    |   super().__init_subclass__(**kwargs)
                    |   ${adt.id.name.name.capitalize}.__registry__[cls.__name__] = cls
                    |
                    |@$pydanticModelSerializer(mode='wrap')
                    |def serialize(self, serializer):
                    |    return {self.__class__.__name__: serializer(self)}
                    |
                    |@$pydanticModelValidator(mode="wrap")
                    |@$pyClassMethod
                    |def polymorphic(cls, values, handler):
                    |    if isinstance(values, dict) and len(values) == 1:
                    |        class_name = next(iter(values))
                    |        registry = ${adt.id.name.name.capitalize}.__registry__
                    |
                    |        if class_name in registry:
                    |            candidate = registry[class_name]
                    |            if issubclass(candidate, cls):
                    |                return candidate.model_validate(values[class_name])
                    |            else:
                    |                raise ValueError("not subcluss")
                    |
                    |    return handler(values)
                    |""".stripMargin)
          } else None

          val adtDocstring = pyTreeTools.renderClassDocstring(defn.docs, Seq.empty, "    ")
          val adtDocTree: Option[TextTree[PyValue]] =
            if (adtDocstring.isEmpty) None else Some(q"$adtDocstring")

          val members = List(
            adtDocTree,
            jsonCodec,
            Some(mainMeta.joinN()),
          ).flatten

          val regs = memberTrees.map(_.codecs)

          PyDefnRepr(
            q"""|class ${adt.id.name.name.capitalize}($parents):
                |    pass
                |
                |    ${members.joinNN().shift(4).trim}
                |
                |${branches.joinNN()}
                |""".stripMargin,
            regs.toList.flatten,
          )

        case contract: Typedef.Contract =>
          val contracts  = contract.contracts.map(c => domainTypes.asPyType(c, fileTools.definitionsBasePkg))
          val allParents = if (contract.contracts.isEmpty) List(genMarker, pyABC) ++ contracts else contracts
          val parents    = mkParents(allParents)
          val methods = contract.fields.map {
            f =>
              val tpe  = domainTypes.asPyRef(f.tpe, fileTools.definitionsBasePkg)
              val name = escapePyKeyword(f.name.name)
              q"""@$pyAbstractMethod
                 |def $name(self) -> $tpe:
                 |    raise NotImplementedError
                 |""".stripMargin
          }
          val contractDocstring = pyTreeTools.renderClassDocstring(defn.docs, Seq.empty, "    ")
          val contractDocTree: Option[TextTree[PyValue]] =
            if (contractDocstring.isEmpty) None else Some(q"$contractDocstring")
          val contractBody = contractDocTree.toList ++ (if (methods.isEmpty) List(q"pass") else methods)
          val allMethods   = contractBody.joinN()
          PyDefnRepr(
            q"""|class ${contract.id.name.name.capitalize}($parents):
                |    ${allMethods.shift(4).trim}
                |""".stripMargin,
            List.empty,
          )
        case service: Typedef.Service =>
          val resolved    = ServiceResultResolver.resolve(domain, "python", target.language.serviceResult, target.language.pragmas)
          val resolvedCtx = ServiceContextResolver.resolve(domain, "python", target.language.serviceContext, target.language.pragmas)
          val ctxParam = resolvedCtx match {
            case ResolvedServiceContext.NoContext               => ""
            case ResolvedServiceContext.AbstractContext(tn, pn) => s"$pn: $tn, "
            case ResolvedServiceContext.ConcreteContext(tn, pn) => s"$pn: $tn, "
          }
          val methods = service.methods.map {
            m =>
              val inType  = domainTypes.asPyRef(m.sig, fileTools.definitionsBasePkg)
              val outType = m.out.map(domainTypes.asPyRef(_, fileTools.definitionsBasePkg))
              val errType = m.err.map(domainTypes.asPyRef(_, fileTools.definitionsBasePkg))
              val retAnnotation: TextTree[PyValue] = if (resolved.noErrors || errType.isEmpty) {
                outType.getOrElse(q"None")
              } else {
                val pyName: PyValue => String = { case t: PyValue.PyType => t.name }
                val outStr                    = outType.map(_.mapRender(pyName)).getOrElse("")
                val errStr                    = errType.map(_.mapRender(pyName))
                val retStr                    = resolved.renderReturnType(outStr, errStr, "None")
                q"${"\"" + retStr + "\""}"
              }
              val methodDocstring = pyTreeTools.renderMethodDocstring(m.docs, "        ")
              val methodDocTree: Option[TextTree[PyValue]] =
                if (methodDocstring.isEmpty) None else Some(q"$methodDocstring")
              val methodBodyParts: List[TextTree[PyValue]] =
                methodDocTree.toList :+ q"raise NotImplementedError"
              val methodBodyTree = methodBodyParts.joinN()
              val asyncKw        = if (target.language.asyncServices) "async " else ""
              val methodPyName   = escapePyKeyword(m.name.name)
              q"""|@$pyAbstractMethod
                  |${asyncKw}def $methodPyName(self, ${ctxParam}arg: $inType) -> $retAnnotation:
                  |    ${methodBodyTree.shift(4).trim}
                  |""".stripMargin
          }
          val serviceDocstring = pyTreeTools.renderClassDocstring(defn.docs, Seq.empty, "    ")
          val serviceDocTree: Option[TextTree[PyValue]] =
            if (serviceDocstring.isEmpty) None else Some(q"$serviceDocstring")
          val serviceBody = serviceDocTree.toList ++ (if (methods.isEmpty) List(q"pass") else methods)
          val allMethods  = serviceBody.joinN()
          // Abstract mode: the service interface is `Generic[Ctx]`. `Generic`
          // and `TypeVar` must be imported (reference them as PyTypes so the
          // import pass emits `from typing import Generic, TypeVar`) and the
          // `Ctx = TypeVar("Ctx")` declaration emitted at module scope so the
          // `Generic[Ctx]` base binds. Concrete/none keep the bare `ABC` base
          // and emit nothing extra (byte-identical).
          val (classBases, ctxTypeVarDecl): (TextTree[PyValue], Option[TextTree[PyValue]]) = resolvedCtx match {
            case ResolvedServiceContext.AbstractContext(tn, _) =>
              (q"$pyABC, $pyGeneric[$tn]", Some(q"""$tn = $pyTypeVar("$tn")"""))
            case _ =>
              (q"$pyABC", None)
          }
          val serviceClass =
            q"""|class ${service.id.name.name.capitalize}($classBases):
                |    ${allMethods.shift(4).trim}
                |""".stripMargin
          PyDefnRepr(
            (ctxTypeVarDecl.toSeq :+ serviceClass).joinNN(),
            List.empty,
          )
        case f: Typedef.Foreign => makeForeignKeyCodecRepr(f)
      }

    }

    /** PR-I.2 (M24 Phase 3.2) — emit a `<Foreign>_KeyCodec` extension hook for
      * every Custom-mapped Python foreign declaration. The host application
      * registers an implementation at boot which the JSON codec then uses to
      * encode/decode map keys. For BaboonRef-mapped foreigns we emit nothing —
      * the existing recursion into the aliased type covers the codec needs.
      *
      * Stringy foreigns (`builtins.str` / `str`) get a default identity impl so
      * the common case works out of the box. Non-stringy foreigns get a stub
      * default that raises `BaboonCodecException.DecoderFailure` with an
      * FQN-bearing diagnostic referring to the Host class (PR-I.1b-D01 lesson).
      */
    private def makeForeignKeyCodecRepr(f: Typedef.Foreign): PyDefnRepr = {
      f.bindings.get(BaboonLang.Py) match {
        case None                                                                  => PyDefnRepr(q"", List.empty)
        case Some(Typedef.ForeignEntry(_, _: Typedef.ForeignMapping.BaboonRef))    => PyDefnRepr(q"", List.empty)
        case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.Custom(decl, _))) =>
          // Names: the user-facing identifier (e.g. "FStr") is taken from the keep-foreigns
          // accessor; the runtime Python type is the deref'd `pyRef` (e.g. `str` for `builtins.str`).
          // Foreign types are not emitted as Python typealiases (see line 90+ of mkRepr — pre-PR
          // emitted q"" for Foreign), so the user-facing name "FStr" doesn't exist as a Python
          // identifier. Methods accept/return the runtime Python type directly.
          val srcRef          = domainTypes.asPyTypeKeepForeigns(f.id, fileTools.definitionsBasePkg)
          val pyRef           = domainTypes.asPyType(f.id, fileTools.definitionsBasePkg)
          val codecName       = s"${srcRef.name}_KeyCodec"
          val hostName        = s"${srcRef.name}_KeyCodecHost"
          val hostFqn         = s"${srcRef.moduleId.path.toList.mkString(".")}.$hostName"
          val instanceVar     = s"_${codecName}_instance"
          val defaultImplName = s"_Default$codecName"
          // Stringy allowlist (PR-I-D06 pattern guidance: only the language's allowlist; no dead alternatives).
          val isStringy = decl == "builtins.str" || decl == "str"
          val defaultImpl = if (isStringy) {
            q"""class $defaultImplName:
               |    def encode_key(self, value: $pyRef) -> $pyStr:
               |        return value
               |    def decode_key(self, s: $pyStr) -> $pyRef:
               |        return s""".stripMargin
          } else {
            q"""class $defaultImplName:
               |    def encode_key(self, value: $pyRef) -> $pyStr:
               |        raise $baboonCodecException.DecoderFailure(f"$hostFqn is not registered; call $hostFqn.register(impl) at app boot.")
               |    def decode_key(self, s: $pyStr) -> $pyRef:
               |        raise $baboonCodecException.DecoderFailure(f"$hostFqn is not registered; call $hostFqn.register(impl) at app boot.")""".stripMargin
          }
          val tree =
            q"""class $codecName($pyProtocol):
               |    def encode_key(self, value: $pyRef) -> $pyStr: ...
               |    def decode_key(self, s: $pyStr) -> $pyRef: ...
               |
               |$defaultImpl
               |
               |$instanceVar: $codecName = $defaultImplName()
               |
               |class $hostName:
               |    @$pyStaticMethod
               |    def register(impl: $codecName) -> None:
               |        global $instanceVar
               |        $instanceVar = impl
               |    @$pyStaticMethod
               |    def instance() -> $codecName:
               |        return $instanceVar""".stripMargin
          PyDefnRepr(tree, List.empty)
      }
    }

    private final case class DtoFnLowering(
      fields: List[TextTree[PyValue]],
      classMembers: List[TextTree[PyValue]],
      moduleHelpers: List[TextTree[PyValue]],
    )

    private def genDtoFields(dto: Typedef.Dto, contractsFields: Set[Field]): DtoFnLowering = {
      val className     = dto.id.name.name.capitalize
      val classMembers  = List.newBuilder[TextTree[PyValue]]
      val moduleHelpers = List.newBuilder[TextTree[PyValue]]
      val fields = dto.fields.flatMap {
        field =>
          // `computed` fields are no-storage: they are exposed as a read-only
          // `@property` (see `renderPyComputedProperty`) and never become a
          // pydantic field. The JSON/UEBA codecs already exclude them from
          // the wire for the same reason.
          field.computed match {
            case Some(fn) =>
              classMembers += renderPyComputedProperty(field, fn)
              Nil
            case None =>
              val plan      = PyFieldPlan(field, contractsFields.contains(field))
              val fieldName = plan.wireName
              val fieldType = domainTypes.asPyRef(field.tpe, fileTools.definitionsBasePkg)
              val defaultLowering = field.default.map(renderPyDefault(_, className, field))
              defaultLowering.foreach(d => moduleHelpers ++= d.helpers)
              // Statement-bodied `@default(fn)` lowers to a module-level
              // helper: zero-arg helpers plug into `default_factory=`, while a
              // present capture root keeps the lambda-argument shape of the
              // pure path by wrapping the helper call.
              val defaultFactory = defaultLowering.flatMap(_.factory)
              val defaultValue   = defaultLowering.map(_.tree).filter(_ => defaultFactory.isEmpty)
              val fieldTree = (plan.needsAlias, defaultValue, defaultFactory) match {
                case (true, _, Some(PyDefaultFactory(helper, Nil))) =>
                  q"${plan.attributeName}: $fieldType = $pydanticField(default_factory=$helper, alias='$fieldName', serialization_alias='$fieldName')"
                case (false, _, Some(PyDefaultFactory(helper, Nil))) =>
                  q"${plan.attributeName}: $fieldType = $pydanticField(default_factory=$helper)"
                case (_, _, Some(PyDefaultFactory(helper, args))) =>
                  val callArgs = args.map(escapePyKeyword).mkString(", ")
                  val wrapped  = q"(lambda $callArgs: $helper($callArgs))"
                  if (plan.needsAlias)
                    q"${plan.attributeName}: $fieldType = $pydanticField(default=$wrapped, alias='$fieldName', serialization_alias='$fieldName')"
                  else
                    q"${plan.attributeName}: $fieldType = $wrapped"
                case (true, Some(dv), None) =>
                  q"${plan.attributeName}: $fieldType = $pydanticField(default=$dv, alias='$fieldName', serialization_alias='$fieldName')"
                case (true, None, None) =>
                  q"${plan.attributeName}: $fieldType = $pydanticField(alias='$fieldName', serialization_alias='$fieldName')"
                case (false, Some(dv), None) =>
                  q"${plan.attributeName}: $fieldType = $dv"
                case (false, None, None) =>
                  q"${plan.attributeName}: $fieldType"
              }
              // `@normalize` runs as a post-init/read hook after construction
              // (see `renderPyNormalizeValidator`). Nested DTOs validate (and
              // normalize) themselves before the parent's hook runs, so
              // children are normalized bottom-up first, structurally.
              field.normalize.foreach {
                nfn => classMembers += renderPyNormalizeValidator(field, plan, nfn)
              }
              field.migrate.foreach {
                m => moduleHelpers += renderPyMigrateFn(className, field, m)
              }
              List(fieldTree)
          }
      }
      DtoFnLowering(
        fields = if (fields.isEmpty) List(q"pass") else fields,
        classMembers = classMembers.result(),
        moduleHelpers = moduleHelpers.result(),
      )
    }

    private def renderPyString(s: String): String = {
      val escaped = s.flatMap {
        case '"'  => "\\\""
        case '\\' => "\\\\"
        case '\n' => "\\n"
        case '\r' => "\\r"
        case '\t' => "\\t"
        case c    => c.toString
      }
      s"\"$escaped\""
    }

    private final case class PyDefaultFactory(helperName: String, captureArgs: List[String])
    private final case class PyDefaultLowering(
      tree: TextTree[PyValue],
      helpers: List[TextTree[PyValue]],
      factory: Option[PyDefaultFactory] = None,
    )

    private def renderPyDefault(fd: FieldDefault, ownerClass: String, field: Field): PyDefaultLowering = {
      def leaf(tree: TextTree[PyValue]): PyDefaultLowering = PyDefaultLowering(tree, Nil)
      fd match {
        case FieldDefault.IntVal(_, raw, _) =>
          leaf(q"$raw")
        case FieldDefault.FloatVal(_, raw) =>
          leaf(q"$raw")
        case FieldDefault.BoolVal(true)  => leaf(q"True")
        case FieldDefault.BoolVal(false) => leaf(q"False")
        case FieldDefault.StrVal(value) =>
          leaf(q"${renderPyString(value)}")
        case FieldDefault.NullVal =>
          leaf(q"None")
        case FieldDefault.EnumVal(memberName) =>
          val enumTpe     = domainTypes.asPyType(field.tpe.id, fileTools.definitionsBasePkg)
          val memberIdent = escapePyKeyword(memberName.split('.').last)
          leaf(q"$enumTpe.$memberIdent")
        case FieldDefault.SpecialFloatVal(kind) =>
          kind match {
            case FieldDefault.SpecialFloatKind.NaN    => leaf(q"float('nan')")
            case FieldDefault.SpecialFloatKind.PosInf => leaf(q"float('inf')")
            case FieldDefault.SpecialFloatKind.NegInf => leaf(q"float('-inf')")
          }
        case FieldDefault.Fn(fn) =>
          renderPyDefaultFn(fn, ownerClass, field)
      }
    }

    // Python slice: inline `@default(fn)` lowering for leaf-typed fns.
    // Zero-arg fns are emitted as immediately-invoked lambdas in
    // `Field(default=...)` position so pydantic sees a plain value default; a
    // present capture root becomes the lambda's argument. Only
    // pure-expression bodies over BinOp/Call/If/Lit/Path (plus unary `!`/`-`)
    // are supported; everything else (lets, loops, match, blocks, free
    // variables, non-leaf types) is a clear per-backend error. Other backends
    // keep their own not-yet-lowered error. `normalize`/`migrate`/`computed`
    // are rejected in `genDtoFields`.
    private def renderPyDefaultFn(fn: TypedFn, ownerClass: String, field: Field): PyDefaultLowering = {
      def err(what: String): Nothing =
        sys.error(s"@default(fn) for field `${field.name.name}`: $what is not yet lowered for the python backend")
      fn.expected match {
        case TypeRef.Scalar(_: TypeId.BuiltinScalar) => ()
        case other                                   => err(s"non-leaf default type $other")
      }
      val nonAssertStmts = fn.body.stmts.filter {
        case io.septimalmind.baboon.typer.model.TypedStmt.Assert(_) => false
        case _                                                      => true
      }
      if (nonAssertStmts.isEmpty) {
        val argNames = fn.capture.map(_.name).toList
        val result   = renderPyFnExpr(fn.body.result, argNames, field)
        val asserts  = fn.body.assertions.map(a => renderPyAssertTrace(a, argNames, field))
        val body =
          if (asserts.isEmpty) result
          else q"(${(asserts :+ result).join(", ")})[-1]"
        val tree =
          if (argNames.isEmpty) q"(lambda: $body)()"
          else q"(lambda ${argNames.map(escapePyKeyword).mkString(", ")}: $body)"
        PyDefaultLowering(tree, Nil)
      } else {
        // Statement bodies (`let`, assignment, loops, `match`, blocks) cannot
        // fit in an inline lambda, so they lower to a module-level helper
        // function with real statements; the expression result is kept as the
        // final `return` value. Zero-arg helpers are referenced through
        // `Field(default_factory=...)`; a present capture root keeps the
        // lambda-argument shape of the pure path by wrapping the helper call.
        val fieldType = domainTypes.asPyRef(field.tpe, fileTools.definitionsBasePkg)
        val argNames  = fn.capture.map(_.name).toList
        val helperName = s"_baboon_default_${ownerClass}_${field.name.name}"
        val lowering   = new PyFnStmtLowering(field, "@default(fn)")
        val (stmtLines, result) = lowering.lowerFn(fn, argNames, Nil)
        val allLines = stmtLines :+ q"return $result"
        val helperArgs = argNames.map(escapePyKeyword).mkString(", ")
        val helper =
          q"""def $helperName($helperArgs) -> $fieldType:
             |    ${allLines.joinN().shift(4).trim}
             |""".stripMargin
        // The inner default expression is unused on this path (`genDtoFields`
        // renders `default_factory=` / a wrapping lambda from `factory`);
        // keep a placeholder so `tree` stays total.
        PyDefaultLowering(q"None", List(helper), Some(PyDefaultFactory(helperName, argNames)))
      }
    }

    private def renderPyFnExpr(expr: TypedExpr, args: List[String], field: Field): TextTree[PyValue] = {
      def err(what: String): Nothing =
        sys.error(s"@default(fn) for field `${field.name.name}`: $what is not yet lowered for the python backend")
      expr match {
        case TypedExpr.Lit(lit, _) =>
          lit match {
            case TypedLit.IntLit(v)   => q"${v.toString}"
            case TypedLit.FloatLit(v) =>
              if (v.isNaN) q"float('nan')"
              else if (v.isPosInfinity) q"float('inf')"
              else if (v.isNegInfinity) q"float('-inf')"
              else q"${v.toString}"
            case TypedLit.BoolLit(v) => if (v) q"True" else q"False"
            case TypedLit.StrLit(v)  => q"${renderPyString(v)}"
            case TypedLit.NullLit()  => q"None"
          }
        case TypedExpr.Path(root, segments, _) =>
          if (!args.contains(root)) err(s"free variable `$root` (only the capture root is in scope)")
          segments.foldLeft[TextTree[PyValue]](q"${escapePyKeyword(root)}") {
            case (acc, TypedPathSeg.Field(name, _))  => q"$acc.${escapePyKeyword(name)}"
            case (acc, TypedPathSeg.Index(index, _)) => q"$acc[${renderPyFnExpr(index, args, field)}]"
          }
        case TypedExpr.BinOp(op, left, right, _) =>
          val pyOp = op match {
            case "&&" => "and"
            case "||" => "or"
            case "==" | "!=" | "<" | "<=" | ">" | ">=" | "+" | "-" | "*" | "/" | "%" => op
            case _    => err(s"binary operator `$op`")
          }
          q"(${renderPyFnExpr(left, args, field)} $pyOp ${renderPyFnExpr(right, args, field)})"
        case TypedExpr.UnaryOp(op, inner, _) =>
          op match {
            case "!" => q"(not ${renderPyFnExpr(inner, args, field)})"
            case "-" => q"(-${renderPyFnExpr(inner, args, field)})"
            case _   => err(s"unary operator `$op`")
          }
        case TypedExpr.If(cond, thenE, elseE, _) =>
          q"(${renderPyFnExpr(thenE, args, field)} if ${renderPyFnExpr(cond, args, field)} else ${renderPyFnExpr(elseE, args, field)})"
        case TypedExpr.Call(target, method, callArgs, _) =>
          renderPyFnCall(renderPyFnExpr(target, args, field), method, callArgs.map(renderPyFnExpr(_, args, field)), field)
        case other =>
          err(s"expression `${other.getClass.getSimpleName}`")
      }
    }

    private def renderPyFnCall(
      target: TextTree[PyValue],
      method: String,
      args: List[TextTree[PyValue]],
      field: Field,
    ): TextTree[PyValue] = {
      def err(what: String): Nothing =
        sys.error(s"@default(fn) for field `${field.name.name}`: $what is not yet lowered for the python backend")
      def one: TextTree[PyValue] = args match {
        case List(a) => a
        case _       => err(s"method `$method` arity ${args.size}")
      }
      method match {
        case "uppercase"                    => q"$target.upper()"
        case "lowercase"                    => q"$target.lower()"
        case "capitalize"                   => q"$target.capitalize()"
        case "trim"                         => q"$target.strip()"
        case "toString" if args.isEmpty     => q"str($target)"
        case "length" if args.isEmpty       => q"len($target)"
        case "split"                        => q"$target.split($one)"
        case "contains"                     => q"($one in $target)"
        case "startsWith"                   => q"$target.startswith($one)"
        case "endsWith"                     => q"$target.endswith($one)"
        case "concat"                       => q"($target + $one)"
        case "join"                         => q"($one.join($target))"
        case "abs" if args.isEmpty          => q"abs($target)"
        case "round" if args.isEmpty        => q"round($target)"
        case "min"                          => q"min($target, $one)"
        case "max"                          => q"max($target, $one)"
        case "floor" | "ceil"               => err(s"method `$method` (needs math import)")
        case _                              => err(s"method `$method`")
      }
    }

    private def renderPyAssertTrace(assert: TypedAssert, args: List[String], field: Field): TextTree[PyValue] = {
      val cond = renderPyFnExpr(assert.cond, args, field)
      val vars =
        assert.capturedVars.distinct.map(v => s"'$v': ${escapePyKeyword(v)}").mkString("{", ", ", "}")
      val msg = assert.msg.map(m => renderPyFnExpr(m, args, field)).getOrElse(q"None")
      q"$pyAssertTrace($cond, $vars, $msg)"
    }

    // Full statement lowering for context fns (statement-bodied `@default`,
    // `@normalize`, `@migrate`, computed). Pure expressions reuse
    // `renderPyFnExpr` so leaf rendering stays identical; everything else
    // lowers to explicit statements while the expression result is kept as
    // the final value (`return` / temp assignment). One instance per fn:
    // `scope` accumulates `let`/loop/match bindings for the same free-variable
    // check the pure path performs, `counter` mints fresh temp names.
    private final class PyFnStmtLowering(field: Field, role: String) {
      private var scope: List[String] = Nil
      private var counter: Int        = 0
      private def err(what: String): Nothing =
        sys.error(s"$role for field `${field.name.name}`: $what is not yet lowered for the python backend")

      private def fresh(prefix: String): String = {
        val id = counter
        counter += 1
        s"_baboon_${prefix}_$id"
      }

      private def bind(name: String): Unit =
        if (!scope.contains(name)) scope = scope :+ name

      def lowerFn(
        fn: TypedFn,
        initialScope: List[String],
        prelude: List[TextTree[PyValue]],
      ): (List[TextTree[PyValue]], TextTree[PyValue]) = {
        scope = initialScope
        counter = 0
        val head = prelude ++ lowerStmtList(fn.body.stmts)
        val (valueLines, result) = lowerValue(fn.body.result)
        (head ++ valueLines, result)
      }

      private def lowerStmtList(stmts: List[TypedStmt]): List[TextTree[PyValue]] =
        stmts.flatMap(lowerStmt)

      private def lowerStmt(stmt: TypedStmt): List[TextTree[PyValue]] = stmt match {
        case TypedStmt.Let(name, _, value) =>
          val (lines, expr) = lowerValue(value)
          bind(name)
          lines :+ q"${escapePyKeyword(name)} = $expr"
        case TypedStmt.Effect(expr) => lowerStmtExpr(expr)
        case TypedStmt.Assert(a)    => List(renderPyAssertTrace(a, scope, field))
      }

      private def lowerStmtExpr(expr: TypedExpr): List[TextTree[PyValue]] = expr match {
        case a: TypedExpr.Assign => lowerAssign(a)._1
        case f: TypedExpr.ForIn  => renderForIn(f)
        case f: TypedExpr.ForC   => renderForC(f)
        case m: TypedExpr.Match  => renderMatch(m, needValue = false)._1
        case b: TypedExpr.Block  => lowerStmtList(b.stmts) ++ b.value.toList.flatMap(lowerStmtExpr)
        case other               => List(renderPyFnExpr(other, scope, field))
      }

      private def lowerValue(expr: TypedExpr): (List[TextTree[PyValue]], TextTree[PyValue]) = expr match {
        case a: TypedExpr.Assign => lowerAssign(a)
        case m: TypedExpr.Match  => renderMatch(m, needValue = true)
        case b: TypedExpr.Block  =>
          val head = lowerStmtList(b.stmts)
          b.value match {
            case Some(v) =>
              val (vLines, r) = lowerValue(v)
              (head ++ vLines, r)
            case None => err("block without a value in value position")
          }
        case i: TypedExpr.If if !isPureExpr(i) => renderIfValue(i)
        case _: TypedExpr.ForIn                => err("loop in value position")
        case _: TypedExpr.ForC                 => err("loop in value position")
        case other                             => (Nil, renderPyFnExpr(other, scope, field))
      }

      private def isPureExpr(expr: TypedExpr): Boolean = expr match {
        case _: TypedExpr.Lit  => true
        case _: TypedExpr.Path => true
        case TypedExpr.Call(t, _, args, _)   => isPureExpr(t) && args.forall(isPureExpr)
        case TypedExpr.BinOp(_, l, r, _)     => isPureExpr(l) && isPureExpr(r)
        case TypedExpr.UnaryOp(_, e, _)       => isPureExpr(e)
        case TypedExpr.If(c, t, e, _)         => isPureExpr(c) && isPureExpr(t) && isPureExpr(e)
        case _                               => false
      }

      private def renderLValue(target: TypedExpr): TextTree[PyValue] = target match {
        case TypedExpr.Path(root, segments, _) =>
          if (!scope.contains(root)) err(s"free variable `$root` (only bound names are in scope)")
          segments.foldLeft[TextTree[PyValue]](q"${escapePyKeyword(root)}") {
            case (acc, TypedPathSeg.Field(name, _))  => q"$acc.${escapePyKeyword(name)}"
            case (acc, TypedPathSeg.Index(index, _)) => q"$acc[${renderPyFnExpr(index, scope, field)}]"
          }
        case other => err(s"cannot assign to `${other.getClass.getSimpleName}`")
      }

      private def lowerAssign(a: TypedExpr.Assign): (List[TextTree[PyValue]], TextTree[PyValue]) = {
        val (valueLines, rhs) = lowerValue(a.value)
        val lval              = renderLValue(a.target)
        (valueLines :+ q"$lval = $rhs", lval)
      }

      // `let total = 0; for (i = 0; i < 10; i = i + 1) { total = total + i; }`
      // lowers to a `for ... in ...:` loop over the iterable.
      private def renderForIn(f: TypedExpr.ForIn): List[TextTree[PyValue]] = {
        val (iterLines, iter) = lowerValue(f.iterable)
        bind(f.varName)
        val bodyLines = lowerStmtList(f.body.stmts) ++ f.body.value.toList.flatMap(lowerStmtExpr)
        val body      = if (bodyLines.isEmpty) q"pass" else bodyLines.joinN()
        iterLines :+ q"""for ${escapePyKeyword(f.varName)} in $iter:
                        |    ${body.shift(4).trim}""".stripMargin
      }

      // C-style `for (i = 0; i < 10; i = i + 1) { ... }` lowers to the
      // equivalent `while` loop: init, condition, body, step-as-statement.
      // Appending the step after the body is sound: the language has no
      // `break`/`continue`, so the step always executes exactly once per
      // iteration, in order.
      private def renderForC(f: TypedExpr.ForC): List[TextTree[PyValue]] = {
        val (initLines, initV) = lowerValue(f.init.value)
        bind(f.init.name)
        val initLine = q"${escapePyKeyword(f.init.name)} = $initV"
        val (condLines, cond) = lowerValue(f.cond)
        val stepLines         = lowerStmtExpr(f.step)
        val bodyLines         = lowerStmtList(f.body.stmts) ++ f.body.value.toList.flatMap(lowerStmtExpr)
        val loopLines         = bodyLines ++ stepLines
        val loopBody          = if (loopLines.isEmpty) q"pass" else loopLines.joinN()
        val loop = q"""while $cond:
                      |    ${loopBody.shift(4).trim}""".stripMargin
        initLines ++ List(initLine) ++ condLines ++ List(loop)
      }

      // `match` lowers to an `if`/`elif`/`else` chain on a scrutinee temp
      // (evaluated once). In value position every arm assigns a shared result
      // temp which becomes the expression result; in statement position arm
      // values are emitted as bare expression statements. `opt` scrutinees
      // match `Some(x)`/`None` directly; ADT scrutinees dispatch through the
      // `baboon_match_variant`/`baboon_variant_value` runtime hooks (tolerant
      // of the single-key wrapped wire form). A trailing wildcard becomes
      // `else`; without one the chain ends in an exhaustiveness raise.
      private def renderMatch(m: TypedExpr.Match, needValue: Boolean): (List[TextTree[PyValue]], TextTree[PyValue]) = {
        val (scrutLines, scrut) = lowerValue(m.scrutinee)
        val scrutVar            = fresh("match")
        val armValues           = m.arms.map(_.body.value)
        // The shared result temp is only needed when the match itself is a
        // value; in statement position arm values lower to bare statements.
        val resVar: Option[String] =
          if (needValue && armValues.exists(_.isDefined)) Some(fresh("match_value")) else None
        if (needValue && resVar.isEmpty)
          err("match without arm values in value position")
        val clauses = m.arms.zipWithIndex.map {
          case (arm, idx) =>
            val (keyword, check) = arm.pattern match {
              case TypedPattern.Wildcard =>
                if (idx != m.arms.size - 1) err("unreachable match arm after `else`")
                ("else", None)
              case TypedPattern.Variant(name, binding, _) =>
                val kw = if (idx == 0) "if" else "elif"
                (kw, Some(matchCheck(m.scrutinee.tpe, scrutVar, name, binding)))
            }
            val bindLine = arm.pattern match {
              case TypedPattern.Variant(name, Some(b), _) =>
                bind(b)
                if (FnTypes.isOpt(m.scrutinee.tpe)) List(q"${escapePyKeyword(b)} = $scrutVar": TextTree[PyValue])
                else List(q"${escapePyKeyword(b)} = $pyBaboonVariantValue($scrutVar, ${name.capitalize})": TextTree[PyValue])
              case _ => Nil
            }
            val armLines = bindLine ++ lowerStmtList(arm.body.stmts) ++ (arm.body.value match {
              case Some(v) if needValue && resVar.isDefined =>
                val (vl, r) = lowerValue(v)
                vl :+ q"${resVar.get} = $r"
              case Some(v) =>
                lowerStmtExpr(v)
              case None =>
                if (needValue) err("match arm without a value in value position")
                else Nil
            })
            val bodyTree = if (armLines.isEmpty) q"pass" else armLines.joinN()
            check match {
              case Some(c) =>
                q"""$keyword $c:
                   |    ${bodyTree.shift(4).trim}""".stripMargin
              case None =>
                q"""else:
                   |    ${bodyTree.shift(4).trim}""".stripMargin
            }
        }
        val exhaustive = m.arms.lastOption.exists(_.pattern == TypedPattern.Wildcard)
        val allClauses =
          if (exhaustive) clauses
          else clauses :+ q"""else:
                              |    raise AssertionError("baboon match fell through (non-exhaustive arms)")""".stripMargin
        val lines = scrutLines ++ List(q"$scrutVar = $scrut": TextTree[PyValue]) ++ allClauses
        (lines, resVar.map(r => q"$r": TextTree[PyValue]).getOrElse(q"None"))
      }

      private def matchCheck(scrutTpe: TypeRef, scrutVar: String, name: String, binding: Option[String]): TextTree[PyValue] = {
        if (FnTypes.isOpt(scrutTpe)) name match {
          case "Some" => q"$scrutVar is not None"
          case "None" =>
            if (binding.isDefined) err("`None` match arm cannot bind a value")
            q"$scrutVar is None"
          case other => err(s"unknown option variant `$other`")
        } else scrutTpe match {
          case TypeRef.Scalar(_: TypeId.User) =>
            q"$pyBaboonMatchVariant($scrutVar, ${name.capitalize})"
          case _ =>
            err(s"match on non-variant type `$scrutTpe`")
        }
      }

      private def renderIfValue(i: TypedExpr.If): (List[TextTree[PyValue]], TextTree[PyValue]) = {
        val (condLines, cond) = lowerValue(i.cond)
        val resVar            = fresh("if")
        val (thenLines, thenV) = lowerValue(i.thenE)
        val (elseLines, elseV) = lowerValue(i.elseE)
        val tree =
          q"""if $cond:
             |    ${(thenLines :+ q"$resVar = $thenV").joinN().shift(4).trim}
             |else:
             |    ${(elseLines :+ q"$resVar = $elseV").joinN().shift(4).trim}""".stripMargin
        (condLines :+ tree, q"$resVar")
      }
    }

    private def renderPyNormalizeValidator(field: Field, plan: PyFieldPlan, fn: TypedFn): TextTree[PyValue] = {
      // `@normalize(fn)` runs after construction as a pydantic `mode="after"`
      // validator: the capture root (when present and not `self` itself) is
      // bound to the pre-normalize field value, `self` is the model under
      // validation, and the fn result is written back with
      // `object.__setattr__` (models are frozen, so plain assignment is
      // rejected). Nested models validate — and normalize — before the
      // parent's hook runs, giving bottom-up ordering structurally.
      val lowering = new PyFnStmtLowering(field, "@normalize(fn)")
      val bindCapture = fn.capture.map(_.name).filterNot(_ == "self").toList.map {
        cap => q"${escapePyKeyword(cap)} = self.${plan.attributeName}"
      }
      val (stmtLines, result) = lowering.lowerFn(fn, "self" :: fn.capture.map(_.name).toList, bindCapture)
      val attr = plan.attributeName
      q"""@$pydanticModelValidator(mode="after")
         |def _baboon_normalize_${field.name.name}(self):
         |    ${(stmtLines :+ q"object.__setattr__(self, '$attr', $result)" :+ q"return self").joinN().shift(4).trim}
         |""".stripMargin
    }

    private def renderPyComputedProperty(field: Field, fn: TypedFn): TextTree[PyValue] = {
      // `= fn` computed fields are no-storage: a read-only `@property` getter
      // evaluated on read. The capture root (when present and not `self`
      // itself) aliases the instance; the fn result is the property value.
      // Computed fields never enter `__init__` or the wire codecs.
      val lowering = new PyFnStmtLowering(field, "computed fn")
      val tpe      = domainTypes.asPyRef(fn.expected, fileTools.definitionsBasePkg)
      val propName = escapePyKeyword(field.name.name)
      val bindCapture = fn.capture.map(_.name).filterNot(_ == "self").toList.map {
        cap => q"${escapePyKeyword(cap)} = self"
      }
      val (stmtLines, result) = lowering.lowerFn(fn, "self" :: fn.capture.map(_.name).toList, bindCapture)
      q"""@property
         |def $propName(self) -> $tpe:
         |    ${(stmtLines :+ q"return $result").joinN().shift(4).trim}
         |""".stripMargin
    }

    private def renderPyMigrateFn(ownerClass: String, field: Field, m: TypedMigrate): TextTree[PyValue] = {
      // `@migrate(fn, [...])` lowers to a module-level version function
      // `migrate_<Class>_<field>(...)` taking the capture root (when present);
      // the fn result is the migrated value. Evolution codecs invoke it when
      // reading payloads written before this field's version.
      val fn       = m.fn
      val lowering = new PyFnStmtLowering(field, "@migrate(fn)")
      val tpe      = domainTypes.asPyRef(fn.expected, fileTools.definitionsBasePkg)
      val argNames = fn.capture.map(_.name).toList
      val helperArgs = argNames.map(escapePyKeyword).mkString(", ")
      val (stmtLines, result) = lowering.lowerFn(fn, argNames, Nil)
      q"""def migrate_${ownerClass}_${field.name.name}($helperArgs) -> $tpe:
         |    \"\"\"Baboon `@migrate` version function for field `${field.name.name}`.\"\"\"
         |    ${(stmtLines :+ q"return $result").joinN().shift(4).trim}
         |""".stripMargin
    }

    private def genDtoProperties(contractsFields: List[Field]): Option[List[TextTree[PyValue]]] = {
      if (contractsFields.nonEmpty) {
        val properties = contractsFields.map {
          f =>
            val plan = PyFieldPlan(f, true)
            // The backing pydantic field is always `${fieldName}_` (see genDtoFields).
            // The property accessor uses the keyword-escaped name so the method declaration is
            // valid Python (e.g. `def class_(self)` instead of `def class(self)`).
            val propertyName = plan.accessorName
            q"""@property
               |def $propertyName(self) -> ${domainTypes.asPyRef(f.tpe, fileTools.definitionsBasePkg)}:
               |    return self.${plan.attributeName}
               |""".stripMargin
        }
        Some(properties)
      } else None
    }

    private def genDtoPydanticModelConf(dtoFields: List[Field], hasContracts: Boolean, jsonCodecActive: Boolean): TextTree[PyType] = {
      val frozen           = Some(q"frozen=True")
      val hasKeywordField  = dtoFields.exists(f => PyKeywords.isKeyword(f.name.name))
      val serializeByAlias = if (jsonCodecActive && (hasContracts || hasKeywordField)) Some(q"serialize_by_alias=True") else None
      // populate_by_name=True allows constructing instances using the Python attribute name (the
      // escaped name, e.g. `class_`) instead of the alias (the original model name `class`). This
      // is required when any field is a Python keyword, because the alias cannot be used as a
      // constructor keyword argument in generated Python code.
      val populateByName = if (hasKeywordField) Some(q"populate_by_name=True") else None
      val serializeDecimalAsJsonNumber =
        if (dtoFields.map(_.tpe.id).contains(TypeId.Builtins.f128)) {
          List(q"json_encoders={Decimal: str}")
        } else Nil

      // pydantic doesn't know how to validate the runtime `AnyOpaque` ABC; opt into
      // arbitrary-types when any field carries an `any`-typed payload (direct or nested via a
      // constructor like `lst[any]` / `opt[any]` / `map[str, any]`).
      val arbitraryTypesAllowed =
        if (dtoFields.exists(f => PyFieldPlan.containsAny(f.tpe))) Some(q"arbitrary_types_allowed=True") else None

      val configs = List(frozen, serializeByAlias, populateByName, serializeDecimalAsJsonNumber, arbitraryTypesAllowed).flatten

      q"""model_config = $pydanticConfigDict(
         |    ${configs.join(",\n").shift(4).trim}
         |)""".stripMargin
    }

    private def mkParents(refs: List[PyType]): TextTree[PyValue] = {
      if (refs.isEmpty) q"" else q"${refs.map(s => q"$s").join(", ")}"
    }

    // ----- Identifier toString + parse_repr emission (PR-57d) -----
    // Spec: docs/spec/identifier-repr.md. Mirrors the per-language emitters
    // in PR-57a/b/c. Python idioms:
    //   - `__repr__(self) -> str` method on the dataclass
    //   - sibling `<TypeName>Codec` class with `@staticmethod parse_repr` (Q-FU-4)
    //   - snake_case method names per PEP 8
    private def signedTypeNamePy(tpe: TypeRef): String = tpe match {
      case TypeRef.Scalar(TypeId.Builtins.i08) => "i08"
      case TypeRef.Scalar(TypeId.Builtins.i16) => "i16"
      case TypeRef.Scalar(TypeId.Builtins.i32) => "i32"
      case other                               => throw new IllegalStateException(s"signedTypeNamePy on non-signed-int: $other")
    }

    private def signedRangeCheckPy(tpe: TypeRef, varName: String): String = tpe match {
      case TypeRef.Scalar(TypeId.Builtins.i08) => s"-128 <= $varName <= 127"
      case TypeRef.Scalar(TypeId.Builtins.i16) => s"-32768 <= $varName <= 32767"
      case TypeRef.Scalar(TypeId.Builtins.i32) => s"-2147483648 <= $varName <= 2147483647"
      case other                               => throw new IllegalStateException(s"signedRangeCheckPy on non-signed-int: $other")
    }

    private def unsignedSmallTypeNamePy(tpe: TypeRef): String = tpe match {
      case TypeRef.Scalar(TypeId.Builtins.u08) => "u08"
      case TypeRef.Scalar(TypeId.Builtins.u16) => "u16"
      case TypeRef.Scalar(TypeId.Builtins.u32) => "u32"
      case other                               => throw new IllegalStateException(s"unsignedSmallTypeNamePy on non-u08/u16/u32: $other")
    }

    private def unsignedSmallRangeCheckPy(tpe: TypeRef, varName: String): String = tpe match {
      case TypeRef.Scalar(TypeId.Builtins.u08) => s"0 <= $varName <= 255"
      case TypeRef.Scalar(TypeId.Builtins.u16) => s"0 <= $varName <= 65535"
      case TypeRef.Scalar(TypeId.Builtins.u32) => s"0 <= $varName <= 4294967295"
      case other                               => throw new IllegalStateException(s"unsignedSmallRangeCheckPy on non-u08/u16/u32: $other")
    }

    private def renderIdentifierFieldValueExprPy(pyFieldName: String, kind: IdentifierFieldKind): TextTree[PyValue] = {
      kind match {
        case IdentifierFieldKind.Bit              => q"$baboonIdReprBitToString(self.$pyFieldName)"
        case IdentifierFieldKind.SignedInt        => q"str(self.$pyFieldName)"
        case IdentifierFieldKind.SignedLong       => q"str(self.$pyFieldName)"
        case IdentifierFieldKind.UnsignedSmallInt => q"str(self.$pyFieldName)"
        case IdentifierFieldKind.UnsignedLong     => q"$baboonIdReprU64ToString(self.$pyFieldName)"
        case IdentifierFieldKind.Str              => q"$baboonIdReprEscapeStr(self.$pyFieldName)"
        // UUID's str() is the lowercase canonical form per RFC 4122.
        case IdentifierFieldKind.Uid         => q"str(self.$pyFieldName)"
        case IdentifierFieldKind.Tsu         => q"$baboonIdReprTsuToString(self.$pyFieldName)"
        case IdentifierFieldKind.Tso         => q"$baboonIdReprTsoToString(self.$pyFieldName)"
        case IdentifierFieldKind.Bytes       => q"$baboonIdReprBytesToHex(self.$pyFieldName)"
        case IdentifierFieldKind.NestedId(_) => q"""f'{${"\"{\""}}{self.$pyFieldName!r}{${"\"}\""}}'""".stripMargin
      }
    }

    private def renderIdentifierRepr(dto: Typedef.Dto): TextTree[PyValue] = {
      val simpleName = dto.id.name.name.capitalize
      val versionStr = domain.version.toString
      val header     = s"$simpleName:$versionStr#"

      val fieldExprs: List[TextTree[PyValue]] = dto.fields.map {
        f =>
          val srcFieldName     = f.name.name
          val escapedFieldName = escapePyKeyword(srcFieldName)
          val kind             = IdentifierFieldKind.classify(f.tpe)
          val valueExpr        = renderIdentifierFieldValueExprPy(escapedFieldName, kind)
          q""""$srcFieldName:" + ($valueExpr)"""
      }

      val joinedFields =
        if (fieldExprs.isEmpty) q""""""""
        else fieldExprs.toSeq.join(""" + ":" + """)

      q"""def __repr__(self) -> str:
         |    return "$header" + $joinedFields
         |
         |def __str__(self) -> str:
         |    return self.__repr__()
         |""".stripMargin
    }

    private def renderIdentifierCodecClass(dto: Typedef.Dto): TextTree[PyValue] = {
      val simpleName     = dto.id.name.name.capitalize
      val versionStr     = domain.version.toString
      val codecClassName = s"${simpleName}Codec"

      val fieldDecoders: List[TextTree[PyValue]] = dto.fields.zipWithIndex.map {
        case (f, idx) =>
          val srcFieldName = f.name.name
          val rawVar       = s"${srcFieldName}_raw"
          val valVar       = s"${srcFieldName}_v"
          val resVar       = s"${srcFieldName}_r"
          val isLast       = idx == dto.fields.length - 1
          val kind         = IdentifierFieldKind.classify(f.tpe)

          val parseHead =
            q"""${srcFieldName}_fnr = $baboonIdReprParseFieldName(cursor, "$srcFieldName")
               |if isinstance(${srcFieldName}_fnr, $baboonLeftType):
               |    return ${srcFieldName}_fnr""".stripMargin

          val parseValue: TextTree[PyValue] = kind match {
            case IdentifierFieldKind.Bit =>
              q"""$rawVar = cursor.read_until_structural()
                 |$resVar = $baboonIdReprParseBit($rawVar)
                 |if isinstance($resVar, $baboonLeftType):
                 |    return $resVar
                 |$valVar = $resVar.value""".stripMargin
            case IdentifierFieldKind.SignedInt =>
              val typeName   = signedTypeNamePy(f.tpe)
              val parsedVar  = s"${srcFieldName}_n"
              val rangeCheck = signedRangeCheckPy(f.tpe, parsedVar)
              q"""$rawVar = cursor.read_until_structural()
                 |# Spec §5.4: signed integers must not carry a leading '+'.
                 |if $rawVar and $rawVar[0] == '+':
                 |    return $baboonLeftType(f"signed integer must not have leading '+' for field $srcFieldName: {$rawVar}")
                 |try:
                 |    $parsedVar = int($rawVar)
                 |except ValueError:
                 |    return $baboonLeftType(f"could not parse signed integer for field $srcFieldName: {$rawVar}")
                 |if not ($rangeCheck):
                 |    return $baboonLeftType(f"$typeName out of range for field $srcFieldName: {$rawVar}")
                 |$valVar = $parsedVar""".stripMargin
            case IdentifierFieldKind.SignedLong =>
              // Python ints are arbitrary precision. i64 range check elided per
              // PR-57a-D01 carryover (no dead `if not (always-true)` block).
              q"""$rawVar = cursor.read_until_structural()
                 |# Spec §5.4: signed integers must not carry a leading '+'.
                 |if $rawVar and $rawVar[0] == '+':
                 |    return $baboonLeftType(f"signed integer must not have leading '+' for field $srcFieldName: {$rawVar}")
                 |try:
                 |    $valVar = int($rawVar)
                 |except ValueError:
                 |    return $baboonLeftType(f"could not parse i64 for field $srcFieldName: {$rawVar}")""".stripMargin
            case IdentifierFieldKind.UnsignedSmallInt =>
              val typeName   = unsignedSmallTypeNamePy(f.tpe)
              val parsedVar  = s"${srcFieldName}_n"
              val rangeCheck = unsignedSmallRangeCheckPy(f.tpe, parsedVar)
              q"""$rawVar = cursor.read_until_structural()
                 |if $rawVar and $rawVar[0] in ('+', '-'):
                 |    return $baboonLeftType(f"unsigned value has leading sign for field $srcFieldName: {$rawVar}")
                 |try:
                 |    $parsedVar = int($rawVar)
                 |except ValueError:
                 |    return $baboonLeftType(f"could not parse unsigned integer for field $srcFieldName: {$rawVar}")
                 |if not ($rangeCheck):
                 |    return $baboonLeftType(f"$typeName out of range for field $srcFieldName: {$rawVar}")
                 |$valVar = $parsedVar""".stripMargin
            case IdentifierFieldKind.UnsignedLong =>
              val parsedVar = s"${srcFieldName}_n"
              q"""$rawVar = cursor.read_until_structural()
                 |if $rawVar and $rawVar[0] in ('+', '-'):
                 |    return $baboonLeftType(f"unsigned value has leading sign for field $srcFieldName: {$rawVar}")
                 |try:
                 |    $parsedVar = int($rawVar)
                 |except ValueError:
                 |    return $baboonLeftType(f"could not parse u64 for field $srcFieldName: {$rawVar}")
                 |if not (0 <= $parsedVar <= 18446744073709551615):
                 |    return $baboonLeftType(f"u64 out of range for field $srcFieldName: {$rawVar}")
                 |$valVar = $parsedVar""".stripMargin
            case IdentifierFieldKind.Str =>
              q"""$resVar = cursor.read_str_field()
                 |if isinstance($resVar, $baboonLeftType):
                 |    return $resVar
                 |$valVar = $resVar.value""".stripMargin
            case IdentifierFieldKind.Uid =>
              q"""$rawVar = cursor.read_until_structural()
                 |if not $baboonIdReprIsCanonicalUid($rawVar):
                 |    return $baboonLeftType(f"uid not in canonical lowercase form for field $srcFieldName: {$rawVar}")
                 |try:
                 |    $valVar = $pyUuid($rawVar)
                 |except ValueError:
                 |    return $baboonLeftType(f"could not parse uid for field $srcFieldName: {$rawVar}")""".stripMargin
            case IdentifierFieldKind.Tsu =>
              q"""${srcFieldName}_rrf = cursor.read_fixed(24)
                 |if isinstance(${srcFieldName}_rrf, $baboonLeftType):
                 |    return ${srcFieldName}_rrf
                 |$rawVar = ${srcFieldName}_rrf.value
                 |$resVar = $baboonIdReprParseTsu($rawVar)
                 |if isinstance($resVar, $baboonLeftType):
                 |    return $resVar
                 |$valVar = $resVar.value""".stripMargin
            case IdentifierFieldKind.Tso =>
              q"""${srcFieldName}_rrf = cursor.read_fixed(29)
                 |if isinstance(${srcFieldName}_rrf, $baboonLeftType):
                 |    return ${srcFieldName}_rrf
                 |$rawVar = ${srcFieldName}_rrf.value
                 |$resVar = $baboonIdReprParseTso($rawVar)
                 |if isinstance($resVar, $baboonLeftType):
                 |    return $resVar
                 |$valVar = $resVar.value""".stripMargin
            case IdentifierFieldKind.Bytes =>
              q"""$rawVar = cursor.read_until_structural()
                 |$resVar = $baboonIdReprParseBytesHex($rawVar)
                 |if isinstance($resVar, $baboonLeftType):
                 |    return $resVar
                 |$valVar = $resVar.value""".stripMargin
            case IdentifierFieldKind.NestedId(uid) =>
              val nestedTpe   = domainTypes.asPyTypeKeepForeigns(uid, fileTools.definitionsBasePkg)
              val nestedCodec = PyType(nestedTpe.moduleId, s"${nestedTpe.name}Codec")
              q"""${srcFieldName}_ro = cursor.expect("{")
                 |if isinstance(${srcFieldName}_ro, $baboonLeftType):
                 |    return ${srcFieldName}_ro
                 |$resVar = $nestedCodec.parse_repr_cursor(cursor)
                 |if isinstance($resVar, $baboonLeftType):
                 |    return $resVar
                 |$valVar = $resVar.value
                 |${srcFieldName}_rc = cursor.expect("}")
                 |if isinstance(${srcFieldName}_rc, $baboonLeftType):
                 |    return ${srcFieldName}_rc""".stripMargin
          }

          val sep =
            if (isLast) q""
            else
              q"""${srcFieldName}_rsep = cursor.expect(":")
                 |if isinstance(${srcFieldName}_rsep, $baboonLeftType):
                 |    return ${srcFieldName}_rsep""".stripMargin

          q"""$parseHead
             |$parseValue
             |$sep""".stripMargin.trim
      }

      val ctorArgs = dto.fields.map {
        f =>
          // Use the keyword-escaped attribute name as the constructor kwarg.
          // Non-keyword fields: `foo=foo_v` (unchanged from before).
          // Keyword fields: `class_=class_v` (requires populate_by_name=True in model_config).
          val attrName = if (PyKeywords.isKeyword(f.name.name)) s"${f.name.name}_" else f.name.name
          q"$attrName=${f.name.name}_v"
      }

      val ctor =
        if (ctorArgs.nonEmpty)
          q"""$simpleName(
             |    ${ctorArgs.join(",\n").shift(4).trim}
             |)""".stripMargin
        else q"$simpleName()"

      val body = (fieldDecoders :+ q"return $baboonRightType($ctor)").joinNN()

      val bodyForEmpty = if (fieldDecoders.isEmpty) {
        // Empty-fields id: just emit the construction so we don't end up with
        // an empty function body (Python requires at least `pass` or a stmt).
        q"return $baboonRightType($ctor)"
      } else body

      q"""class $codecClassName:
         |    \"\"\"Schema-directed parser for the canonical identifier repr per
         |    docs/spec/identifier-repr.md. NOT a method on the dataclass — Q-FU-4.
         |    \"\"\"
         |
         |    @$pyStaticMethod
         |    def parse_repr(s: str) -> $baboonEitherType:
         |        cursor = $baboonIdReprCursor(s)
         |        inner = $codecClassName.parse_repr_cursor(cursor)
         |        if isinstance(inner, $baboonLeftType):
         |            return inner
         |        if not cursor.at_end():
         |            return $baboonLeftType(f"unexpected trailing input at {cursor.position()}")
         |        return inner
         |
         |    @$pyStaticMethod
         |    def parse_repr_cursor(cursor: $baboonIdReprCursor) -> $baboonEitherType:
         |        h = $baboonIdReprParseHeader(cursor, "$simpleName", "$versionStr")
         |        if isinstance(h, $baboonLeftType):
         |            return h
         |        ${bodyForEmpty.shift(8).trim}
         |""".stripMargin
    }

    private def getOutputPath(defn: DomainMember.User, prefix: Option[String] = None, suffix: Option[String] = None): String = {
      val fbase = fileTools.basename(domain, evolution)
      val fname = s"${prefix.getOrElse("")}${defn.id.name.name.capitalize}${suffix.getOrElse("")}.py"
      defn.defn.id.owner match {
        case Owner.Toplevel => s"$fbase/$fname"
        case Owner.Ns(path) => s"$fbase/${path.map(_.name.toLowerCase).mkString("/")}/$fname"
        case Owner.Adt(id)  => s"$fbase/${id.name.name.toLowerCase}.$fname"
      }
    }
  }
}
