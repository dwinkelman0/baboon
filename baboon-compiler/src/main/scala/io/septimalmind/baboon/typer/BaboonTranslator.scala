package io.septimalmind.baboon.typer

import io.septimalmind.baboon.parser.model.*
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, TyperIssue, VerificationIssue}
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.Scope.NestedScope
import io.septimalmind.baboon.typer.model.TypeRef.AnyVariant
import io.septimalmind.baboon.typer.model.Typedef.*
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.IzCollections.*
import izumi.fundamentals.collections.nonempty.NEList

object BaboonTranslator {
  type Factory[F[+_, +_]] = (
    Pkg,
    NestedScope[ExtendedRawDefn],
    Map[TypeId, DomainMember],
  ) => BaboonTranslator[F]
}

class BaboonTranslator[F[+_, +_]: Error2](
  pkg: Pkg,
  defn: NestedScope[ExtendedRawDefn],
  defined: Map[TypeId, DomainMember],
  scopeSupport: ScopeSupport[F],
) {

  def translate(): F[NEList[BaboonIssue], List[DomainMember]] = {
    val rawDefn = defn.defn
    for {
      id <- scopeSupport.resolveUserTypeId(
        rawDefn.defn.name,
        defn,
        pkg,
        rawDefn.defn.meta,
      )
      members <- convertMember(id, rawDefn, defn)
    } yield {
      members
    }
  }

  def resolveAliasInfo(): F[NEList[BaboonIssue], Option[AliasInfo]] = {
    defn.defn.defn match {
      case alias: RawAlias =>
        for {
          resolved <- convertTpe(alias.target, alias.meta)
          owner    <- scopeSupport.ownerOf(defn, pkg)
        } yield {
          Some(AliasInfo(TypeName(alias.name.name), owner, alias.target.render, resolved, alias.meta, defn.defn.gcRoot))
        }
      case _ => F.pure(None)
    }
  }

  /** Order-independent shell for fn assembly.
    *
    * The driver translates DTOs in hard-dep toposort order, which is arbitrary
    * for types related only through field refs or fn bodies (e.g. a table DTO
    * translated before its point DTO). `fnSchemaFor` reads the translator's
    * `defined` map, so without shells any fn mentioning a not-yet-translated
    * sibling type fails with a spurious "unknown type"/"no fields on" error.
    *
    * A shell carries real (name, converted type) field pairs plus
    * signature-only `computed`/`normalize`/`migrate`/`default-Fn` placeholders
    * (real params + expected type, dummy body). Shells are used ONLY for fn
    * pipeline resolution (`dtoFields`, user-method signatures); they are
    * always overwritten by the full translation before `out` is built, and
    * shell bodies are never lowered. The driver pre-computes shells for the
    * whole domain and merges them under the accumulated translations
    * (`shells ++ acc`, so full translations win).
    *
    * Covers DTO/Identifier/Contract (field paths + user methods) and Enum
    * (presence for type positions). ADTs/services/namespaces/aliases are
    * skipped: ADT members are hard-dep ordered already, services have no
    * fn-callable fields.
    */
  def shellMember(): F[NEList[BaboonIssue], List[DomainMember.User]] = {
    val rawDefn = defn.defn
    for {
      id <- scopeSupport.resolveUserTypeId(rawDefn.defn.name, defn, pkg, rawDefn.defn.meta)
      out <- rawDefn.defn match {
        case d: RawDto =>
          shellDto(id, rawDefn.gcRoot || d.special.isDefined, d, d.derived, d.meta) { case (i, fs, refs) => Typedef.Dto(i, fs, refs, special = d.special.map(Typedef.SpecialKind.fromRaw)) }
        case d: RawIdentifier =>
          shellDto(id, rawDefn.gcRoot || d.special.isDefined, d, d.derived, d.meta) { case (i, fs, refs) => Typedef.Dto(i, fs, refs, isIdentifier = true, special = d.special.map(Typedef.SpecialKind.fromRaw)) }
        case d: RawContract =>
          shellDto(id, rawDefn.gcRoot, d, Set.empty[RawMemberMeta], d.meta) { case (i, fs, refs) => Typedef.Contract(i, fs, refs) }
        case e: RawEnum =>
          converEnum(id, rawDefn.gcRoot || e.special.isDefined, e).map(e => List(e))
        case _ =>
          F.pure(List.empty)
      }
    } yield out
  }

  private def shellDto(
    id: TypeId.User,
    isRoot: Boolean,
    dto: RawDtoid,
    derived: Set[RawMemberMeta],
    meta: RawNodeMeta,
  )(
    produce: (TypeId.User, List[Field], List[TypeId.User]) => Typedef.User
  ): F[NEList[BaboonIssue], List[DomainMember.User]] = {
    F.traverseAccumErrors(dto.members.collect {
      case f: RawDtoMember.FieldDef            => (f.field, f.meta, Option.empty[RawDtoMember.FnProperty])
      case p: RawDtoMember.FnProperty          => (p.field, p.meta, Some(p))
      case f: RawDtoMember.TemplateArmFieldDef => (f.field, f.meta, Option.empty[RawDtoMember.FnProperty])
    }.toList) { case (rawField, m, prop) =>
      for {
        tpe <- convertTpe(rawField.tpe, m)
        fdefault <- rawField.default match {
          case Some(RawDefaultValue.Fn(fn)) => shellPlaceholderFn(fn, tpe, m, id, FnRole.Default).map(d => Some(FieldDefault.Fn(d): FieldDefault))
          case _                            => F.pure(None)
        }
        normalize <- rawField.normalize match {
          case Some(fn) => shellPlaceholderFn(fn, tpe, m, id, FnRole.Normalize).map(Some(_))
          case None     => F.pure(None)
        }
        migrate <- rawField.migrate match {
          case Some(mig) => shellPlaceholderFn(mig.fn, tpe, m, id, FnRole.Migrate).map(fn => Some(TypedMigrate(fn)))
          case None      => F.pure(None)
        }
        computed <- prop match {
          case Some(p) =>
            p.field.default match {
              case Some(RawDefaultValue.Fn(fn)) => shellPlaceholderFn(fn, tpe, p.meta, id, FnRole.Computed).map(Some(_))
              case _                            => F.pure(None)
            }
          case None => F.pure(None)
        }
      } yield Field(
        FieldName(rawField.name.name),
        tpe,
        rawField.prevName.map(pn => FieldName(pn.name)),
        Docs.empty,
        fdefault,
        normalize,
        migrate,
        computed = computed,
      )
    }.map(fs => List(DomainMember.User(isRoot, produce(id, fs.toList, Nil), derived, meta)))
  }

  /** Signature-only placeholder `TypedFn`: real params + expected type so user-method
    * resolution (`checkUserCall`) works for not-yet-translated types; the body is a
    * dummy zero literal that is never lowered (shells are always overwritten by the
    * full translation before `out` is built).
    */
  private def shellPlaceholderFn(
    fn: RawFn,
    expected: TypeRef,
    meta: RawNodeMeta,
    owner: TypeId.User,
    role: FnRole,
  ): F[NEList[BaboonIssue], model.TypedFn] = {
    // Mirror of `FnPipeline.selfFor`: a bare `Self` param must NOT go through
    // `convertTpe` (ScopeSupport has no `Self` binding and fails with
    // UnexpectedNonBuiltin). `Computed`/`Migrate` self is the enclosing
    // container (owner); `Default`/`Normalize` self is the field value.
    F.traverseAccumErrors(fn.params.toList) { p =>
      p.tpe match {
        case RawTypeRef.Simple(n, _) if n.name == "Self" =>
          val selfTpe = role match {
            case FnRole.Computed | FnRole.Migrate   => TypeRef.Scalar(owner)
            case FnRole.Default | FnRole.Normalize => expected
          }
          F.pure((p.name, selfTpe))
        case other =>
          convertTpe(other, meta).map(t => (p.name, t))
      }
    }.map { ps =>
      val zero = expected match {
        case TypeRef.Scalar(b) if b == TypeId.Builtins.bit => model.TypedLit.BoolLit(false)
        case t if model.FnTypes.isFloat(t)                 => model.TypedLit.FloatLit(0.0)
        case t if model.FnTypes.isInt(t)                   => model.TypedLit.IntLit(0)
        case TypeRef.Scalar(b) if b == TypeId.Builtins.str => model.TypedLit.StrLit("")
        case _                                             => model.TypedLit.IntLit(0)
      }
      model.TypedFn(None, model.TypedBody(Nil, model.TypedExpr.Lit(zero, expected), expected), expected, ps)
    }
  }

  private def convertMember(
    id: TypeId.User,
    defn: ExtendedRawDefn,
    thisScope: NestedScope[ExtendedRawDefn],
  ): F[NEList[BaboonIssue], List[DomainMember.User]] = {
    val root = defn.gcRoot
    defn.defn match {
      case d: RawDto => convertDto(id, root || d.special.isDefined, d) { case (id, finalFields, contractRefs) => Typedef.Dto(id, finalFields, contractRefs, special = d.special.map(Typedef.SpecialKind.fromRaw)) }.map(d => List(d))
      case d: RawIdentifier =>
        convertDto(id, root || d.special.isDefined, d) { case (id, finalFields, contractRefs) => Typedef.Dto(id, finalFields, contractRefs, isIdentifier = true, special = d.special.map(Typedef.SpecialKind.fromRaw)) }.map(d => List(d))
      case e: RawEnum     => converEnum(id, root || e.special.isDefined, e).map(e => List(e))
      case a: RawAdt      => convertAdt(id, root || a.special.isDefined, a, thisScope).map(_.toList)
      case f: RawForeign  => convertForeign(id, root, f).map(_.toList)
      case c: RawContract => convertDto(id, root || c.special.isDefined, c) { case (id, finalFields, contractRefs) => Typedef.Contract(id, finalFields, contractRefs) }.map(d => List(d))

      // namespace itself is not a typedef :3
      case _: RawNamespace => F.pure(List.empty)
      // alias is resolved transparently at usage sites
      case _: RawAlias => F.pure(List.empty)
      case s: RawService =>
        convertService(id, root, s, thisScope).map(_.toList)
    }
  }

  private def convertForeign(id: TypeId.User, isRoot: Boolean, f: RawForeign): F[NEList[BaboonIssue], NEList[DomainMember.User]] = {
    val (rtEntries, langEntries) = f.defns.partition(_.lang == "rt")

    for {
      rtMapping <- rtEntries match {
        case Nil => F.pure(None: Option[TypeRef])
        case single :: Nil =>
          single.decl match {
            case RawForeignDecl.BaboonRef(rawTypeRef) =>
              convertTpe(rawTypeRef, f.meta).map(Some(_))
            case RawForeignDecl.Custom(_, _) =>
              F.fail(BaboonIssue.of(TyperIssue.InvalidRtMapping(id, "rt binding must be a baboon type reference, not a string", f.meta)))
          }
        case _ =>
          F.fail(BaboonIssue.of(TyperIssue.InvalidRtMapping(id, "multiple rt bindings are not allowed", f.meta)))
      }
      mappedEntries <- F.traverse(langEntries) {
        e =>
          for {
            lang <- F.fromEither {
              BaboonLang.fromString(e.lang).toRight(BaboonIssue.of(TyperIssue.UnknownForeignLang(e.lang, id, f.meta)))
            }
            mapping <- e.decl match {
              case RawForeignDecl.Custom(decl, attrs) =>
                F.pure(ForeignMapping.Custom(decl, ForeignEntryAttrs(attrs.attrs.map(a => ForeignEntryAttr(a.name, a.value)))))
              case RawForeignDecl.BaboonRef(rawTypeRef) =>
                convertTpe(rawTypeRef, f.meta).map(ForeignMapping.BaboonRef(_))
            }
          } yield (lang, ForeignEntry(lang, mapping))
      }
      entries <- F.fromEither {
        mappedEntries
          .toUniqueMap(e => BaboonIssue.of(TyperIssue.NonUniqueForeignEntries(e, id, f.meta)))
      }
    } yield {
      NEList(DomainMember.User(isRoot, Typedef.Foreign(id, entries, rtMapping), f.derived, f.meta))
    }
  }

  private def converEnum(
    id: TypeId.User,
    isRoot: Boolean,
    choice: RawEnum,
  ): F[NEList[BaboonIssue], DomainMember.User] = {
    for {
      converted <- F.traverseAccumErrors(choice.members) {
        raw =>
          val name = raw.value
          for {
            _ <- SymbolNames.validEnumMemberName(name, raw.meta)
          } yield {
            EnumMember(
              name,
              raw.associated.map {
                case RawEnumConst.RawInt(int) => int
              },
              raw.prevName,
            )
          }
      }
      _ <- F.fromEither {
        converted
          .map(m => (m.name.toLowerCase, m))
          .toUniqueMap(e => BaboonIssue.of(TyperIssue.NonUniqueEnumBranches(e, id, choice.meta)))
      }
      nel <- F.fromOption(BaboonIssue.of(TyperIssue.EmptyEnum(id, choice.meta))) {
        NEList.from(converted)
      }
    } yield {
      DomainMember.User(isRoot, Typedef.Enum(id, nel, special = choice.special.map(Typedef.SpecialKind.fromRaw)), choice.derived, choice.meta)
    }
  }

  /** Minimal Domain→FnSchema adapter for the fn pipeline (translator-local).
    *
    * Type lookup comes from the translator's existing `defined` map
    * (no new global registry): every already-typed user typedef keyed by
    * simple name, plus a stub for the DTO currently being converted (the
    * current owner is not in `defined` yet; without the stub P4 slot
    * resolution would fail with "unknown owner type"). Contract member
    * fields ride along inside their owning DTO/Contract typedefs, so no
    * separate contract-membership table is needed. ADT variant tables are
    * inferred conservatively (single-field branch DTO => payload, else
    * valueless) — `opt` matching is handled by the pipeline itself.
    *
    * The owner stub carries the FULL sibling field list
    * (via `fnSchemaForAll`), not just the single field under conversion, so
    * computed/default/normalize/migrate fns can read sibling fields through
    * `self`. Previously only the current field was visible and any
    * multi-field fn body failed resolution. Sibling entries are type-level
    * only (no defaults); the merged stub dedupes by name preferring the
    * current field.
    */
  private def fnSchemaFor(stub: Typedef): FnSchema = {
    val fromDefined: Map[String, Typedef] = defined.values.collect {
      case DomainMember.User(_, d: Typedef.Dto, _, _)      => d.id.name.name -> (d: Typedef)
      case DomainMember.User(_, e: Typedef.Enum, _, _)     => e.id.name.name -> (e: Typedef)
      case DomainMember.User(_, a: Typedef.Adt, _, _)      => a.id.name.name -> (a: Typedef)
      case DomainMember.User(_, c: Typedef.Contract, _, _) => c.id.name.name -> (c: Typedef)
    }.toMap
    val adts: Map[String, List[FnAdtVariant]] = defined.values.collect {
      case DomainMember.User(_, a: Typedef.Adt, _, _) => a
    }.map { adt =>
      adt.id.name.name -> adt.members.toList.map { m =>
        val payload: Option[TypeRef] = defined.get(m).collect {
          case DomainMember.User(_, d: Typedef.Dto, _, _) if d.fields.size == 1 => d.fields.head.tpe
        }
        FnAdtVariant(m.name.name, payload)
      }
    }.toMap
    FnSchema(fromDefined + (stub.id.name.name -> stub), adts)
  }

  /** Full-field owner stub. `stub` is the field under
    * conversion; `siblings` are pre-converted type-level fields of the same
    * DTO (see `siblingStubs` in `convertDto`). */
  private def fnSchemaForAll(owner: TypeId.User, stub: Field, siblings: Seq[Field]): FnSchema = {
    val merged = stub +: siblings.filterNot(_.name == stub.name).toList
    fnSchemaFor(Typedef.Dto(owner, merged, Nil))
  }

  /** Run one fn site through `FnPipeline.assemble` and project the IR node
    * to the model-level `TypedFn`. Plain-string pipeline errors are mapped
    * to `VerificationIssue.FnNotSupported` (owner + meta preserved so
    * diagnostics keep their positions); no new issue types.
    *
    * `Self` is resolved role-dependently by the pipeline (P2): computed /
    * migrate `Self` is the owner container, default / normalize `Self` is
    * the field's own value. No translator-local rewrite.
    */
  private def assembleFn(
    owner: TypeId.User,
    fieldName: String,
    role: FnRole,
    rawFn: RawFn,
    expectedRaw: RawTypeRef,
    rawSources: List[String],
    stubField: Field,
    meta: RawNodeMeta,
    label: String,
    siblings: Seq[Field] = Nil,
    ownerIsRoot: Boolean = false,
    nodeCapture: Option[RawCapture] = None,
  ): F[NEList[BaboonIssue], model.TypedFn] = {
    val fn = rawFn
    val schema = fnSchemaForAll(owner, stubField, siblings)
    val sourcesR: Either[List[String], Set[SchemaPath]] =
      rawSources.foldRight(Right(Nil): Either[List[String], List[SchemaPath]]) { (s, acc) =>
        for {
          tail <- acc
          sp <- SchemaPath
            .resolve(owner, s.split("\\.").toList.filter(_.nonEmpty), schema)
            .left.map(e => List(e))
        } yield sp :: tail
      }.map(_.toSet)
    sourcesR match {
      case Left(es) =>
        val issues = es.flatMap(e => BaboonIssue.of(VerificationIssue.FnNotSupported(owner, s"$label fn: $e", meta)).toList)
        F.fail(NEList.unsafeFrom(issues))
      case Right(sources) =>
        val input = FnPipeline.FnSiteInputs(fn, owner.name.name, fieldName, role, expectedRaw, sources, schema, ownerIsRoot, nodeCapture)
        FnPipeline.assemble(List(input)) match {
          case Right(prog) =>
            prog.fns.headOption match {
              case Some((_, typed, _)) => F.pure(model.TypedFn(typed.capture, typed.body, typed.expected, typed.params))
              case None =>
                F.fail(BaboonIssue.of(VerificationIssue.FnNotSupported(owner, s"$label fn: pipeline returned no fns", meta)))
            }
          case Left(errs) =>
            val issues = errs.flatMap(e => BaboonIssue.of(VerificationIssue.FnNotSupported(owner, s"$label fn: $e", meta)).toList)
            F.fail(NEList.unsafeFrom(issues))
        }
    }
  }

  private def dtoFieldToDefs(
    owner: TypeId.User,
    field: RawField,
    meta: RawNodeMeta,
    siblings: Seq[Field] = Nil,
    ownerIsRoot: Boolean = false,
  ): F[NEList[BaboonIssue], Seq[Field]] = {
    for {
      name    <- F.pure(FieldName(field.name.name))
      _       <- SymbolNames.validFieldName(name, meta)
      tpe     <- convertTpe(field.tpe, meta)
      prevName = field.prevName.map(pn => FieldName(pn.name))
      // Field-level docs are lifted from the field's RawNodeMeta. Both
      // prefix `/** … */` and suffix `//!` flow through; layered drop is a
      // no-op here because RawField positions do legally accept both
      // (spec §3.1, §3.3). Empty/whitespace-only bodies are silently
      // dropped inside `DocFormat.liftDocs` per spec §5.4.
      docs = DocFormat.liftDocs(meta.docs)
      stubBase = Field(name, tpe, prevName, docs, None)
      fdefault <- field.default match {
        case Some(d) => convertDefault(owner, name, tpe, d, stubBase, meta, field.tpe, siblings, ownerIsRoot).map(Some(_))
        case None    => F.pure(None)
      }
      normalize <- field.normalize match {
        case Some(fn) => assembleFn(owner, name.name, FnRole.Normalize, fn, field.tpe, Nil, stubBase, meta, "normalize", siblings, ownerIsRoot).map(Some(_))
        case None     => F.pure(None)
      }
      migrate <- field.migrate match {
        case Some(m) => assembleFn(owner, name.name, FnRole.Migrate, m.fn, field.tpe, m.sources, stubBase, meta, "migrate", siblings, ownerIsRoot).map(fn => Some(TypedMigrate(fn)))
        case None    => F.pure(None)
      }
    } yield {
      Seq(Field(name, tpe, prevName, docs, fdefault, normalize, migrate))
    }
  }

  private def convertDefault(
    owner: TypeId.User,
    fieldName: FieldName,
    tpe: TypeRef,
    raw: RawDefaultValue,
    stub: Field,
    meta: RawNodeMeta,
    expectedRaw: RawTypeRef,
    siblings: Seq[Field] = Nil,
    ownerIsRoot: Boolean = false,
  ): F[NEList[BaboonIssue], FieldDefault] = {
    val targetType = tpe match {
      case TypeRef.Scalar(b: TypeId.BuiltinScalar) => b
      case _                                       => TypeId.Builtins.i32
    }
    raw match {
      case RawDefaultValue.IntLit(value, r)    => F.pure(FieldDefault.IntVal(value, r, targetType))
      case RawDefaultValue.FloatLit(value, r)  => F.pure(FieldDefault.FloatVal(value, r))
      case RawDefaultValue.BoolLit(value)      => F.pure(FieldDefault.BoolVal(value))
      case RawDefaultValue.StrLit(value)       => F.pure(FieldDefault.StrVal(value))
      case RawDefaultValue.NullLit             => F.pure(FieldDefault.NullVal)
      case RawDefaultValue.EnumRef(value)      => F.pure(FieldDefault.EnumVal(value))
      case v: RawDefaultValue.SpecialFloat     =>
        val kind = v match {
          case RawDefaultValue.SpecialFloat.NaN    => FieldDefault.SpecialFloatKind.NaN
          case RawDefaultValue.SpecialFloat.PosInf => FieldDefault.SpecialFloatKind.PosInf
          case RawDefaultValue.SpecialFloat.NegInf => FieldDefault.SpecialFloatKind.NegInf
        }
        F.pure(FieldDefault.SpecialFloatVal(kind))
      case RawDefaultValue.Fn(fn) =>
        assembleFn(owner, fieldName.name, FnRole.Default, fn, expectedRaw, Nil, stub, meta, "default", siblings, ownerIsRoot).map(FieldDefault.Fn(_))
    }
  }

  private def dtoParentToDefs(
    parent: ScopedRef,
    meta: RawNodeMeta,
    refMeta: RawNodeMeta,
  ): F[NEList[BaboonIssue], Seq[Field]] = {
    for {
      id       <- scopeSupport.resolveScopedRef(parent, defn, pkg, refMeta)
      parentDef = defined(id)
      out <- parentDef match {
        case DomainMember.User(_, defn: Typedef.Dto, _, _) =>
          F.pure(defn.fields)
        case o =>
          F.fail(BaboonIssue.of(TyperIssue.WrongParent(id, o.id, meta)))
      }
    } yield {
      out
    }
  }

  def readContractContent(id: TypeId.User, meta: RawNodeMeta): F[NEList[BaboonIssue], List[ContractContent]] = {
    for {
      parentDef <- F.pure(defined(id))
      fields <- parentDef match {
        case DomainMember.User(_, defn: Typedef.Contract, _, _) =>
          for {
            parents <- F.flatTraverseAccumErrors(defn.contracts) {
              c =>
                readContractContent(c, meta)
            }

          } yield {
            parents ++ Set(ContractContent(defn.fields, Set(id)))
          }
        case o =>
          F.fail(BaboonIssue.of(TyperIssue.WrongParent(id, o.id, meta)))
      }
    } yield {
      fields
    }
  }

  def contractContent(c: RawDtoMember.ContractRef, meta: RawNodeMeta, refMeta: RawNodeMeta): F[NEList[BaboonIssue], List[ContractContent]] = {
    for {
      id      <- scopeSupport.resolveScopedRef(c.contract.tpe, defn, pkg, refMeta)
      content <- readContractContent(id, meta)
    } yield {
      content
    }
  }

  case class ContractContent(fields: Seq[Field], refs: Set[TypeId.User])

  private def convertDto(
    id: TypeId.User,
    isRoot: Boolean,
    dto: RawDtoid,
  )(produce: (TypeId.User, List[Field], List[TypeId.User]) => Typedef.User
  ): F[NEList[BaboonIssue], DomainMember.User] = {
    for {
      // Type-level sibling stubs so fn bodies can resolve
      // `self.<sibling>` during assembly (the owner is not in `defined` yet).
      // Best-effort ordering note: a bad sibling *type* fails here before the
      // main pass runs, so the error surfaces once from this pre-pass.
      // Parent/contract/intersection members are out of scope: no fn bodies
      // in this study read through them.
      siblingStubs <- F.flatTraverseAccumErrors(dto.members.collect {
        case f: RawDtoMember.FieldDef             => (f.field, f.meta)
        case p: RawDtoMember.FnProperty           => (p.field, p.meta)
        case f: RawDtoMember.TemplateArmFieldDef  => (f.field, f.meta)
      }) { case (rawField, m) =>
        for {
          sname <- F.pure(FieldName(rawField.name.name))
          _     <- SymbolNames.validFieldName(sname, m)
          stpe  <- convertTpe(rawField.tpe, m)
        } yield List(Field(sname, stpe, rawField.prevName.map(pn => FieldName(pn.name)), Docs.empty, None))
      }
      converted <- F.flatTraverseAccumErrors(dto.members) {
        case p: RawDtoMember.ContractRef =>
          dto match {
            case _: RawDto | _: RawIdentifier =>
              contractContent(p, dto.meta, p.meta)
                .map(_.flatMap(_.fields))
            case _: RawContract =>
              F.pure(List.empty)
          }
        case f: RawDtoMember.FieldDef =>
          dtoFieldToDefs(id, f.field, f.meta, siblingStubs, isRoot)
        case f: RawDtoMember.TemplateArmFieldDef =>
          // PR-33.9 [PR-33.3-D01]: typer-internal carrier produced by
          // `TemplateInstantiator.convertLoweredArm` (Plus arm) for fields originating from a
          // `+ Template[Args]` inline expansion. Field-level conversion is identical to
          // `FieldDef`; the variant tag only matters for the duplicate-name detection below.
          dtoFieldToDefs(id, f.field, f.meta, siblingStubs, isRoot)
        case p: RawDtoMember.FnProperty =>
          p.field.default match {
            case Some(RawDefaultValue.Fn(fn)) =>
              for {
                fname <- F.pure(FieldName(p.field.name.name))
                _     <- SymbolNames.validFieldName(fname, p.meta)
                ftpe  <- convertTpe(p.field.tpe, p.meta)
                prevName = p.field.prevName.map(pn => FieldName(pn.name))
                docs     = DocFormat.liftDocs(p.meta.docs)
                stub     = Field(fname, ftpe, prevName, docs, None)
                typed <- assembleFn(id, fname.name, FnRole.Computed, fn, p.field.tpe, Nil, stub, p.meta, "computed", siblingStubs, isRoot)
              } yield Seq(Field(fname, ftpe, prevName, docs, None, computed = Some(typed)))
            case _ =>
              dtoFieldToDefs(id, p.field, p.meta, siblingStubs, isRoot)
          }
        case p: RawDtoMember.ParentDef =>
          // PR-33.2 (M33): when args.isDefined, the TemplateInstantiator pre-pass has already
          // spliced the substituted FieldDefs into the receiving DTO's member list (Approach M1).
          // Any ParentDef surviving to the translator therefore carries args = None — a legacy
          // `+ ParentRef` (no template instantiation) — and is handled exactly as before.
          dtoParentToDefs(p.parent, dto.meta, p.meta)
        case _ =>
          F.pure(Seq.empty)
      }
      removed <- F.flatTraverseAccumErrors(dto.members) {
        case f: RawDtoMember.UnfieldDef =>
          dtoFieldToDefs(id, f.field, f.meta, siblingStubs, isRoot)
        case p: RawDtoMember.UnparentDef =>
          // PR-33.2 (M33): when args.isDefined, the TemplateInstantiator pre-pass has already
          // converted each substituted FieldDef into an UnfieldDef and spliced them into the
          // receiving DTO's member list (Approach M1). Any UnparentDef surviving to the
          // translator therefore carries args = None — a legacy `- ParentRef` (no template
          // instantiation) — and is handled exactly as before.
          dtoParentToDefs(p.parent, dto.meta, p.meta)
        case _ =>
          F.pure(Seq.empty)
      }
      intersectionLimiters <- F.flatTraverseAccumErrors(dto.members) {
        case p: RawDtoMember.IntersectionDef =>
          // PR-33.2 (M33): when args.isDefined the TemplateInstantiator pre-pass has already
          // lowered this arm into RawDtoMember.IntersectionFields (see below). Any IntersectionDef
          // surviving to the translator therefore carries args = None — a legacy `^ ParentRef`
          // (no template instantiation) — and is handled exactly as before.
          dtoParentToDefs(p.parent, dto.meta, p.meta)
        case f: RawDtoMember.IntersectionFields =>
          // PR-33.2 (M33): typer-synthesized intersection-limiter (M3 carrier for
          // `^ Template[Args]`). Each FieldDef is converted via dtoFieldToDefs to a Field, so
          // Field equality matches the receiver's other-arm-produced fields exactly.
          // PR-33.2-D06: per-field meta is preserved on each FieldDef so diagnostics anchor at
          // the original template-body field declaration site (not at the `^` operator position).
          F.flatTraverseAccumErrors(f.fields)(fd => dtoFieldToDefs(id, fd.field, fd.meta, siblingStubs, isRoot))
        case _ =>
          F.pure(Seq.empty)
      }

      adtContracts <- dto match {
        case _: RawDto | _: RawIdentifier =>
          id.owner match {
            case Owner.Adt(xid) =>
              defined(xid) match {
                case DomainMember.User(_, defn: Typedef.Adt, _, _) =>
                  F.flatTraverseAccumErrors(defn.contracts) {
                    c => readContractContent(c, dto.meta)
                  }
                case o =>
                  F.fail(BaboonIssue.of(TyperIssue.WrongParent(id, o.id, dto.meta)))
              }
            case _ =>
              F.pure(List.empty)
          }
        case _: RawContract =>
          F.pure(List.empty)
      }

      localContracts <- F.flatTraverseAccumErrors(dto.members) {
        case p: RawDtoMember.ContractRef =>
          contractContent(p, dto.meta, p.meta)
        case _ =>
          F.pure(Seq.empty)
      }

      contracts      = (adtContracts ++ localContracts).distinct
      contractFields = contracts.flatMap(_.fields)

      // PR-33.9 [PR-33.3-D01]: provenance-aware narrowing of the duplicate-field-name detection.
      // The `.distinct` below silently absorbs structurally-identical Field instances; that is
      // correct for the contract-diamond case (e.g. pkg03 `T4_A1#B1` two `f2: #i32` from `+`
      // ContractRef paths) but wrong for `+ MyGen[i32]; + MyGen[i32]` (idempotent template-arm
      // duplicate). We narrow by counting only fields whose origin is a template-arm inline
      // expansion (`RawDtoMember.TemplateArmFieldDef`, produced by
      // `TemplateInstantiator.convertLoweredArm` Plus arm). When ≥2 same-name converted fields
      // are template-arm-origin, fire `NonUniqueFields` BEFORE `.distinct` collapses them.
      // Edge cases (documented in PR-33.9 task brief):
      //   - `+ MyGen[i32]; + MyGen[i32]` → 2 template-arm origins → fires.
      //   - pkg03 contract diamond `+ S1; + S2` both with `f2: i32` → 0 template-arm origins → silent.
      //   - `+ MyGen[i32]; + MyGen[str]` → 2 template-arm origins, types differ → fires here
      //     (existing toUniqueMap below would also fire; new check is earlier with same case-name).
      //   - `+ ConcreteBase; + MyGen[i32]` both producing `v: i32` → 1 template-arm origin → silent
      //     (matches the orchestrator-stated rule "≥2 origins are template-arm").
      // [PR-33.9-D03] Ordering note: this check runs BEFORE `removed` is subtracted from
      // `converted`. Consequently, `+ Tmpl[i32]; + Tmpl[i32]; - Tmpl[i32]` fires NonUniqueFields
      // even though the `-` arm would have cancelled both `+` arms. This is intentional fail-fast
      // on duplicate intent: two identical `+` arms indicate a user mistake regardless of what `-`
      // follows. The concrete-type analogue (`+ A; + A; - A`) is unaffected — concrete-arm origins
      // are not `TemplateArmFieldDef`, so templateArmCounts["v"] = 0 → silent (`.distinct` absorbs).
      templateArmOriginNamesLc: Seq[String] = dto.members.collect {
        case f: RawDtoMember.TemplateArmFieldDef => f.field.name.name.toLowerCase
      }
      templateArmCounts = templateArmOriginNamesLc.groupBy(identity).view.mapValues(_.size).toMap
      _ <- {
        val convertedByLcName: Map[String, List[Field]] =
          converted.toList.groupBy(_.name.name.toLowerCase)
        val offenders: Map[String, List[Field]] =
          convertedByLcName.collect {
            case (lc, fields) if fields.size >= 2 && templateArmCounts.getOrElse(lc, 0) >= 2 => lc -> fields
          }
        if (offenders.nonEmpty) {
          F.fail(BaboonIssue.of(TyperIssue.NonUniqueFields(id, offenders, dto.meta)))
        } else {
          F.unit
        }
      }

      removedSet      = removed.toSet
      intersectionSet = intersectionLimiters.toSet

      withoutRemoved = (adtContracts.flatMap(_.fields) ++ converted)
        .filterNot(f => removedSet.contains(f))
        .distinct

      finalFields =
        if (intersectionSet.isEmpty) {
          withoutRemoved
        } else {
          withoutRemoved.filter(f => intersectionSet.contains(f))
        }

      mergedFields <- mergeFieldDefaults(id, finalFields, dto.meta)

      missingIrremovable = contractFields.distinct.diff(withoutRemoved.distinct)
      _ <- dto match {
        case _: RawDto | _: RawIdentifier =>
          F.when(missingIrremovable.nonEmpty)(
            F.fail(BaboonIssue.of(TyperIssue.MissingContractFields(id, missingIrremovable, dto.meta)))
          )
        case _: RawContract =>
          F.pure(())
      }

      _ <- F.fromEither {
        mergedFields
          .map(m => (m.name.name.toLowerCase, m))
          .toUniqueMap(e => BaboonIssue.of(TyperIssue.NonUniqueFields(id, e, dto.meta)))
      }
      contractRefs = contracts.flatMap(_.refs).distinct
    } yield {
      val decls = dto match {
        case d: RawDto        => d.derived
        case d: RawIdentifier => d.derived
        case _: RawContract   => Set.empty[RawMemberMeta]
      }

      DomainMember.User(
        isRoot,
        produce(id, mergedFields, contractRefs),
        decls,
        dto.meta,
      )
    }
  }

  private def mergeFieldDefaults(
    id: TypeId.User,
    fields: List[Field],
    meta: RawNodeMeta,
  ): F[NEList[BaboonIssue], List[Field]] = {
    val order = fields.map(_.name.name.toLowerCase).distinct
    val grouped = fields.groupBy(_.name.name.toLowerCase)

    val merged = order.flatMap {
      lcName =>
        grouped.get(lcName).toList.flatMap {
          group =>
            val distinctTypes = group.map(_.tpe).distinct
            if (distinctTypes.size > 1) {
              // Same field name with different types cannot be merged. Preserve all
              // entries so the downstream duplicate-name check can fire
              // `NonUniqueFields` with the correct provenance.
              group
            } else {
              val representative = group.head
              val distinctDefaults = group.map(_.default).distinct
              distinctDefaults match {
                case List(None)        => List(representative.copy(default = None))
                case List(Some(d))     => List(representative.copy(default = Some(d)))
                case _                 =>
                  val nonEmpty = distinctDefaults.collect { case Some(d) => d }.distinct
                  if (nonEmpty.size == 1) {
                    List(representative.copy(default = Some(nonEmpty.head)))
                  } else {
                    List.empty
                  }
              }
            }
        }
    }

    val conflicts = order.flatMap {
      lcName =>
        grouped.get(lcName).toList.flatMap {
          group =>
            val distinctTypes = group.map(_.tpe).distinct
            if (distinctTypes.size > 1) {
              List.empty
            } else {
              val distinctDefaults = group.map(_.default).distinct
              val nonEmpty = distinctDefaults.collect { case Some(d) => d }.distinct
              if (nonEmpty.size > 1) {
                List((group.head.name, nonEmpty))
              } else {
                List.empty
              }
            }
        }
    }

    if (conflicts.nonEmpty) {
      val issues = conflicts.map {
        case (name, defs) =>
          BaboonIssue.of(VerificationIssue.ConflictingDefaults(id, name, defs, meta))
      }
      F.fail(NEList.unsafeFrom(issues.flatten))
    } else {
      F.pure(merged)
    }
  }

  private def convertAdt(
    id: TypeId.User,
    isRoot: Boolean,
    adt: RawAdt,
    thisScope: NestedScope[ExtendedRawDefn],
  ): F[NEList[BaboonIssue], NEList[DomainMember.User]] = {
    for {
      converted <- F.sequenceAccumErrors {
        // Fail fast: the M20 inheritance arms (Include, Exclude, Intersect) must be desugared
        // by the PR-63 typer pre-pass before convertAdt is invoked. If any survive to this point
        // the pre-pass is missing.
        adt.members.foreach {
          case _: RawAdtMember.Include   => throw new RuntimeException("BUG: RawAdtMember.Include reached convertAdt before PR-63 pre-pass")
          case _: RawAdtMember.Exclude   => throw new RuntimeException("BUG: RawAdtMember.Exclude reached convertAdt before PR-63 pre-pass")
          case _: RawAdtMember.Intersect => throw new RuntimeException("BUG: RawAdtMember.Intersect reached convertAdt before PR-63 pre-pass")
          case _: RawAdtMember.Keep      => throw new RuntimeException("BUG: RawAdtMember.Keep reached convertAdt before materialization pre-pass")
          case _: RawAdtMember.Drop      => throw new RuntimeException("BUG: RawAdtMember.Drop reached convertAdt before materialization pre-pass")
          case _                         =>
        }
        adt.members.collect { case d: RawAdtMemberDto => d; case d: RawAdtMemberContract => d }
          .map(
            member =>
              scopeSupport
                .resolveUserTypeId(member.defn.name, thisScope, pkg, member.meta)
          )
      }
      nel <- F.fromOption(BaboonIssue.of(TyperIssue.EmptyAdt(id, adt.meta))) {
        NEList.from(converted)
      }
      contracts <- F
        .traverseAccumErrors(adt.contracts) {
          ref =>
            scopeSupport.resolveScopedRef(ref.contract.tpe, defn, pkg, ref.meta)
        }
        .map(_.toList)
      fields <- F
        .flatTraverseAccumErrors(adt.contracts) {
          c =>
            contractContent(c, adt.meta, c.meta)
              .map(_.flatMap(_.fields))
        }
        .map(_.toList)
    } yield {
      NEList(
        DomainMember
          .User(isRoot, Typedef.Adt(id, nel, contracts, fields, special = adt.special.map(Typedef.SpecialKind.fromRaw)), adt.derived, adt.meta)
      )
    }
  }

  private def convertService(
    id: TypeId.User,
    isRoot: Boolean,
    svc: RawService,
    @annotation.unused thisScope: NestedScope[ExtendedRawDefn],
  ): F[NEList[BaboonIssue], NEList[DomainMember.User]] = {

    for {
      defs <- F.traverseAccumErrors(svc.defns)(f => convertMethod(svc, f))
      _ <- F.fromEither {
        defs
          .map(m => (m.name.name.toLowerCase, m))
          .toUniqueMap(dupes => BaboonIssue.of(TyperIssue.NonUniqueMethodNames(svc.name.name, dupes.view.mapValues(_.map(_.name.name).toList).toMap, svc.meta)))
      }
    } yield {
      NEList(
        DomainMember
          .User(isRoot, Typedef.Service(id, defs.toList), Set.empty, svc.meta)
      )
    }
  }

  private def convertArg(svc: RawService, fn: RawFunc, f: RawFuncArg): F[NEList[BaboonIssue], (String, TypeRef)] = {

    f match {
      case RawFuncArg.Ref(ref, marker, meta) =>
        for {
          tpe <- convertTpe(ref, meta)
        } yield {
          (marker, tpe)
        }
      case RawFuncArg.Struct(defn) =>
        for {
          tpe <- convertTpe(RawTypeRef.Simple(defn.name, List(svc.name, RawTypeName(fn.name))), defn.meta)
        } yield {
          (defn.name.name, tpe)
        }

    }

  }

  private def convertMethod(svc: RawService, f: RawFunc): F[NEList[BaboonIssue], MethodDef] = {

    for {
      args   <- F.traverseAccumErrors(f.sig)(arg => convertArg(svc, f, arg))
      inargs  = args.filter(_._1.toLowerCase == "in").map(_._2)
      outargs = args.filter(_._1.toLowerCase == "out").map(_._2)
      errargs = args.filter(_._1.toLowerCase == "err").map(_._2)

      _ <- F.when(outargs.size != 1)(F.fail(BaboonIssue.of(TyperIssue.ServiceMissingOutput(svc.name.name, f.name, f.meta))))
      _ <- F.when(outargs.size > 1)(F.fail(BaboonIssue.of(TyperIssue.ServiceMultipleOutputs(svc.name.name, f.name, outargs.size, f.meta))))
      _ <- F.when(inargs.size > 1)(F.fail(BaboonIssue.of(TyperIssue.ServiceMultipleInputs(svc.name.name, f.name, inargs.size, f.meta))))
      _ <- F.when(errargs.size > 1)(F.fail(BaboonIssue.of(TyperIssue.ServiceMultipleErrors(svc.name.name, f.name, errargs.size, f.meta))))
    } yield {
      // Method-level docs come from the function's RawNodeMeta. Spec §3.1
      // accepts only prefix `/** … */` here (postfix `//!` is field-only;
      // the parser wires it on `fieldDef`, not on `def`), so any `suffix`
      // captured by the uniform `withMeta` site would already be empty.
      val methodDocs = DocFormat.liftDocs(f.meta.docs)
      MethodDef(MethodName(f.name), inargs.head, outargs.headOption, errargs.headOption, methodDocs)
    }
  }

  private def convertTpe(
    tpe: RawTypeRef,
    meta: RawNodeMeta,
  ): F[NEList[BaboonIssue], TypeRef] = {
    convertTpeFrom(tpe, meta, defn, Set.empty)
  }

  /** @param from            the scope the names in `tpe` are looked up from: the definition being
    *                        translated, unless `tpe` is the target of an alias (then it is that alias,
    *                        which may be declared in another namespace) or carries a scope of its own
    * @param followedAliases the aliases followed on the way to `tpe`
    */
  private def convertTpeFrom(
    tpe: RawTypeRef,
    meta: RawNodeMeta,
    from: Scope[ExtendedRawDefn],
    followedAliases: Set[Scope.ScopeUID],
  ): F[NEList[BaboonIssue], TypeRef] = {
    tpe match {
      // Bare `any` (unprefixed, no brackets) always resolves to `TypeRef.Any(Global, None)` —
      // this match fires BEFORE `scopeSupport.resolveAlias`, so top-level user types or aliases
      // literally named `any` are shadowed when referenced unprefixed. Nested / prefixed
      // references (`foo.any`, `my.pkg.any`) fall through to normal scope lookup.
      //
      // Back-compat note: the parser preserves `any` as a legal user type name (PR 1.1). A
      // top-level `data any { ... }` still exists in `domain.defs.meta.nodes`, but it cannot be
      // referenced via bare `any`; callers must use a prefix (e.g. `nested.any`).
      case RawTypeRef.Simple(RawTypeName("any"), Nil) =>
        F.pure(TypeRef.Any(AnyVariant.Global, None))
      case RawTypeRef.Simple(name, prefix) =>
        scopeSupport.resolveAlias(prefix, name, from) match {
          case Some(alias) =>
            if (followedAliases.contains(alias.scope.id)) {
              F.fail(BaboonIssue.of(TyperIssue.CircularAlias(name, meta)))
            } else {
              convertTpeFrom(alias.target, meta, alias.scope, followedAliases + alias.scope.id)
            }
          case None =>
            for {
              id <- scopeSupport.resolveTypeId(prefix, name, from, pkg, meta)
              asScalar <- id match {
                case scalar: TypeId.Scalar =>
                  F.pure(scalar)
                case _ =>
                  F.fail(BaboonIssue.of(TyperIssue.ScalarExpected(id, meta)))
              }
            } yield {
              TypeRef.Scalar(asScalar)
            }
        }
      case RawTypeRef.Constructor(name, params, prefix) =>
        for {
          _ <- F.when(prefix.nonEmpty)(
            F.fail(BaboonIssue.of(TyperIssue.ScopedRefToNamespacedGeneric(prefix, meta)))
          )
          id <- scopeSupport.resolveTypeId(prefix, name, from, pkg, meta)
          asCollection <- id match {
            case coll: TypeId.BuiltinCollection =>
              F.pure(coll)
            case _ =>
              F.fail(BaboonIssue.of(TyperIssue.CollectionExpected(id, meta)))
          }
          args <- F.traverseAccumErrors(params.toList)(convertTpeFrom(_, meta, from, followedAliases))
          nel <- F.fromOption(BaboonIssue.of(TyperIssue.EmptyGenericArgs(id, meta))) {
            NEList.from(args)
          }
        } yield {
          TypeRef.Constructor(asCollection, nel)
        }
      case RawTypeRef.AnyRef(qualifier, underlying) =>
        val variant = qualifier match {
          case None                                  => AnyVariant.Global
          case Some(RawTypeRef.AnyRef.DomainThis)    => AnyVariant.ThisDom
          case Some(RawTypeRef.AnyRef.DomainCurrent) => AnyVariant.Current
        }
        underlying match {
          case None      => F.pure(TypeRef.Any(variant, None))
          case Some(raw) => convertTpeFrom(raw, meta, from, followedAliases).map(u => TypeRef.Any(variant, Some(u)))
        }
      case RawTypeRef.Scoped(ref, scope) =>
        convertTpeFrom(ref, meta, scopeSupport.lookupScope(scope, from), followedAliases)
    }
  }

}
