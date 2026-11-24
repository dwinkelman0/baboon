package io.septimalmind.baboon.parser.defns

import fastparse.*
import io.septimalmind.baboon.parser.{ParserContext, model}
import io.septimalmind.baboon.parser.defns.base.{DefDocs, Literals, idt, kw}
import io.septimalmind.baboon.parser.model.{BodyExpr, BodyLiteral, BodyStmt, ExtractionKind, ForInit, MatchArm, MatchPattern, RawBody, RawCapture, RawCaptureStep, RawContractRef, RawDefaultValue, RawDocComment, RawDocs, RawDtoMember, RawField, RawFieldName, RawFn, RawFnParam, RawIdentifier, RawLocatorSeg, RawMigrate, RawSpecial, RawStarCapture, RawTypeName, RawTypeRef, ScopedRef}
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.language.Quirks.Discarder

/** Fn/body + capture-path + field-annotation grammar extracted from DefDto.
  * Owned by DefDto (`dto.fn`); DefDto delegates so existing callers keep compiling.
  * Shared helpers (`typeRef`, `fieldName`) are called back on the owning DefDto.
  */
final case class FieldAnnotations(
  default: Option[RawDefaultValue],
  normalize: Option[RawFn],
  migrate: Option[RawMigrate],
)

class DefFn(context: ParserContext, meta: DefMeta, docs: DefDocs, dto: DefDto) {
  context.discard()

  def fieldAnnotations[$: P]: P[FieldAnnotations] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    (defaultAnnotation.? ~ normalizeAnnotation.? ~ migrateAnnotation.?).map {
      case (d, n, m) => FieldAnnotations(d, n, m)
    }
  }

  def fieldWas[$: P]: P[RawFieldName] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    kw.was ~ dto.fieldName
  }

  def defaultAnnotation[$: P]: P[RawDefaultValue] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    "@" ~ kw.default ~ "(" ~/ defaultArg ~ ")"
  }

  def normalizeAnnotation[$: P]: P[RawFn] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    "@" ~ kw.normalize ~ "(" ~/ fnDecl ~ ")"
  }

  def migrateAnnotation[$: P]: P[RawMigrate] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    P("@" ~ kw.migrate ~ "(" ~/ fnDecl).flatMap { fn =>
      P("," ~/ "[" ~ sourceRef.rep(sep = ",") ~ "]" ~ ")").map { sources =>
        RawMigrate(fn, sources.toList)
      }
    }
  }

  private def sourceRef[$: P]: P[String] = {
    import fastparse.NoWhitespace.noWhitespaceImplicit
    noWhitespaceImplicit.discard()
    idt.symbolSeq.map(_.mkString("."))
  }

  private def defaultArg[$: P]: P[RawDefaultValue] = {
    P(fnDecl.map(RawDefaultValue.Fn(_)) | specialFloat | strLit | boolLit | nullLit | floatLit | intLit | enumIdent)
  }

  private def specialFloat[$: P]: P[RawDefaultValue.SpecialFloat] = {
    (kw.nan.map(_ => RawDefaultValue.SpecialFloat.NaN: RawDefaultValue.SpecialFloat) |
      kw.posInf.map(_ => RawDefaultValue.SpecialFloat.PosInf: RawDefaultValue.SpecialFloat) |
      kw.negInf.map(_ => RawDefaultValue.SpecialFloat.NegInf: RawDefaultValue.SpecialFloat))
  }

  private def boolLit[$: P]: P[RawDefaultValue.BoolLit] = {
    (kw.boolTrue.map(_ => RawDefaultValue.BoolLit(true)) |
      kw.boolFalse.map(_ => RawDefaultValue.BoolLit(false)))
  }

  private def nullLit[$: P]: P[RawDefaultValue.NullLit.type] = {
    kw.nullLit.map(_ => RawDefaultValue.NullLit)
  }

  private def strLit[$: P]: P[RawDefaultValue.StrLit] = {
    Literals.Literals.Str.map(RawDefaultValue.StrLit.apply)
  }

  private def floatLit[$: P]: P[RawDefaultValue.FloatLit] = {
    import fastparse.NoWhitespace.noWhitespaceImplicit
    P(("-" | "+").?.! ~ Literals.Literals.Float.!).map { case (sign, raw) =>
      val fullRaw = sign + raw
      RawDefaultValue.FloatLit(fullRaw.toDouble, fullRaw)
    }
  }

  private def intLit[$: P]: P[RawDefaultValue.IntLit] = {
    import fastparse.NoWhitespace.noWhitespaceImplicit
    P(("-" | "+").?.! ~ Literals.Literals.Int.!).map { case (sign, raw) =>
      val fullRaw = sign + raw
      RawDefaultValue.IntLit(fullRaw.toLong, fullRaw)
    }
  }

  private def enumIdent[$: P]: P[RawDefaultValue.EnumRef] = {
    idt.symbolSeq.map(seq => RawDefaultValue.EnumRef(seq.mkString(".")))
  }

  // ─── context-function grammar (Phase A) ───────────────────────────────────

  // Single-capture limit: at most one `[name: Type...]` head per fn. Two captures
  // (`fn[a: A, b: B]...`) is a parse failure (the `]` expectation fails after the
  // first capture and the cut after `fn` prevents silent backtracking). Zero-capture
  // `fn(...){...}` (no brackets) stays valid. Previous-version `!old: T` prefix
  // syntax is deferred — `!` is not accepted here (unary `!` lives only in body exprs).
  def fnDecl[$: P]: P[RawFn] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    P("fn" ~/ ("[" ~ capture ~ "]").? ~ "(" ~ param.rep(sep = ",") ~ ")" ~ fnBody)
      .map { case (captures, params, body) => RawFn(captures.toList, params.toList, body) }
  }

  def captureAnnotation[$: P]: P[RawCapture] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    P("@" ~ kw.capture ~ "(" ~/ capture ~ ")")
  }

  /** Shared node-level prefix parser (see `NodePrefix`). */
  def nodePrefix[$: P]: P[NodePrefix] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    P(captureAnnotation.? ~ meta.specialMarker.?).map { case (cap, special) => NodePrefix(cap, special) }
  }

  /** Ordered capture-path tail shared by `capture`: star / field / key steps
    * in source order. Single helper (not a bare `.rep` at the call site) so
    * the capture-head rule reads as `name : rootType path`.
    */
  private def capturePath[$: P]: P[List[RawCaptureStep]] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    captureStep.rep.map(_.toList)
  }

  private def capture[$: P]: P[RawCapture] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    (idt.symbol ~ ":" ~/ rootType ~ capturePath).map {
      case (name, rt, path) => RawCapture(name, rt, path)
    }
  }

  private def captureStep[$: P]: P[RawCaptureStep] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    star.map(RawCaptureStep.Star.apply) | step.map(RawCaptureStep.Single.apply)
  }

  private def rootType[$: P]: P[RawTypeRef] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    (idt.symbol ~ ("[" ~/ dto.typeRef.rep(sep = ",") ~ "]").?).map {
      case (name, None)        => RawTypeRef.Simple(RawTypeName(name), Nil)
      case (name, Some(params)) => RawTypeRef.Constructor(RawTypeName(name), NEList.unsafeFrom(params.toList), Nil)
    }
  }

  private def star[$: P]: P[RawStarCapture] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    def steps = step.rep(1)
    P("(" ~/ "$" ~ idt.symbol ~ "*" ~ ":" ~/ steps ~ ")")
      .map { case (chainName, steps) => RawStarCapture(chainName, steps.toList) }
  }

  private def step[$: P]: P[RawLocatorSeg] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    // `.Some`→Field rule (single canonical note for capture `step` and body
    // `segment` alike): explicit `.Some` parses as Field("Some") — `opt`
    // payload situating is a typer concern — and collections stay transparent
    // as plain `.field` chains, with optional `[$k]` keys in instance-style
    // paths. No Variant case exists at AST level.
    P("." ~/ idt.symbol).map(RawLocatorSeg.Field.apply) |
    P("[" ~/ "$" ~ idt.symbol ~ "]").map(RawLocatorSeg.Key.apply)
  }

  private def param[$: P]: P[RawFnParam] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    // Bare `self` (no type) means the same role-dependent type as the legacy
    // `self: Self` alias (normalized here so the typer sees identical input).
    // Any other bare name is rejected: the typed alternative fails (no `:`)
    // and the bare alternative only matches `self`.
    (idt.symbol ~ ":" ~/ dto.typeRef).map { case (name, tpe) => RawFnParam(name, tpe) } |
      idt.symbol.filter(_ == kw.selfName).map { name => RawFnParam(name, RawTypeRef.Simple(RawTypeName(kw.SelfName), Nil)) }
  }

  private def fnBody[$: P]: P[RawBody] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    P("{" ~/ stmt.rep ~ expr ~ "}").map { case (stmts, result) =>
      val flat = stmts.toList
      RawBody(result, flat)
    }
  }

  private def stmt[$: P]: P[BodyStmt] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    P(assertStmt | forStmt | matchStmt | letStmt | assignStmt)
  }

  private def assertStmt[$: P]: P[BodyStmt] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    P(kw.assert ~/ "(" ~/ expr ~ ("," ~/ expr).? ~ ")" ~ ";").map {
      case (cond, msg) => BodyStmt.Assert(cond, msg)
    }
  }

  private def letStmt[$: P]: P[BodyStmt] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    P(kw.let ~/ idt.symbol ~ (":" ~/ dto.typeRef).? ~ "=" ~/ expr ~ ";").map {
      case (name, tpe, e) => BodyStmt.Let(name, tpe, e)
    }
  }

  private def assignStmt[$: P]: P[BodyStmt] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    // Lookahead-first: a bare `expr` here would fire inner cuts (callSuffix,
    // `[...]`, `(...)`, string literals, `if`) and then die on a missing `=`,
    // poisoning the enclosing `stmt.rep` fallback to the trailing expression.
    // The cut-free probe only commits when a depth-0 `=` (not `==`) follows.
    P(assignAhead ~ expr ~ "=" ~/ expr ~ ";").map { case (target, value) =>
      BodyStmt.Effect(BodyExpr.Assign(target, value))
    }
  }

  /** Cut-free probe for `symbol (.field | [...] | (...) | "...")* =` with a
    * single `=` (not `==`) next. Skips bracketed/paren regions and
    * double-quoted strings; `\"` escapes inside a probe abort it, which only
    * rejects exotic assigns, never valid trailing expressions.
    */
  private def assignAhead[$: P]: P[Unit] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    P(&(idt.symbol ~ assignAheadFrag.rep ~ "=" ~ !"="))
  }

  private def assignAheadFrag[$: P]: P[Unit] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    P(
      CharsWhile(c => c != '=' && c != ';' && c != '{' && c != '}' && c != '[' && c != ']' && c != '(' && c != ')' && c != '"', 1) |
        ("[" ~ assignAheadFrag.rep ~ "]") |
        ("(" ~ assignAheadFrag.rep ~ ")") |
        ("\"" ~ CharsWhile(_ != '"', 0) ~ "\"")
    )
  }

  private def forStmt[$: P]: P[BodyStmt] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    // Shared `for (` prefix with the cut before the branch: forC/forIn
    // alternatives below it backtrack cleanly (no cuts fire before `=`/`in`).
    P(kw.`for` ~/ "(" ~/ (forCRest | forInRest))
  }

  private def forInRest[$: P]: P[BodyStmt] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    P(idt.symbol ~ kw.in ~/ expr ~ ")" ~ block).map {
      case (varName, iterable, body) => BodyStmt.Effect(BodyExpr.ForIn(varName, iterable, body))
    }
  }

  private def forCRest[$: P]: P[BodyStmt] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    P(forInit ~ ";" ~/ expr ~ ";" ~/ forStep ~ ")" ~ block).map {
      case (init, cond, step, body) => BodyStmt.Effect(BodyExpr.ForC(init, cond, step, body))
    }
  }

  private def forInit[$: P]: P[ForInit] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    P(idt.symbol ~ (":" ~/ dto.typeRef).? ~ "=" ~/ expr).map {
      case (name, tpe, value) => ForInit(name, tpe, value)
    }
  }

  private def forStep[$: P]: P[BodyExpr] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    P((expr ~ "+=" ~/ expr).map { case (target, e) =>
      // `i += e` desugars to `i = i + e`; no dedicated AST node.
      BodyExpr.Assign(target, BodyExpr.BinOp("+", target, e))
    } | (expr ~ "=" ~/ expr).map { case (target, value) =>
      BodyExpr.Assign(target, value)
    })
  }

  private def matchStmt[$: P]: P[BodyStmt] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    P(kw.`match` ~/ "(" ~/ expr ~ ")" ~ "{" ~/ matchArm.rep(1) ~ "}").map {
      case (scrutinee, arms) => BodyStmt.Effect(BodyExpr.Match(scrutinee, arms.toList))
    }
  }

  private def matchArm[$: P]: P[MatchArm] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    P(matchPattern ~ "=>" ~/ block).map { case (pattern, body) => MatchArm(pattern, body) }
  }

  private def matchPattern[$: P]: P[MatchPattern] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    P("_".!.map(_ => MatchPattern.Wildcard: MatchPattern) |
      P(idt.symbol ~ ("(" ~/ idt.symbol ~ ")").?).map { case (name, binding) =>
        MatchPattern.Variant(name, binding): MatchPattern
      })
  }

  private def block[$: P]: P[BodyExpr.Block] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    P("{" ~/ stmt.rep ~ expr.? ~ "}").map { case (stmts, value) =>
      BodyExpr.Block(stmts.toList, value)
    }
  }

  /** Precedence-chain entry (lowest binding level): `orExpr`. `expr` tries
    * `ifExpr` first, then falls into the binary-operator chain here.
    */
  private def precedenceEntry[$: P]: P[BodyExpr] = P(orExpr)

  private def expr[$: P]: P[BodyExpr]        = P(ifExpr | precedenceEntry)
  private def orExpr[$: P]: P[BodyExpr]      = binOpLevel(orOp, andExpr)
  private def andExpr[$: P]: P[BodyExpr]     = binOpLevel(andOp, eqExpr)
  private def eqExpr[$: P]: P[BodyExpr]      = binOpLevel(eqOp, relExpr)
  private def relExpr[$: P]: P[BodyExpr]     = binOpLevel(relOp, addExpr)
  private def addExpr[$: P]: P[BodyExpr]     = binOpLevel(addOp, mulExpr)
  private def mulExpr[$: P]: P[BodyExpr]     = binOpLevel(mulOp, unary)

  private def ifExpr[$: P]: P[BodyExpr] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    P(kw.`if` ~/ unary ~ "{" ~/ expr ~ "}" ~ kw.`else` ~ "{" ~/ expr ~ "}").map {
      case (cond, thenE, elseE) => BodyExpr.If(cond, thenE, elseE)
    }
  }

  private def orOp[$: P]: P[String]  = P("||").!
  private def andOp[$: P]: P[String] = P("&&").!
  private def eqOp[$: P]: P[String]  = P("==" | "!=").!
  private def relOp[$: P]: P[String] = P("<=" | ">=" | "<" | ">").!
  private def addOp[$: P]: P[String] = P("+" | "-").!
  private def mulOp[$: P]: P[String] = P("*" | "/" | "%").!

  private def binOpLevel[$: P](ops: => P[String], next: => P[BodyExpr]): P[BodyExpr] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    P(next ~ (ops ~ next).rep).map { case (first, rest) =>
      rest.foldLeft(first) { case (l, (op, r)) => BodyExpr.BinOp(op, l, r) }
    }
  }

  private def unary[$: P]: P[BodyExpr] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    P((("-" | "!").! ~ primary).map { case (op, e) => BodyExpr.UnaryOp(op, e) } | primary)
  }

  private def primary[$: P]: P[BodyExpr] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    P((literal | pathOrCall | "(" ~/ expr ~ ")") ~ callSuffix.rep).map { case (target, calls) =>
      calls.foldLeft(target) { case (t, (method, args)) => BodyExpr.Call(t, method, args) }
    }
  }

  private def pathOrCall[$: P]: P[BodyExpr] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    P(idt.symbol ~ segment.rep).map { case (root, segs) => BodyExpr.Path(root, segs.toList) }
  }

  private def segment[$: P]: P[BodyExpr.Segment] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    // Same `.Some`→Field rule as capture `step` above: uniform `.field` shape.
    P("." ~ idt.symbol ~ !"(").map(BodyExpr.Segment.Field.apply) |
    P("[" ~/ expr ~ "]").map(BodyExpr.Segment.Index.apply)
  }

  private def callArgs[$: P]: P[List[BodyExpr]] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    P("(" ~ ")").map(_ => Nil) |
    P("(" ~/ expr ~ ("," ~/ expr).rep ~ ")").map { case (first, rest) =>
      first :: rest.toList
    }
  }

  private def callSuffix[$: P]: P[(String, List[BodyExpr])] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    // User-method calls parse as a generic Call; the typer decides whether
    // `method` is a builtin or a user-defined (computed-fn) method on the
    // receiver type. Unknown names are NOT rejected here.
    P("." ~/ idt.symbol ~ callArgs).map { case (method, args) =>
      (method, args)
    }
  }

  private def literal[$: P]: P[BodyExpr] = {
    P(bodyFloatLit | bodyIntLit | bodyBoolLit | bodyNullLit | bodyStrLit)
  }

  private def bodyIntLit[$: P]: P[BodyExpr] = {
    import fastparse.NoWhitespace.noWhitespaceImplicit
    P(Literals.Literals.Int.!).map(raw => BodyExpr.Lit(BodyLiteral.IntLit(raw.toLong)))
  }

  private def bodyFloatLit[$: P]: P[BodyExpr] = {
    import fastparse.NoWhitespace.noWhitespaceImplicit
    P(Literals.Literals.Float.!).map(raw => BodyExpr.Lit(BodyLiteral.FloatLit(raw.toDouble)))
  }

  private def bodyBoolLit[$: P]: P[BodyExpr] = {
    import fastparse.NoWhitespace.noWhitespaceImplicit
    (kw.boolTrue.map(_ => BodyExpr.Lit(BodyLiteral.BoolLit(true))) |
      kw.boolFalse.map(_ => BodyExpr.Lit(BodyLiteral.BoolLit(false))))
  }

  private def bodyNullLit[$: P]: P[BodyExpr] = {
    import fastparse.NoWhitespace.noWhitespaceImplicit
    kw.nullLit.map(_ => BodyExpr.Lit(BodyLiteral.NullLit))
  }

  private def bodyStrLit[$: P]: P[BodyExpr] = {
    import fastparse.NoWhitespace.noWhitespaceImplicit
    Literals.Literals.Str.map(s => BodyExpr.Lit(BodyLiteral.StrLit(s)))
  }

  /** Field-definition core shared by plain fields and `= fn` properties.
    *
    * Container-self clarification (no grammar change here — roles are carried
    * by the produced member):
    *   - a trailing `= fn(self) { ... }` parses into the returned `RawFn`
    *     and becomes an `FnProperty` (role `Computed`): `self` is the
    *     enclosing container instance (the owner DTO), and capture paths on it
    *     situate the container subtree (transparent through `opt`/`set`/`map`,
    *     `opt.Some` implied);
    *   - `@normalize(fn(self) { ... })` / `@default(fn(self) { ... })`
    *     keep the fn on the `RawField` (roles `Normalize` / `Default`):
    *     `self` there is the field's own value (the declared field type).
    * P2 resolves `Self` per role accordingly.
    */
  def fieldDefCore[$: P]: P[(RawField, Option[RawFn])] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    (fieldAnnotations ~ dto.fieldName ~ ":" ~/ dto.typeRef ~ ("=" ~/ fnDecl).? ~ fieldWas.?).map {
      case (ann, name, tpe, propFn, prev) =>
        // First-class fn home: an `@default(fn...)` initializer lives in
        // `RawField.fn` (with `default = None`), same as a trailing `= fn`
        // property initializer. Roles stay on the member wrapper (`FieldDef`
        // + fn => `Default`, `FnProperty` => `Computed`). A pathological
        // `@default(fn...) x: T = fn...` keeps the legacy dual encoding
        // (`propFn` wins the `fn` slot) so no initializer is silently dropped.
        ann.default match {
          case Some(RawDefaultValue.Fn(f)) if propFn.isEmpty =>
            (model.RawField(name, tpe, prev, None, ann.normalize, ann.migrate, fn = Some(f)), None)
          case _ =>
            (model.RawField(name, tpe, prev, ann.default, ann.normalize, ann.migrate), propFn)
        }
    }
  }

  // Field-definition branch with optional postfix `//!` doc capture.
  //
  // The connection between `fieldDef` and `suffixDoc` runs under `NoWhitespace`
  // so that no NLC can be consumed between the field's last token and the `//!`
  // marker -- otherwise `BaboonWhitespace` (which eats `\n`) would let a
  // freestanding `//!` on the next line silently bind to the previous field.
  // `suffixDoc` itself consumes a leading run of `[ \t]*` before the marker.
  // [PR-30.2-D01].
  def fieldDefWithSuffix[$: P]: P[((RawField, Option[RawFn]), Option[RawDocComment])] = {
    import fastparse.NoWhitespace.noWhitespaceImplicit
    noWhitespaceImplicit.discard()
    P(fieldDefCore ~ docs.suffixDoc).map { case (field, propFn, suffix) =>
      ((field, propFn), suffix)
    }
  }
}
