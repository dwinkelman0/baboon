package io.septimalmind.baboon.parser.defns

import fastparse.*
import io.septimalmind.baboon.parser.{ParserContext, model}
import io.septimalmind.baboon.parser.defns.base.{DefDocs, Literals, idt, kw, struct}
import io.septimalmind.baboon.parser.model.RawDtoMember.ContractRef
import io.septimalmind.baboon.parser.model.{BodyExpr, BodyLiteral, BodyStmt, ExtractionKind, ForInit, MatchArm, MatchPattern, RawBody, RawCapture, RawCaptureStep, RawContractRef, RawDefaultValue, RawDocComment, RawDocs, RawDto, RawDtoMember, RawField, RawFieldName, RawFn, RawFnParam, RawIdentifier, RawLocatorSeg, RawMigrate, RawStarCapture, RawTypeName, RawTypeRef, ScopedRef}
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.language.Quirks.Discarder

class DefDto(context: ParserContext, meta: DefMeta, docs: DefDocs) {
  context.discard()

  def typeParams[$: P]: P[NEList[RawTypeRef]] = {
    import fastparse.SingleLineWhitespace.whitespace
    ("[" ~ typeRef.rep(min = 1, sep = ",") ~ "]")
      .map(p => NEList.unsafeFrom(p.toList))
  }

  /** Parses the optional type-parameter head `[T1, T2, …]` on a template declaration.
    *
    * Grammar: `"[" idt.symbol ("," idt.symbol)* "]"` — bare identifiers only (NOT typeRef), at
    * least one parameter required so that `[]` produces a parse failure.
    */
  def templateHead[$: P]: P[List[RawTypeName]] = {
    import fastparse.SingleLineWhitespace.whitespace
    ("[" ~ idt.symbol.rep(min = 1, sep = ",") ~ "]")
      .map(p => p.toList.map(RawTypeName.apply))
  }

  def nonGenericTypeRef[$: P]: P[ScopedRef] = {
    idt.symbolSeq.map(s => ScopedRef(NEList.unsafeFrom(s.map(p => RawTypeName(p)).toList)))
  }

  private def anyQualifier[$: P]: P[RawTypeRef.AnyRef.Qual] = {
    // Qualifier tokens have no internal whitespace (e.g. `domain: this` must not parse).
    // Pin the no-whitespace contract at compile time: if a future edit introduces a `~` inside
    // this body it will use `NoWhitespace`, preserving the strict behavior instead of silently
    // inheriting the caller's `SingleLineWhitespace`. `.discard()` keeps the implicit in scope
    // without triggering the "unused import" warning when no `~` is currently present.
    import fastparse.NoWhitespace.noWhitespaceImplicit
    noWhitespaceImplicit.discard()
    P("domain:this").map(_ => RawTypeRef.AnyRef.DomainThis) |
    P("domain:current").map(_ => RawTypeRef.AnyRef.DomainCurrent)
  }

  private def anyTypeRefArgs[$: P]: P[(Option[RawTypeRef.AnyRef.Qual], Option[RawTypeRef])] = {
    import fastparse.SingleLineWhitespace.whitespace
    // Strict arg order: qualifier first (optional), then underlying typeRef (optional). Empty brackets rejected.
    //
    // Leading `CharsWhileIn(" \t", 0)` explicitly consumes any spaces/tabs between the `any`
    // identifier and `[`. We cannot rely on the surrounding implicit: this parser is invoked
    // from a `flatMap` continuation in `typeRef`, which does not insert a `~` boundary before
    // the call site. Without this, `any [T]` silently truncates to `Simple("any", Nil)` and the
    // `[T]` tail dangles — the exact partial-consume failure mode described in D15.
    P(
      CharsWhileIn(" \t", 0) ~ "[" ~/ (
        (anyQualifier ~ ("," ~ typeRef).?).map { case (q, t) => (Some(q): Option[RawTypeRef.AnyRef.Qual], t) } |
        typeRef.map(t => (None: Option[RawTypeRef.AnyRef.Qual], Some(t)))
      ) ~ "]"
    )
  }

  def typeRef[$: P]: P[RawTypeRef] = {
    import fastparse.SingleLineWhitespace.whitespace
    // `any` is syntactically special ONLY when it appears as `any[...]` at the head of a typeRef.
    // Bare `any` and prefixed forms like `foo.any` / `any.Foo` are parsed as ordinary identifiers;
    // the typer decides whether the identifier resolves to the builtin or to a user type.
    //
    // We parse the identifier path atomically first (no backtrack hazards after `"any"` consumes
    // 3 chars), then branch: when the head is exactly `any` (single segment, no prefix), route
    // to `anyTypeRefArgs` if `[` follows; otherwise fall through to the generic shape.
    nonGenericTypeRef.flatMap {
      ref =>
        val name   = ref.path.last.name
        val prefix = ref.path.toList.init

        if (name == "any" && prefix.isEmpty) {
          // Head is bare `any`. If brackets follow, interpret as the any-builtin syntax; otherwise
          // treat as an ordinary identifier.
          anyTypeRefArgs.map { case (q, u) => RawTypeRef.AnyRef(q, u): RawTypeRef } |
          Pass(RawTypeRef.Simple(RawTypeName(name), prefix): RawTypeRef)
        } else {
          typeParams.?.map {
            case Some(value) => RawTypeRef.Constructor(RawTypeName(name), value, prefix)
            case None        => RawTypeRef.Simple(RawTypeName(name), prefix)
          }
        }
    }
  }

  def fieldName[$: P]: P[RawFieldName] =
    idt.symbol.map(name => RawFieldName(name))

  def contractDef[$: P]: P[RawContractRef] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    ("is" ~ nonGenericTypeRef).map { case t => model.RawContractRef(t) }
  }

  def extendedContractRef[$: P]: P[ContractRef] = {
    meta
      .withMeta(contractDef)
      .map { case (meta, ref) => ContractRef(ref, meta) }
  }

  private def fieldWas[$: P]: P[RawFieldName] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    kw.was ~ fieldName
  }

  private def defaultAnnotation[$: P]: P[RawDefaultValue] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    "@" ~ kw.default ~ "(" ~/ defaultArg ~ ")"
  }

  private def normalizeAnnotation[$: P]: P[RawFn] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    "@" ~ kw.normalize ~ "(" ~/ fnDecl ~ ")"
  }

  private def migrateAnnotation[$: P]: P[RawMigrate] = {
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
    (kw.kw("nan").map(_ => RawDefaultValue.SpecialFloat.NaN: RawDefaultValue.SpecialFloat) |
      kw.kw("+inf").map(_ => RawDefaultValue.SpecialFloat.PosInf: RawDefaultValue.SpecialFloat) |
      kw.kw("-inf").map(_ => RawDefaultValue.SpecialFloat.NegInf: RawDefaultValue.SpecialFloat))
  }

  private def boolLit[$: P]: P[RawDefaultValue.BoolLit] = {
    (kw.kw("true").map(_ => RawDefaultValue.BoolLit(true)) |
      kw.kw("false").map(_ => RawDefaultValue.BoolLit(false)))
  }

  private def nullLit[$: P]: P[RawDefaultValue.NullLit.type] = {
    kw.kw("null").map(_ => RawDefaultValue.NullLit)
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
  private def fnDecl[$: P]: P[RawFn] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    P("fn" ~/ ("[" ~ capture ~ "]").? ~ "(" ~ param.rep(sep = ",") ~ ")" ~ fnBody)
      .map { case (captures, params, body) => RawFn(captures.toList, params.toList, body) }
  }

  def captureAnnotation[$: P]: P[RawCapture] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    P("@" ~ kw.kw("capture") ~ "(" ~/ capture ~ ")")
  }

  private def capture[$: P]: P[RawCapture] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    (idt.symbol ~ ":" ~/ rootType ~ captureStep.rep).map {
      case (name, rt, path) => RawCapture(name, rt, path.toList)
    }
  }

  private def captureStep[$: P]: P[RawCaptureStep] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    star.map(RawCaptureStep.Star.apply) | step.map(RawCaptureStep.Single.apply)
  }

  private def rootType[$: P]: P[RawTypeRef] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    (idt.symbol ~ ("[" ~/ typeRef.rep(sep = ",") ~ "]").?).map {
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
    // No grammar change needed for transparent collections: traversal through
    // lst/map/set/opt (including implied opt.Some payload situating) is just a
    // `.field` chain, with optional `[$k]` keys in instance-style paths.
    // Explicit `.Some` parses as Field("Some") here (typer resolves it later);
    // Variant parsing is intentionally NOT reintroduced at AST level.
    P("." ~/ idt.symbol).map(RawLocatorSeg.Field.apply) |
    P("[" ~/ "$" ~ idt.symbol ~ "]").map(RawLocatorSeg.Key.apply)
  }

  private def param[$: P]: P[RawFnParam] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    // Bare `self` (no type) means the same role-dependent type as the legacy
    // `self: Self` alias (normalized here so the typer sees identical input).
    // Any other bare name is rejected: the typed alternative fails (no `:`)
    // and the bare alternative only matches `self`.
    (idt.symbol ~ ":" ~/ typeRef).map { case (name, tpe) => RawFnParam(name, tpe) } |
      idt.symbol.filter(_ == "self").map { name => RawFnParam(name, RawTypeRef.Simple(RawTypeName("Self"), Nil)) }
  }

  private def fnBody[$: P]: P[RawBody] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    P("{" ~/ stmt.rep ~ expr ~ "}").map { case (stmts, result) =>
      val flat = stmts.toList
      RawBody(flat.collect { case BodyStmt.Let(n, _, v) => (n, v) }, result, flat)
    }
  }

  private def stmt[$: P]: P[BodyStmt] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    P(assertStmt | forStmt | matchStmt | letStmt | assignStmt)
  }

  private def assertStmt[$: P]: P[BodyStmt] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    P(kw.kw("assert") ~/ "(" ~/ expr ~ ("," ~/ expr).? ~ ")" ~ ";").map {
      case (cond, msg) => BodyStmt.Assert(cond, msg)
    }
  }

  private def letStmt[$: P]: P[BodyStmt] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    P(kw.kw("let") ~/ idt.symbol ~ (":" ~/ typeRef).? ~ "=" ~/ expr ~ ";").map {
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
    P(kw.kw("for") ~/ "(" ~/ (forCRest | forInRest))
  }

  private def forInRest[$: P]: P[BodyStmt] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    P(idt.symbol ~ kw.kw("in") ~/ expr ~ ")" ~ block).map {
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
    P(idt.symbol ~ (":" ~/ typeRef).? ~ "=" ~/ expr).map {
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
    P(kw.kw("match") ~/ "(" ~/ expr ~ ")" ~ "{" ~/ matchArm.rep(1) ~ "}").map {
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

  private def expr[$: P]: P[BodyExpr]        = P(ifExpr | orExpr)
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
    (kw.kw("true").map(_ => BodyExpr.Lit(BodyLiteral.BoolLit(true))) |
      kw.kw("false").map(_ => BodyExpr.Lit(BodyLiteral.BoolLit(false))))
  }

  private def bodyNullLit[$: P]: P[BodyExpr] = {
    import fastparse.NoWhitespace.noWhitespaceImplicit
    kw.kw("null").map(_ => BodyExpr.Lit(BodyLiteral.NullLit))
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
  private def fieldDefCore[$: P]: P[(RawField, Option[RawFn])] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    (defaultAnnotation.? ~ normalizeAnnotation.? ~ migrateAnnotation.? ~ fieldName ~ ":" ~/ typeRef ~ ("=" ~/ fnDecl).? ~ fieldWas.?).map {
      case (d, n, m, name, tpe, propFn, prev) =>
        (model.RawField(name, tpe, prev, d, n, m), propFn)
    }
  }

  def fieldDef[$: P]: P[RawField] = fieldDefCore.map(_._1)

  // NOTE: BaboonWhitespace (imported below) consumes newlines, so `nonGenericTypeRef ~ typeParams.?`
  // allows `[…]` on a separate line to silently bind as args. This is safe contingent on no other
  // dtoMember alternative starting with `[`; a future extension that introduces one would break this
  // silently. Pinned by the cross-line binding tripwire test in M33StructuralTemplateInheritanceParserTest.
  def parentDef[$: P]: P[(ScopedRef, Option[NEList[RawTypeRef]])] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    ("+" ~ nonGenericTypeRef ~ typeParams.?).map { case (ref, args) => (ref, args) }
  }

  def unparentDef[$: P]: P[(ScopedRef, Option[NEList[RawTypeRef]])] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    ("-" ~ nonGenericTypeRef ~ typeParams.?).map { case (ref, args) => (ref, args) }
  }

  def intersectionDef[$: P]: P[(ScopedRef, Option[NEList[RawTypeRef]])] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    ("^" ~ nonGenericTypeRef ~ typeParams.?).map { case (ref, args) => (ref, args) }
  }

  def unfieldDef[$: P]: P[RawField] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    "-" ~ fieldDef.map(f => f.copy(default = None))
  }

  /** Parses `has (mirror | contract) <bare-identifier>` (T37).
    *
    * Grammar (BaboonWhitespace):  `has` keyword — then `mirror` or `contract` keyword — then a
    * bare symbol (no dot-qualified path, no type args).
    *
    * D15 discipline: this alternative does NOT use `/~` (cut) after `kw.has`, so that a field
    * named `has` (e.g. `has: i32`) still backtracks cleanly to `fieldDef`. The order in
    * `dtoMember` places the `meta.withMeta(extractionDef)` branch before `fieldDef` so it is
    * tried first, but the absence of a cut means failure inside `extractionDef` reverts the
    * cursor to before `has`. Fields named `mirror` or `contract` are unaffected — `fieldDef`
    * does not restrict `fieldName` to non-keywords.
    */
  private def extractionDefCore[$: P]: P[(ExtractionKind, RawTypeName)] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    kw.has ~ (kw.mirror.map(_ => ExtractionKind.Mirror: ExtractionKind) | kw.contract.map(_ => ExtractionKind.Contract: ExtractionKind)) ~ idt.symbol.map(RawTypeName.apply)
  }

  def extendedExtractionDef[$: P]: P[RawDtoMember.ExtractionDef] = {
    meta.withMeta(extractionDefCore).map { case (m, (kind, name)) => RawDtoMember.ExtractionDef(kind, name, m) }
  }

  // Field-definition branch with optional postfix `//!` doc capture.
  //
  // The connection between `fieldDef` and `suffixDoc` runs under `NoWhitespace`
  // so that no NLC can be consumed between the field's last token and the `//!`
  // marker -- otherwise `BaboonWhitespace` (which eats `\n`) would let a
  // freestanding `//!` on the next line silently bind to the previous field.
  // `suffixDoc` itself consumes a leading run of `[ \t]*` before the marker.
  // [PR-30.2-D01].
  private def fieldDefWithSuffix[$: P]: P[((RawField, Option[RawFn]), Option[RawDocComment])] = {
    import fastparse.NoWhitespace.noWhitespaceImplicit
    noWhitespaceImplicit.discard()
    P(fieldDefCore ~ docs.suffixDoc).map { case (field, propFn, suffix) =>
      ((field, propFn), suffix)
    }
  }

  def dtoMember[$: P]: P[RawDtoMember] = {
    // No `~` operators are composed directly inside this body -- each branch's
    // whitespace contract is managed by its own helper (`fieldDefWithSuffix`,
    // `parentDef`, `meta.withMeta(...)`, etc.). The `|` alternation operator
    // does not consume whitespace, so no implicit Whitespace import is needed
    // at this scope.
    P(meta.withMeta(fieldDefWithSuffix)).map {
      case (m, ((field, propFn), suffix)) =>
        val merged = m.copy(docs = RawDocs(m.docs.prefix, suffix))
        propFn match {
          case Some(fn) => model.RawDtoMember.FnProperty(field.copy(default = Some(RawDefaultValue.Fn(fn))), merged)
          case None     => model.RawDtoMember.FieldDef(field, merged)
        }
    } | P(meta.withMeta(parentDef)).map {
      case (meta, (parent, args)) =>
        model.RawDtoMember.ParentDef(parent, meta, args)
    } | P(meta.withMeta(unfieldDef)).map {
      case (meta, field) =>
        model.RawDtoMember.UnfieldDef(field, meta)
    } | P(meta.withMeta(unparentDef)).map {
      case (meta, (parent, args)) =>
        model.RawDtoMember.UnparentDef(parent, meta, args)
    } | P(meta.withMeta(intersectionDef)).map {
      case (meta, (parent, args)) =>
        model.RawDtoMember.IntersectionDef(parent, meta, args)
    } | extendedContractRef | extendedExtractionDef
  }

  def dto[$: P]: P[Seq[RawDtoMember]] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    P(dtoMember.rep())
  }

  def dtoEnclosed[$: P]: P[RawDto] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    P(captureAnnotation.? ~ meta.member(kw.data, templateHead.? ~ meta.derived ~ meta.specialMarker.? ~ struct.enclosed(dto))).map {
      case (cap, (meta, name, (tps, derived, special, members))) =>
        RawDto(RawTypeName(name), members, derived, meta, tps.getOrElse(Nil), special, cap)
    }
  }

  def identifierEnclosed[$: P]: P[RawIdentifier] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    P(captureAnnotation.? ~ meta.member(kw.identifier, templateHead.? ~ meta.derived ~ meta.specialMarker.? ~ struct.enclosed(dto))).map {
      case (cap, (meta, name, (tps, derived, special, members))) => RawIdentifier(RawTypeName(name), members, derived, meta, tps.getOrElse(Nil), special, cap)
    }
  }

}
