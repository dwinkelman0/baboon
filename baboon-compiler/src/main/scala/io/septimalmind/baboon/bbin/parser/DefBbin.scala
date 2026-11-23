package io.septimalmind.baboon.bbin.parser

import fastparse.*
import fastparse.CharPredicates.{isDigit, isLetter}
import io.septimalmind.baboon.bbin.model.*
import io.septimalmind.baboon.parser.ParserContext
import io.septimalmind.baboon.parser.defns.base.{BaboonWhitespace, Literals, idt}
import io.septimalmind.baboon.parser.model.RawTypeName

/** Grammar for the `.bbin` parser subset (parse level only).
  *
  * In scope:
  * - `using some.pkg @"1.0.0"` header (bare versions like `@1.0.0` also accepted)
  * - `import (a, a: T, b as c, d as e: T) from rel:'file.bbin'` (selective
  *   imports with optional `as` renaming and optional `: Type` ascription;
  *   ascribed imports unseal at import by copy — `g: ~SoC` is mutable)
  * - statements: `let name: Type = EXPR`, `target[.path] (=|+=|*=) EXPR`,
  *   `for ((idx, row) in set) { stmts }` (binders must differ and are
  *   read-only in the body), `with provenance({...}) { stmts }`
  * - types: plain baboon refs, `~T` partials, `{T}` sets,
  *   `{idx: idx_type > T}` indexed sets
  * - expressions: literals (str/int/float/bool/None/undefined), bare
  *   identifiers, struct construction (`Type { f = v, ... }`, `=` field
  *   delimiters, no enclosing braces), set literals (`{k: Row { ... }}` keyed
  *   or `{Row { ... }}` positional — never mixed; row fields use `=`, the
  *   outer `:` delimits keyed entries only), `CsvLoader(T, rel:'f')` /
  *   `JsonLoader(T, rel:'f')` calls (loader type args accept the same four
  *   `BbinType` forms as `let` annotations and import ascriptions),
  *   `rel:'...'` path literals, `.a.b[0]`
  *   schema paths, binary comparisons/arithmetic, `leaf(base, {...})`,
  *   `partial.seal([x])`, and method chains (`.map/.filter/.cross/
  *   .innerJoin/.outerJoin/.leftJoin/.rightJoin/.zip/.concat/.reindex/
  *   .seal/.unseal/.with_provenance/.unwrap`).
  *
  * Parse-level rejections: mixed keyed/positional sets, bare rows,
  * brace-wrapped `{Type{...}}` struct forms, free `seal(...)` calls
  * (sealing is method-only: `x.seal()` / `partial.seal([x])`), free
  * `cross()/zip()/concat()/filter()` calls, single-arg `leaf()`,
  * single-binder `for`, binder shadowing or writes to read-only binders,
  * bare-root paths like `ops[0]` (schema paths require the leading dot),
  * `.concat()` after a seal in one chain (sealed/partial concat mixing),
  * and anything else — all fail with a `ParserFailed` error.
  */
class DefBbin(context: ParserContext) {

  def bbinFile[$: P]: P[(List[BbinUsing], List[BbinImport], List[BbinStmt])] = {
    import BaboonWhitespace.whitespace
    P(Start ~ usingStmt.rep(1) ~ importStmt.rep() ~ bbinStmt.rep() ~ End).map {
      case (usings, imports, stmts) =>
        (usings.toList, imports.toList, stmts.toList)
    }
  }

  def usingStmt[$: P]: P[BbinUsing] = {
    import BaboonWhitespace.whitespace
    P(keyword("using") ~/ idt.symbolSeq ~ "@" ~ versionToken).map {
      case (pkg, version) => BbinUsing(pkg.mkString("."), version)
    }
  }

  def importStmt[$: P]: P[BbinImport] = {
    import BaboonWhitespace.whitespace
    P(keyword("import") ~/ "(" ~ importName.rep(sep = ",", min = 1) ~ ",".? ~ ")" ~ keyword("from") ~ relLit).map {
      case (names, from) => BbinImport(names.toList, from)
    }
  }

  private def importName[$: P]: P[BbinImportItem] = {
    import BaboonWhitespace.whitespace
    P(idt.symbol ~ (keyword("as") ~ idt.symbol).? ~ (":" ~ bbinType).?).map {
      case (n, a, t) => BbinImportItem(n, a, t)
    }
  }

  def bbinStmt[$: P]: P[BbinStmt] = {
    import BaboonWhitespace.whitespace
    P(letStmt | forStmt | withStmt | assignStmt)
  }

  def letStmt[$: P]: P[BbinStmt] = {
    import BaboonWhitespace.whitespace
    P(keyword("let") ~/ idt.symbol ~ ":" ~ bbinType ~ "=" ~ bbinExpr).map {
      case (name, tpe, expr) => BbinStmt.LetStmt(BbinLet(name, tpe, expr))
    }
  }

  def forStmt[$: P]: P[BbinStmt] = {
    import BaboonWhitespace.whitespace
    P(
      keyword("for") ~ "(" ~ "(" ~ idt.symbol ~ "," ~ idt.symbol ~ ")" ~ keyword("in") ~ bbinExpr ~ ")" ~ "{" ~ bbinStmt.rep() ~ "}"
    ).filter {
      case (idx, row, _, body) => idx != row && !writesOrBinds(body.toList, Set(idx, row))
    }.map {
      case (idx, row, set, body) => BbinStmt.ForStmt(BbinFor(idx, row, set, body.toList))
    }
  }

  def withStmt[$: P]: P[BbinStmt] = {
    import BaboonWhitespace.whitespace
    P(keyword("with") ~ keyword("provenance") ~ "(" ~ "{" ~ structField.rep(sep = ",") ~ ",".? ~ "}" ~ ")" ~ "{" ~ bbinStmt.rep() ~ "}").map {
      case (fields, body) => BbinStmt.WithStmt(BbinWith(fields.toList, body.toList))
    }
  }

  def assignStmt[$: P]: P[BbinStmt] = {
    import BaboonWhitespace.whitespace
    P(assignTarget ~ assignOp ~ bbinExpr).map {
      case (target, op, value) => BbinStmt.AssignStmt(BbinAssign(target, op, value))
    }
  }

  /** Binders are read-only: a `for` body must not assign to them, re-bind
    * them with `let`, or shadow them with a nested `for` binder. Nested
    * `for` bodies are separate scopes and checked by their own rule.
    */
  private def writesOrBinds(stmts: List[BbinStmt], binders: Set[String]): Boolean =
    stmts.exists {
      case BbinStmt.LetStmt(l) => binders.contains(l.name)
      case BbinStmt.AssignStmt(a) =>
        a.target.segments.headOption.exists {
          case BbinPathSeg.Field(name) => binders.contains(name)
          case _                       => false
        }
      case BbinStmt.ForStmt(f) =>
        binders.contains(f.idx) || binders.contains(f.row)
      case BbinStmt.WithStmt(w) => writesOrBinds(w.body, binders)
    }

  def bbinType[$: P]: P[BbinType] = {
    import BaboonWhitespace.whitespace
    P(partialType | indexedSetType | setOfType | plainType)
  }

  private def partialType[$: P]: P[BbinType] = {
    import BaboonWhitespace.whitespace
    P("~" ~ context.defDto.typeRef).map(ref => BbinType.Partial(ref): BbinType)
  }

  private def indexedSetType[$: P]: P[BbinType] = {
    import BaboonWhitespace.whitespace
    P("{" ~ idt.symbol ~ ":" ~ context.defDto.typeRef ~ ">" ~ context.defDto.typeRef ~ "}").map {
      case (idx, idxType, elem) => BbinType.IndexedSet(idx, idxType, elem): BbinType
    }
  }

  private def setOfType[$: P]: P[BbinType] = {
    import BaboonWhitespace.whitespace
    P("{" ~ context.defDto.typeRef ~ "}").map(ref => BbinType.SetOf(ref): BbinType)
  }

  private def plainType[$: P]: P[BbinType] = {
    import BaboonWhitespace.whitespace
    P(context.defDto.typeRef).map(ref => BbinType.Plain(ref): BbinType)
  }

  def bbinExpr[$: P]: P[BbinExpr] = {
    import BaboonWhitespace.whitespace
    P(cmpExpr)
  }

  private def cmpExpr[$: P]: P[BbinExpr] = {
    import BaboonWhitespace.whitespace
    P(addExpr ~ (cmpOp ~ addExpr).rep()).map {
      case (head, tail) => tail.foldLeft(head) { case (l, (op, r)) => BbinExpr.BinOp(op, l, r) }
    }
  }

  private def cmpOp[$: P]: P[BbinBinOp] = {
    import BaboonWhitespace.whitespace
    P(
      "==".!.map(_ => BbinBinOp.Eq: BbinBinOp) |
        "!=".!.map(_ => BbinBinOp.Ne: BbinBinOp) |
        "<=".!.map(_ => BbinBinOp.Le: BbinBinOp) |
        ">=".!.map(_ => BbinBinOp.Ge: BbinBinOp) |
        "<".!.map(_ => BbinBinOp.Lt: BbinBinOp) |
        ">".!.map(_ => BbinBinOp.Gt: BbinBinOp)
    )
  }

  private def addExpr[$: P]: P[BbinExpr] = {
    import BaboonWhitespace.whitespace
    P(mulExpr ~ (("+" | "-").! ~ mulExpr).rep()).map {
      case (head, tail) =>
        tail.foldLeft(head) {
          case (l, (op, r)) => BbinExpr.BinOp(if (op == "+") BbinBinOp.Add else BbinBinOp.Sub, l, r)
        }
    }
  }

  private def mulExpr[$: P]: P[BbinExpr] = {
    import BaboonWhitespace.whitespace
    P(chainExpr ~ (("*" | "/").! ~ chainExpr).rep()).map {
      case (head, tail) =>
        tail.foldLeft(head) {
          case (l, (op, r)) => BbinExpr.BinOp(if (op == "*") BbinBinOp.Mul else BbinBinOp.Div, l, r)
        }
    }
  }

  private def chainExpr[$: P]: P[BbinExpr] = {
    import BaboonWhitespace.whitespace
    P(chainBase ~ chainStep.rep())
      .filter { case (base, ops) => chainOk(base, ops.toList) }
      .map {
        case (base, ops) if ops.isEmpty => base
        case (base, ops)                => BbinExpr.Chain(base, ops.toList)
      }
  }

  /** Sealed values stay frozen: `.concat()` after a seal in one chain, or on
    * a `partial.seal(...)` base, is rejected at parse level.
    */
  private def chainOk(base: BbinExpr, ops: List[BbinChainOp]): Boolean = {
    var sealedSeen = base match {
      case BbinExpr.PartialSeal(_) => true
      case _                       => false
    }
    for (op <- ops) {
      op match {
        case BbinChainOp.Concat(_) if sealedSeen => return false
        case BbinChainOp.Seal                    => sealedSeen = true
        case _                                   =>
      }
    }
    true
  }

  private def chainBase[$: P]: P[BbinExpr] = {
    import BaboonWhitespace.whitespace
    P(
      partialSealExpr | leafExpr | loaderExpr | setExpr | structExpr | relExpr | litExpr | schemaPathExpr | identExpr | parenExpr
    )
  }

  private def chainStep[$: P]: P[BbinChainOp] = {
    import BaboonWhitespace.whitespace
    P(
      "." ~ (mapChain | filterChain | crossChain | innerJoinChain | outerJoinChain | leftJoinChain | rightJoinChain | zipChain | concatChain | reindexChain | sealChain | unsealChain | withProvenanceChain | unwrapChain)
    )
  }

  private def mapChain[$: P]: P[BbinChainOp] = {
    import BaboonWhitespace.whitespace
    P(keyword("map") ~ "(" ~ mapArg.rep(sep = ",", min = 1) ~ ",".? ~ ")").map {
      args => BbinChainOp.Map(args.toList)
    }
  }

  private def filterChain[$: P]: P[BbinChainOp] = {
    import BaboonWhitespace.whitespace
    P(keyword("filter") ~ "(" ~ bbinExpr ~ ")").map(BbinChainOp.Filter.apply)
  }

  private def crossChain[$: P]: P[BbinChainOp] = {
    import BaboonWhitespace.whitespace
    P(keyword("cross") ~ "(" ~ bbinExpr ~ ")").map(BbinChainOp.Cross.apply)
  }

  private def innerJoinChain[$: P]: P[BbinChainOp] = {
    import BaboonWhitespace.whitespace
    P(keyword("innerJoin") ~ "(" ~ bbinExpr ~ ")").map(BbinChainOp.InnerJoin.apply)
  }

  private def outerJoinChain[$: P]: P[BbinChainOp] = {
    import BaboonWhitespace.whitespace
    P(keyword("outerJoin") ~ "(" ~ bbinExpr ~ ")").map(BbinChainOp.OuterJoin.apply)
  }

  private def leftJoinChain[$: P]: P[BbinChainOp] = {
    import BaboonWhitespace.whitespace
    P(keyword("leftJoin") ~ "(" ~ bbinExpr ~ ")").map(BbinChainOp.LeftJoin.apply)
  }

  private def rightJoinChain[$: P]: P[BbinChainOp] = {
    import BaboonWhitespace.whitespace
    P(keyword("rightJoin") ~ "(" ~ bbinExpr ~ ")").map(BbinChainOp.RightJoin.apply)
  }

  private def zipChain[$: P]: P[BbinChainOp] = {
    import BaboonWhitespace.whitespace
    P(keyword("zip") ~ "(" ~ bbinExpr ~ ")").map(BbinChainOp.Zip.apply)
  }

  private def concatChain[$: P]: P[BbinChainOp] = {
    import BaboonWhitespace.whitespace
    P(keyword("concat") ~ "(" ~ bbinExpr ~ ")").map(BbinChainOp.Concat.apply)
  }

  private def reindexChain[$: P]: P[BbinChainOp] = {
    import BaboonWhitespace.whitespace
    P(keyword("reindex") ~ "(" ~ bbinExpr ~ ")").map(BbinChainOp.Reindex.apply)
  }

  private def sealChain[$: P]: P[BbinChainOp] = {
    import BaboonWhitespace.whitespace
    P(keyword("seal") ~ "(" ~ ")").map(_ => BbinChainOp.Seal)
  }

  private def unsealChain[$: P]: P[BbinChainOp] = {
    import BaboonWhitespace.whitespace
    P(keyword("unseal") ~ "(" ~ ")").map(_ => BbinChainOp.Unseal)
  }

  private def withProvenanceChain[$: P]: P[BbinChainOp] = {
    import BaboonWhitespace.whitespace
    P(keyword("with_provenance") ~ "(" ~ "{" ~ structField.rep(sep = ",") ~ ",".? ~ "}" ~ ")").map {
      fields => BbinChainOp.WithProvenance(fields.toList)
    }
  }

  private def unwrapChain[$: P]: P[BbinChainOp] = {
    import BaboonWhitespace.whitespace
    P(keyword("unwrap") ~ "(" ~ ")").map(_ => BbinChainOp.Unwrap)
  }

  private def leafExpr[$: P]: P[BbinExpr] = {
    import BaboonWhitespace.whitespace
    P(keyword("leaf") ~ "(" ~ bbinExpr ~ "," ~ "{" ~ structField.rep(sep = ",") ~ ",".? ~ "}" ~ ")").map {
      case (base, fields) => BbinExpr.Leaf(base, fields.toList)
    }
  }

  private def partialSealExpr[$: P]: P[BbinExpr] = {
    import BaboonWhitespace.whitespace
    P(keyword("partial") ~ "." ~ keyword("seal") ~ "(" ~ bbinExpr.? ~ ")").map {
      target => BbinExpr.PartialSeal(target)
    }
  }

  /** Struct construction: `Type { f = v, ... }` with `=` field delimiters
    * and no enclosing braces. The brace-wrapped `{Type{...}}` shape is a
    * set literal (parsed by `setExpr` with `:` row fields), never a struct.
    */
  private def structExpr[$: P]: P[BbinExpr] = {
    import BaboonWhitespace.whitespace
    P(idt.symbol ~ "{" ~ structField.rep(sep = ",") ~ ",".? ~ "}").map {
      case (name, fields) => BbinExpr.StructLit(BbinRow(RawTypeName(name), fields.toList))
    }
  }

  private def structField[$: P]: P[BbinField] = {
    import BaboonWhitespace.whitespace
    P(idt.symbol ~ "=" ~ fieldValue).map { case (name, value) => BbinField(name, value) }
  }

  private def identExpr[$: P]: P[BbinExpr] = {
    import BaboonWhitespace.whitespace
    P(idt.symbol).map(BbinExpr.Ident.apply)
  }

  private def litExpr[$: P]: P[BbinExpr] = {
    import BaboonWhitespace.whitespace
    P(
      bbinStr.map(BbinExpr.StrLit.apply) |
        floatLitRaw.map(raw => BbinExpr.FloatLit(raw.toDouble, raw): BbinExpr) |
        intLitRaw.map(raw => BbinExpr.IntLit(raw.toLong, raw): BbinExpr) |
        boolLit.map(BbinExpr.BoolLit.apply) |
        keyword("None").map(_ => BbinExpr.NoneLit: BbinExpr) |
        keyword("undefined").map(_ => BbinExpr.UndefinedLit: BbinExpr)
    )
  }

  private def schemaPathExpr[$: P]: P[BbinExpr] = {
    import BaboonWhitespace.whitespace
    P(schemaPathSegs).map(segments => BbinExpr.SchemaPath(BbinPath(segments)))
  }

  private def parenExpr[$: P]: P[BbinExpr] = {
    import BaboonWhitespace.whitespace
    P("(" ~ bbinExpr ~ ")")
  }

  def loaderExpr[$: P]: P[BbinExpr] = {
    import BaboonWhitespace.whitespace
    P(loaderCall ~ mapStep.rep()).map {
      case (call, maps) => BbinExpr.Loader(call.copy(maps = maps.toList))
    }
  }

  def setExpr[$: P]: P[BbinExpr] = {
    import BaboonWhitespace.whitespace
    // Keyed (`{k: Row{...}}`) and positional (`{Row{...}}`) entries are
    // separate alternatives so a mixed set parses as neither and fails.
    P("{" ~ (keyedEntry.rep(sep = ",", min = 1) | posEntry.rep(sep = ",", min = 1)) ~ ",".? ~ "}").map {
      entries => BbinExpr.SetLit(BbinSet(entries.toList))
    }
  }

  def relExpr[$: P]: P[BbinExpr] = {
    import BaboonWhitespace.whitespace
    P(relLit).map(BbinExpr.Rel.apply)
  }

  def pathExpr[$: P]: P[BbinExpr] = {
    import BaboonWhitespace.whitespace
    P(schemaPathSegs).map(segments => BbinExpr.SchemaPath(BbinPath(segments)))
  }

  private def loaderCall[$: P]: P[BbinLoaderCall] = {
    import BaboonWhitespace.whitespace
    P(
      (keyword("CsvLoader").map(_ => BbinLoaderKind.Csv: BbinLoaderKind) |
        keyword("JsonLoader").map(_ => BbinLoaderKind.Json: BbinLoaderKind)) ~/
        "(" ~ bbinType ~ "," ~ loaderSource ~ ")"
    ).map {
      case (kind, rowType, source) => BbinLoaderCall(kind, rowType, source, Nil)
    }
  }

  private def loaderSource[$: P]: P[String] = {
    import BaboonWhitespace.whitespace
    P(relLit | bbinStr)
  }

  private def mapStep[$: P]: P[BbinMapCall] = {
    import BaboonWhitespace.whitespace
    P("." ~ keyword("map") ~/ "(" ~ mapArg.rep(sep = ",", min = 1) ~ ",".? ~ ")").map {
      args => BbinMapCall(args.toList)
    }
  }

  private def mapArg[$: P]: P[BbinMapArg] = {
    import BaboonWhitespace.whitespace
    P(
      bbinStr.map(BbinMapArg.Str.apply) |
        schemaPathSegs.map(segments => BbinMapArg.Path(segments): BbinMapArg)
    )
  }

  private def keyedEntry[$: P]: P[BbinSetEntry] = {
    import BaboonWhitespace.whitespace
    P(setKey ~ ":" ~ rowLit).map { case (key, row) => BbinSetEntry(Some(key), row) }
  }

  private def posEntry[$: P]: P[BbinSetEntry] = {
    import BaboonWhitespace.whitespace
    P(rowLit).map(row => BbinSetEntry(None, row))
  }

  private def setKey[$: P]: P[String] = {
    import BaboonWhitespace.whitespace
    P(bbinStr | idt.symbol)
  }

  private def rowLit[$: P]: P[BbinRow] = {
    import BaboonWhitespace.whitespace
    P(idt.symbol ~ "{" ~ structField.rep(sep = ",") ~ ",".? ~ "}").map {
      case (name, fields) => BbinRow(RawTypeName(name), fields.toList)
    }
  }

  private def fieldValue[$: P]: P[BbinFieldValue] = {
    import BaboonWhitespace.whitespace
    P(
      bbinStr.map(BbinFieldValue.Str.apply) |
        boolLit.map(BbinFieldValue.Bool.apply) |
        floatLitRaw.map(raw => BbinFieldValue.Float(raw.toDouble, raw): BbinFieldValue) |
        intLitRaw.map(raw => BbinFieldValue.Int(raw.toLong, raw): BbinFieldValue) |
        keyword("None").map(_ => BbinFieldValue.NoneLit: BbinFieldValue) |
        keyword("undefined").map(_ => BbinFieldValue.Undefined: BbinFieldValue) |
        relLit.map(BbinFieldValue.Rel.apply) |
        schemaPathSegs.map(segments => BbinFieldValue.Path(segments): BbinFieldValue) |
        idt.symbol.map(BbinFieldValue.Ident.apply)
    )
  }

  private def schemaPathSegs[$: P]: P[List[BbinPathSeg]] = {
    import BaboonWhitespace.whitespace
    // A dotted method opener (`.map(`, `.filter(`, ...) starts a chain step,
    // never a path segment; a bare `.map` with no call paren stays a field.
    P(("." ~ !(chainMethodName ~ "(") ~ idt.symbol ~ ("[" ~ intIndex ~ "]").rep()).rep(1)).map {
      groups =>
        groups.toList.flatMap {
          case (field, idxs) => BbinPathSeg.Field(field) +: idxs.toList.map(BbinPathSeg.Index.apply)
        }
    }
  }

  private def chainMethodName[$: P]: P[Unit] = {
    import BaboonWhitespace.whitespace
    P(
      keyword("map") | keyword("filter") | keyword("cross") | keyword("innerJoin") | keyword("outerJoin") |
        keyword("leftJoin") | keyword("rightJoin") | keyword("zip") | keyword("concat") | keyword("reindex") |
        keyword("seal") | keyword("unseal") | keyword("with_provenance") | keyword("unwrap")
    )
  }

  private def assignTarget[$: P]: P[BbinPath] = {
    import BaboonWhitespace.whitespace
    P(
      ".".!.? ~ idt.symbol ~ (("." ~ idt.symbol).map(name => BbinPathSeg.Field(name): BbinPathSeg) |
        ("[" ~ intIndex ~ "]").map(idx => BbinPathSeg.Index(idx): BbinPathSeg)).rep()
    ).map {
      case (_, head, tail) => BbinPath(BbinPathSeg.Field(head) +: tail.toList)
    }
  }

  private def assignOp[$: P]: P[BbinAssignOp] = {
    import BaboonWhitespace.whitespace
    P(
      "+=".!.map(_ => BbinAssignOp.Add: BbinAssignOp) |
        "*=".!.map(_ => BbinAssignOp.Mul: BbinAssignOp) |
        "=".!.map(_ => BbinAssignOp.Set: BbinAssignOp)
    )
  }

  private def intIndex[$: P]: P[Int] = {
    import fastparse.NoWhitespace.noWhitespaceImplicit
    P(CharsWhileIn("0123456789").!).map(_.toInt)
  }

  private def intLitRaw[$: P]: P[String] = {
    import fastparse.NoWhitespace.noWhitespaceImplicit
    P(("-".? ~ CharsWhileIn("0123456789")).!)
  }

  private def floatLitRaw[$: P]: P[String] = {
    import fastparse.NoWhitespace.noWhitespaceImplicit
    P((floatDec | floatExp).!)
  }

  private def floatDec[$: P]: P[Unit] = {
    import fastparse.NoWhitespace.noWhitespaceImplicit
    P("-".? ~ CharsWhileIn("0123456789") ~ "." ~ CharsWhileIn("0123456789") ~ floatExpSuffix.?)
  }

  private def floatExp[$: P]: P[Unit] = {
    import fastparse.NoWhitespace.noWhitespaceImplicit
    P("-".? ~ CharsWhileIn("0123456789") ~ floatExpSuffix)
  }

  private def floatExpSuffix[$: P]: P[Unit] = {
    import fastparse.NoWhitespace.noWhitespaceImplicit
    P(("e" | "E") ~ ("+" | "-").? ~ CharsWhileIn("0123456789"))
  }

  private def boolLit[$: P]: P[Boolean] = {
    import fastparse.NoWhitespace.noWhitespaceImplicit
    P(keyword("true").map(_ => true) | keyword("false").map(_ => false))
  }

  def relLit[$: P]: P[String] = {
    import BaboonWhitespace.whitespace
    P(keyword("rel") ~/ ":" ~/ bbinStr)
  }

  /** Double-quoted (`"f"`, via the shared literal rule) or single-quoted
    * (`'f'`, the `.bbin` spelling used by `rel:'...'`) string content.
    */
  def bbinStr[$: P]: P[String] = {
    import fastparse.NoWhitespace.noWhitespaceImplicit
    P(Literals.Literals.SimpleStr | ("'" ~/ CharsWhile(_ != '\'', 0).! ~ "'"))
  }

  private def versionToken[$: P]: P[String] = {
    import fastparse.NoWhitespace.noWhitespaceImplicit
    P(bbinStr | CharsWhile(c => isLetter(c) | isDigit(c) | c == '.' | c == '_' | c == '-').!)
  }

  private def keyword[$: P](s: String): P[Unit] = {
    import fastparse.NoWhitespace.noWhitespaceImplicit
    P(s ~ !CharPred(c => isLetter(c) | isDigit(c) | c == '_'))
  }
}
