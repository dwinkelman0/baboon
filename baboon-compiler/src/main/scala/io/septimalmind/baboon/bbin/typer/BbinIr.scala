package io.septimalmind.baboon.bbin.typer

import io.septimalmind.baboon.bbin.model.*
import io.septimalmind.baboon.bbin.parser.BbinImportResolver.ResolvedImport
import io.septimalmind.baboon.parser.model.{RawTypeName, RawTypeRef}

/** Backend-neutral `.bbin` IR (phase 6, see `plans/inputs-ir.md`).
  *
  * Nodes record, they never execute: set derivations reference source set
  * bindings by name (loops are never unrolled), rows carry statically
  * filled slots with best-effort provenance ([[IRSlot]]), expression
  * payloads are classified Eager/Deferred ([[IRExpr]]), and statements
  * mirror [[RawBbin]] shapes with binder types resolved from the phase-2
  * [[Scope]]. Per-backend lowering
  * (Rust first) consumes [[IRProgram]] and stays a stub in
  * `BbinLowering`.
  */
sealed trait IROp

object IROp {
  final case class Lit(entries: Int) extends IROp
  final case class LoaderRef(name: String) extends IROp
  final case class Cross(of: String) extends IROp
  final case class Zip(of: String) extends IROp
  final case class Concat(of: String) extends IROp
  final case class Join(of: String) extends IROp
  final case class Reindex(of: String) extends IROp
  final case class Filter(of: String) extends IROp
}

/** Evaluation-strategy classification of an IR expression payload.
  *
  * [[IRExpr.Eager]] wraps a [[BbinExpr]] free of schema-path references —
  * its value is fully determined at assembly time. [[IRExpr.Deferred]]
  * wraps an expression that transitively references a schema path
  * ([[BbinExpr.SchemaPath]], or a `StructLit`/`Leaf` field value that is a
  * [[BbinFieldValue.Path]]) — its value depends on schema resolution and
  * can only be settled by lowering. The wrapped payload stays a
  * [[BbinExpr]]: the classification is IR-native metadata, not a redesign
  * of the expression language. */
sealed trait IRExpr {

  /** The classified payload expression. */
  def expr: BbinExpr
}

object IRExpr {
  final case class Eager(expr: BbinExpr) extends IRExpr
  final case class Deferred(expr: BbinExpr) extends IRExpr

  /** Classifies by structural SchemaPath presence: [[Deferred]] when
    * `expr` transitively contains [[BbinExpr.SchemaPath]] (through `BinOp`
    * args, `Chain` base/ops, `Leaf` base/overrides, or `StructLit` field
    * `Path` values), [[Eager]] otherwise. */
  def classify(expr: BbinExpr): IRExpr =
    if (hasSchemaPath(expr)) Deferred(expr) else Eager(expr)

  /** Structural walk mirroring the `refNames` helper below: descends every
    * product node, so new [[BbinExpr]]/[[BbinChainOp]]/[[BbinFieldValue]]
    * variants are covered without walk edits. */
  private def hasSchemaPath(v: Any): Boolean = v match {
    case _: BbinExpr.SchemaPath => true
    case _: BbinFieldValue.Path => true
    case p: Product             => p.productIterator.exists(hasSchemaPath)
    case _                      => false
  }
}

/** A named set binding: `idx` is `None` for anonymous `{T}` sets and
  * `Some((index, idxType))` for `{idx: T > E}` sets; `row` is the element
  * row name; `partial` marks loader sets over `~T` rows; `op` records how
  * the set derives (by name, never unrolled). */
final case class IRSet(
  name: String,
  idx: Option[(String, String)],
  row: String,
  partial: Boolean,
  op: IROp,
)

/** One statically known slot of a row literal occurrence.
  *
  * `provenance` is the best-effort IR-level tag: `Some("ambient")` for
  * slots assigned inside a `WithStmt` body, `Some("explicit")` for slots
  * coming from `Leaf` overrides or `WithProvenance` chain results, and
  * `None` otherwise.
  *
  * TODO: full `Provenance` struct materialization (source chain, stamps,
  * per-field records) stays backend lowering work — this tag classifies
  * how a slot acquired provenance, it does not carry the provenance
  * payload itself.
  */
final case class IRSlot(path: String, provenance: Option[String])

/** A flat row shape: `slots` are the statically filled slots of one
  * row literal occurrence, each with its best-effort provenance. */
final case class IRRow(name: String, slots: List[IRSlot])

sealed trait IRStmt

/** Broadcast/per-row write; `targetType` is the scope type of the target
  * root; `value` is classified [[IRExpr.Eager]]/[[IRExpr.Deferred]] at
  * assembly time. */
final case class IRAssign(
  target: BbinPath,
  targetType: Option[BbinType],
  op: BbinAssignOp,
  value: IRExpr,
) extends IRStmt

/** `for ((idx, row) in set) { body }` with binder types resolved from scope. */
final case class IRFor(
  idx: String,
  idxType: BbinType,
  row: String,
  rowType: BbinType,
  set: IRExpr,
  body: List[IRStmt],
) extends IRStmt

final case class IRWithProv(provenance: List[BbinField], body: List[IRStmt]) extends IRStmt

final case class IRSeal(target: String, sealedType: String) extends IRStmt

final case class IRUnseal(target: String, unsealedType: String) extends IRStmt

/** `leaf(base, {...})`: `base` is the root binding name, `overrides` the
  * override field names. */
final case class IRLeaf(base: String, overrides: List[String]) extends IRStmt

/** A join site: `kind` is one of `inner|outer|left|right|cross`. */
final case class IRJoin(kind: String, left: String, right: String) extends IRStmt

final case class IRProgram(
  sets: List[IRSet],
  rows: List[IRRow],
  stmts: List[IRStmt],
  seals: List[BbinSealSite],
  lineage: BbinLineage,
)

/** Phase 6 — end-to-end AST-to-IR assembly.
  *
  * Runs scope → slotcheck → lineage (+placement+sealPin) → sealplan in
  * order, threading the phase-2 [[Scope]] (never raw annotations) and the
  * phase-4 [[BbinLineage]] into IR construction. Errors from every phase
  * are collected; assembly is best-effort — IR is built for whatever
  * passed and `None` is returned only when scope itself failed (without a
  * scope no later phase can run).
  */
object BbinIr {

  def assemble(
    raw: RawBbin,
    resolvedImports: List[ResolvedImport],
    knownTypes: Set[String],
    requiredFields: Map[String, Set[String]],
    provenanceRequired: Set[String],
    captures: Map[String, String],
  ): (Option[IRProgram], List[String]) = {
    val (scope, scopeErrors) = BbinScopeBuilder.build(raw, resolvedImports)
    if (scopeErrors.nonEmpty) {
      (None, scopeErrors)
    } else {
      val slotErrors =
        raw.statements.flatMap(stmt => BbinSlotCheck.check(scope, knownTypes, stmt))
      val lineage   = BbinLineage.analyze(raw.statements)
      val scopeLets = scopeLetsInOrder(raw, resolvedImports)
      val placement = BbinLineage.checkPlacement(scopeLets, captures)
      val sealPin   = BbinLineage.checkSealPin(raw.usings)
      val (sites, sealErrors) =
        BbinSealPlan.plan(scopeLets, raw.statements, requiredFields, provenanceRequired)
      val errors = slotErrors ++ placement ++ sealPin ++ sealErrors
      val program = IRProgram(
        sets = collectSets(raw, scope),
        rows = collectRows(raw),
        stmts = buildStmts(raw.statements, scope),
        seals = sites,
        lineage = lineage,
      )
      (Some(program), errors)
    }
  }

  private val unknownType: BbinType =
    BbinType.Plain(RawTypeRef.Simple(RawTypeName("unknown"), Nil))

  /** Lets in program order (imports first, then every `let` transitively):
    * the order placement and seal planning consume. */
  private def scopeLetsInOrder(
    raw: RawBbin,
    resolvedImports: List[ResolvedImport],
  ): List[(String, BbinType)] = {
    val imported = resolvedImports.map(ri => ri.alias.getOrElse(ri.name) -> ri.resolvedType)
    imported ++ letsInStmts(raw.statements)
  }

  private def letsInStmts(stmts: List[BbinStmt]): List[(String, BbinType)] =
    stmts.flatMap {
      case BbinStmt.LetStmt(l)    => List(l.name -> l.tpe)
      case BbinStmt.AssignStmt(_) => Nil
      case BbinStmt.ForStmt(f)    => letsInStmts(f.body)
      case BbinStmt.WithStmt(w)   => letsInStmts(w.body)
    }

  private def letExprs(stmts: List[BbinStmt]): Map[String, BbinExpr] =
    stmts.flatMap {
      case BbinStmt.LetStmt(l)    => List(l.name -> l.expr)
      case BbinStmt.AssignStmt(_) => Nil
      case BbinStmt.ForStmt(f)    => letExprs(f.body).toList
      case BbinStmt.WithStmt(w)   => letExprs(w.body).toList
    }.toMap

  private def collectSets(raw: RawBbin, scope: Scope): List[IRSet] = {
    val exprs = letExprs(raw.statements)
    letsInStmts(raw.statements).flatMap {
      case (name, BbinType.SetOf(elem)) =>
        val expr = exprs.getOrElse(name, BbinExpr.SetLit(BbinSet(Nil)))
        Some(IRSet(name, None, rowNameOf(elem), isPartialLoader(expr), setOp(expr)))
      case (name, BbinType.IndexedSet(idx, idxType, elem)) =>
        val expr = exprs.getOrElse(name, BbinExpr.SetLit(BbinSet(Nil)))
        Some(
          IRSet(name, Some((idx, idxType.render)), rowNameOf(elem), isPartialLoader(expr), setOp(expr)),
        )
      case _ =>
        Nil
    }
  }

  private def isPartialLoader(expr: BbinExpr): Boolean = expr match {
    case BbinExpr.Loader(call) => call.rowType.isInstanceOf[BbinType.Partial]
    case _                     => false
  }

  /** Derives the set op from the RHS, referencing source bindings by name.
    * A bare `Ident` RHS is an identity reference (`Reindex`); a chain
    * with no set-combining op falls back to the base binding (`Reindex`)
    * or `Lit(0)` when the base is anonymous. */
  private def setOp(expr: BbinExpr): IROp = expr match {
    case BbinExpr.SetLit(set)  => IROp.Lit(set.entries.size)
    case BbinExpr.Loader(call) => IROp.LoaderRef(call.source)
    case BbinExpr.Ident(n)     => IROp.Reindex(n)
    case c: BbinExpr.Chain     => chainOp(c)
    case _                     => IROp.Lit(0)
  }

  private def chainOp(chain: BbinExpr.Chain): IROp = {
    val base = rootOf(chain.base)
    chain.ops.collectFirst {
      case BbinChainOp.Cross(other)      => IROp.Cross(rootOf(other))
      case BbinChainOp.Zip(other)        => IROp.Zip(rootOf(other))
      case BbinChainOp.Concat(other)     => IROp.Concat(rootOf(other))
      case BbinChainOp.InnerJoin(other)  => IROp.Join(rootOf(other))
      case BbinChainOp.OuterJoin(other)  => IROp.Join(rootOf(other))
      case BbinChainOp.LeftJoin(other)   => IROp.Join(rootOf(other))
      case BbinChainOp.RightJoin(other)  => IROp.Join(rootOf(other))
      case BbinChainOp.Reindex(other)    => IROp.Reindex(rootOf(other))
      case BbinChainOp.Filter(_)         => IROp.Filter(base)
    }.getOrElse {
      if (base == "<expr>") IROp.Lit(0) else IROp.Reindex(base)
    }
  }

  /** Provenance tags recorded on [[IRSlot]]s: `"ambient"` inside `WithStmt`
    * bodies, `"explicit"` for `Leaf` overrides and `WithProvenance` chain
    * results. */
  private val ambientProv: String  = "ambient"
  private val explicitProv: String = "explicit"

  private def collectRows(raw: RawBbin): List[IRRow] =
    rowsInStmts(raw.statements, ctx = None).distinct

  /** `ctx` is the enclosing provenance classification: `Some(explicitProv)`
    * inside a `WithProvenance` chain (explicit wins over ambient),
    * `Some(ambientProv)` inside a `WithStmt` body, `None` elsewhere. */
  private def rowsInStmts(stmts: List[BbinStmt], ctx: Option[String]): List[IRRow] =
    stmts.flatMap {
      case BbinStmt.LetStmt(l)    => rowsInExpr(l.expr, ctx)
      case BbinStmt.AssignStmt(a) => rowsInExpr(a.value, ctx)
      case BbinStmt.ForStmt(f)    => rowsInExpr(f.set, ctx) ++ rowsInStmts(f.body, ctx)
      case BbinStmt.WithStmt(w)   => rowsInStmts(w.body, refine(ctx, ambientProv))
    }

  private def rowsInExpr(expr: BbinExpr, ctx: Option[String]): List[IRRow] = expr match {
    case BbinExpr.SetLit(set)    => set.entries.map(entry => irRow(entry.row, ctx))
    case BbinExpr.StructLit(row) => List(irRow(row, ctx))
    case BbinExpr.BinOp(_, l, r) => rowsInExpr(l, ctx) ++ rowsInExpr(r, ctx)
    case BbinExpr.Leaf(base, overrides) =>
      val overrideSlots = overrides.map(o => IRSlot(o.name, Some(explicitProv)))
      rowsInExpr(base, ctx).map(row => IRRow(row.name, row.slots ++ overrideSlots))
    case BbinExpr.PartialSeal(t) => t.map(rowsInExpr(_, ctx)).getOrElse(Nil)
    case BbinExpr.Chain(base, ops) =>
      val chainCtx =
        if (ops.exists(_.isInstanceOf[BbinChainOp.WithProvenance])) Some(explicitProv) else ctx
      rowsInExpr(base, chainCtx) ++ ops.flatMap(op => rowsInOp(op, chainCtx))
    case _ => Nil
  }

  private def rowsInOp(op: BbinChainOp, ctx: Option[String]): List[IRRow] = op match {
    case BbinChainOp.Filter(pred)     => rowsInExpr(pred, ctx)
    case BbinChainOp.Cross(other)     => rowsInExpr(other, ctx)
    case BbinChainOp.InnerJoin(other) => rowsInExpr(other, ctx)
    case BbinChainOp.OuterJoin(other) => rowsInExpr(other, ctx)
    case BbinChainOp.LeftJoin(other)  => rowsInExpr(other, ctx)
    case BbinChainOp.RightJoin(other) => rowsInExpr(other, ctx)
    case BbinChainOp.Zip(other)       => rowsInExpr(other, ctx)
    case BbinChainOp.Concat(other)    => rowsInExpr(other, ctx)
    case BbinChainOp.Reindex(other)   => rowsInExpr(other, ctx)
    case _                            => Nil
  }

  /** An explicit classification beats an ambient one; ambient applies only
    * where no more specific tag is already in force. */
  private def refine(ctx: Option[String], tag: String): Option[String] =
    if (ctx.contains(explicitProv)) ctx else Some(tag)

  private def irRow(row: BbinRow, ctx: Option[String]): IRRow =
    IRRow(row.typeName.name, row.fields.map(f => IRSlot(f.name, ctx)))

  private def buildStmts(stmts: List[BbinStmt], scope: Scope): List[IRStmt] =
    stmts.flatMap(stmt => buildStmt(stmt, scope))

  private def buildStmt(stmt: BbinStmt, scope: Scope): List[IRStmt] = stmt match {
    case BbinStmt.LetStmt(l) =>
      sealNodes(l.name, scopeTypeString(scope, l.name, l.tpe), l.expr)
    case BbinStmt.AssignStmt(a) =>
      val root       = rootName(a.target)
      val targetType = root.flatMap(scope.typeOf)
      IRAssign(a.target, targetType, a.op, IRExpr.classify(a.value)) :: sealNodes(
        root.getOrElse("<expr>"),
        root.flatMap(scope.typeOf).map(renderBbinType).getOrElse("<unknown>"),
        a.value,
      )
    case BbinStmt.ForStmt(f) =>
      val idxT = scope.typeOf(f.idx).getOrElse(unknownType)
      val rowT = scope.typeOf(f.row).getOrElse(unknownType)
      List(IRFor(f.idx, idxT, f.row, rowT, IRExpr.classify(f.set), buildStmts(f.body, scope)))
    case BbinStmt.WithStmt(w) =>
      List(IRWithProv(w.provenance, buildStmts(w.body, scope)))
  }

  /** Seal/leaf/join nodes recorded for an RHS expression: `leaf(...)`
    * becomes [[IRLeaf]], `.seal()`/`.unseal()` become [[IRSeal]]/[[IRUnseal]],
    * and cross/join ops become [[IRJoin]] alongside. */
  private def sealNodes(name: String, tpe: String, expr: BbinExpr): List[IRStmt] = expr match {
    case leaf: BbinExpr.Leaf =>
      List(IRLeaf(rootOf(leaf.base), leaf.overrides.map(_.name)))
    case chain: BbinExpr.Chain =>
      joinsInChain(chain) ++
        (if (chain.ops.contains(BbinChainOp.Seal)) List(IRSeal(name, tpe)) else Nil) ++
        (if (chain.ops.contains(BbinChainOp.Unseal)) List(IRUnseal(name, tpe)) else Nil)
    case _ =>
      Nil
  }

  private def joinsInChain(chain: BbinExpr.Chain): List[IRStmt] = {
    val left = rootOf(chain.base)
    chain.ops.collect {
      case BbinChainOp.Cross(other)     => IRJoin("cross", left, rootOf(other))
      case BbinChainOp.InnerJoin(other) => IRJoin("inner", left, rootOf(other))
      case BbinChainOp.OuterJoin(other) => IRJoin("outer", left, rootOf(other))
      case BbinChainOp.LeftJoin(other)  => IRJoin("left", left, rootOf(other))
      case BbinChainOp.RightJoin(other) => IRJoin("right", left, rootOf(other))
    }
  }

  private def scopeTypeString(scope: Scope, name: String, fallback: BbinType): String =
    renderBbinType(scope.typeOf(name).getOrElse(fallback))

  private def renderBbinType(tpe: BbinType): String = tpe match {
    case BbinType.Plain(ref)            => ref.render
    case BbinType.Partial(underlying)   => s"~${underlying.render}"
    case BbinType.SetOf(elem)           => s"{${elem.render}}"
    case BbinType.IndexedSet(idx, idxType, elem) =>
      s"{$idx: ${idxType.render} > ${elem.render}}"
  }

  private def rootName(target: BbinPath): Option[String] =
    target.segments.headOption.collect { case BbinPathSeg.Field(n) => n }

  private def rootOf(expr: BbinExpr): String = expr match {
    case BbinExpr.Ident(n)    => n
    case BbinExpr.Chain(b, _) => rootOf(b)
    case _                    => "<expr>"
  }

  private def rowNameOf(ref: RawTypeRef): String =
    refNames(ref).headOption.getOrElse(ref.render)

  private def refNames(ref: RawTypeRef): List[String] = {
    def loop(v: Any): List[String] = v match {
      case tn: RawTypeName => List(tn.name)
      case p: Product      => p.productIterator.toList.flatMap(loop)
      case _               => Nil
    }
    loop(ref).distinct
  }
}
