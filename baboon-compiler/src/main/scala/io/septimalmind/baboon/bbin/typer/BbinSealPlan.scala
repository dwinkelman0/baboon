package io.septimalmind.baboon.bbin.typer

import io.septimalmind.baboon.bbin.model.*
import io.septimalmind.baboon.parser.model.{RawTypeName, RawTypeRef}

/** A backend-runtime check emitted for a seal site.
  *
  * Seal planning never executes the default→migrate→normalize→validate
  * pipeline; it only decides which runtime nodes the backend must run.
  */
sealed trait BbinRuntimeCheck

object BbinRuntimeCheck {
  final case class CheckDynamicCompleteness(target: String) extends BbinRuntimeCheck
  final case class CheckLstContiguity(target: String) extends BbinRuntimeCheck
  final case class CheckProvenanceComplete(target: String) extends BbinRuntimeCheck
  final case class CheckPerElementErrors(target: String) extends BbinRuntimeCheck
}

/** One planned seal: a sealed binding or `.seal()` call with the errors
  * provable statically plus the checks the backend must run.
  */
final case class BbinSealSite(
  target: String,
  sealedType: String,
  staticErrors: List[String],
  runtimeChecks: List[BbinRuntimeCheck],
)

/** Phase 5 — seal planning (planning only, no execution; no IR lowering).
  *
  * See `plans/inputs-ir.md` phase 5 and `plans/inputs.md` (Partial
  * representation & sealing, Provenance in .bbin).
  *
  * A seal site is each `let x: T` with sealed `T` (every non-`~`
  * [[BbinType]]: `Plain`/`SetOf`/`IndexedSet`) and each `.seal()` chain
  * call that is not the direct RHS of a sealed `let` (that call is the
  * mechanism of the let's own seal and is covered by the let's site).
  *
  * Per site:
  *  - static: filled top-level slots are collected from [[BbinAssign]]
  *    targets whose root is the sealed name; required top-level field
  *    names missing from the filled set become `staticErrors` entries.
  *    The static missing-field check runs only when the site target is
  *    itself a sealed binding. Writes to `~T` partials that later seal
  *    (matching the partial's name through `unseal`/`let` chains) are
  *    OUT of scope: such tracing is left to a later phase, so a
  *    `.seal()` site rooted at a partial name skips the static check
  *    and relies on the runtime completeness check. Whole-row
  *    overwrites (`x = ...`) and struct-literal RHS fields likewise
  *    contribute no named slots — only multi-segment assign targets do.
  *  - runtime: every site always gets [[BbinRuntimeCheck.CheckDynamicCompleteness]]
  *    and [[BbinRuntimeCheck.CheckPerElementErrors]] (set-wide ops
  *    attempt every element and collect all errors; dynamic/loader rows
  *    can only be proven complete at runtime).
  *  - `lst` contiguity is unknowable without the `Domain` (whether a row
  *    type contains `lst` fields), so a heuristic is used instead: when
  *    any assigned path or indexed path for the target uses an integer
  *    index segment, [[BbinRuntimeCheck.CheckLstContiguity]] is emitted.
  *  - provenance: when any `WithProvenance` chain op, `with provenance`
  *    block, or `leaf` override mentions the target (by root name,
  *    best-effort) and the row type is in `provenanceRequired`,
  *    [[BbinRuntimeCheck.CheckProvenanceComplete]] is emitted. When the
  *    row type is provenance-required but provenance is statically
  *    known-incomplete (an empty `WithProvenance` fields list, an empty
  *    `with provenance` block, an argument-less `leaf`, or no mention
  *    at all), a `staticErrors` entry is recorded — incomplete
  *    provenance blocks seal. Row types outside `provenanceRequired`
  *    get no provenance check and no provenance errors.
  *
  * TODO (later phase): wire `requiredFields` to the real Domain
  * required-slot table instead of a caller-supplied map (same pattern
  * as [[BbinSlotCheck]] `knownTypes` and [[BbinLineage]] `captures`).
  */
object BbinSealPlan {

  def plan(
    scopeLets: List[(String, BbinType)],
    stmts: List[BbinStmt],
    requiredFields: Map[String, Set[String]],
    provenanceRequired: Set[String],
  ): (List[BbinSealSite], List[String]) = {
    val scope: Map[String, BbinType] = scopeLets.toMap
    val assigns: List[BbinAssign]    = stmts.flatMap(assignsInStmts)
    val allChains: List[BbinExpr.Chain] = stmts.flatMap(chainsInStmts)
    val withBlocks: List[BbinWith]     = stmts.flatMap(withsInStmts)
    val leaves: List[BbinExpr.Leaf]    = stmts.flatMap(leavesInStmts)

    val errors = Vector.newBuilder[String]
    val sites  = Vector.newBuilder[BbinSealSite]

    scopeLets.foreach { case (name, tpe) =>
      if (isSealed(tpe)) {
        val rows    = rowNamesOf(tpe)
        val filled  = filledSlots(assigns, name)
        val paths   = pathsForLet(stmts, assigns, name)
        val prov    = provFor(name, allChains, withBlocks, leaves)
        val indexed = paths.exists(_.exists(_.isInstanceOf[BbinPathSeg.Index]))
        sites += buildSite(
          target = name,
          sealedType = renderType(tpe),
          rows = rows,
          requiredFields = requiredFields,
          provenanceRequired = provenanceRequired,
          filled = filled,
          checkStaticMissing = true,
          usesIndex = indexed,
          prov = prov,
        )
      }
    }

    val directSeal = directSealChains(stmts)
    allChains
      .filter(c => c.ops.contains(BbinChainOp.Seal) && !directSeal.exists(_ eq c))
      .foreach { chain =>
        val target = chainRoot(chain.base).getOrElse("<expr>")
        val (sealedType, rows, filled, checkStatic) = scope.get(target) match {
          case Some(tpe) =>
            val sealedTpe = isSealed(tpe)
            (
              renderType(tpe),
              rowNamesOf(tpe),
              if (sealedTpe) filledSlots(assigns, target) else Set.empty[String],
              sealedTpe,
            )
          case None =>
            errors += s"seal of unknown target '$target': no binding in scope"
            ("<unknown>", Nil, Set.empty[String], false)
        }
        val paths =
          pathsInExpr(chain) ++ assigns
            .filter(a => rootName(a.target).contains(target))
            .flatMap(a => a.target.segments :: pathsInExpr(a.value))
        val prov    = provFor(target, allChains, withBlocks, leaves)
        val indexed = paths.exists(_.exists(_.isInstanceOf[BbinPathSeg.Index]))
        sites += buildSite(target, sealedType, rows, requiredFields, provenanceRequired, filled, checkStatic, indexed, prov)
      }

    (sites.result().toList, errors.result().toList)
  }

  private def buildSite(
    target: String,
    sealedType: String,
    rows: List[String],
    requiredFields: Map[String, Set[String]],
    provenanceRequired: Set[String],
    filled: Set[String],
    checkStaticMissing: Boolean,
    usesIndex: Boolean,
    prov: ProvInfo,
  ): BbinSealSite = {
    val required = rows.flatMap(r => requiredFields.getOrElse(r, Set.empty)).toSet
    val statics  = Vector.newBuilder[String]
    if (checkStaticMissing) {
      val missing = (required -- filled).toList.sorted
      if (missing.nonEmpty)
        statics +=
          s"seal '$target': missing required slots [${missing.mkString(", ")}] for row type '${rows.mkString(", ")}'"
    }
    val checks = Vector.newBuilder[BbinRuntimeCheck]
    checks += BbinRuntimeCheck.CheckDynamicCompleteness(target)
    if (usesIndex) checks += BbinRuntimeCheck.CheckLstContiguity(target)
    rows.find(provenanceRequired.contains).foreach { row =>
      if (prov.hasNonEmpty) checks += BbinRuntimeCheck.CheckProvenanceComplete(target)
      if (prov.hasEmpty)
        statics +=
          s"seal '$target': incomplete provenance blocks seal (empty provenance fields for row type '$row')"
      else if (!prov.mentioned)
        statics +=
          s"seal '$target': missing provenance for row type '$row': incomplete provenance blocks seal"
    }
    checks += BbinRuntimeCheck.CheckPerElementErrors(target)
    BbinSealSite(target, sealedType, statics.result().toList, checks.result().toList)
  }

  private def isSealed(tpe: BbinType): Boolean = tpe match {
    case BbinType.Partial(_) => false
    case _                   => true
  }

  private def renderType(tpe: BbinType): String = tpe match {
    case BbinType.Plain(ref)              => ref.render
    case BbinType.Partial(underlying)     => s"~${underlying.render}"
    case BbinType.SetOf(elem)             => s"{${elem.render}}"
    case BbinType.IndexedSet(idx, idxType, elem) =>
      s"{$idx: ${idxType.render} > ${elem.render}}"
  }

  /** Row-type names carried by a `let` type (the indexed-set scalar is not
    * a domain row and is excluded, mirroring [[BbinSlotCheck]]). */
  private def rowNamesOf(tpe: BbinType): List[String] = {
    val refs = tpe match {
      case BbinType.Plain(ref)               => List(ref)
      case BbinType.Partial(underlying)      => List(underlying)
      case BbinType.SetOf(elem)              => List(elem)
      case BbinType.IndexedSet(_, _, elem)   => List(elem)
    }
    refs.flatMap(refNames).distinct
  }

  /** All row-type names referenced by a [[RawTypeRef]], collected by walking
    * the (sealed) ref structure generically so every current and future
    * variant is covered without enumerating cases. */
  private def refNames(ref: RawTypeRef): List[String] = {
    def loop(v: Any): List[String] = v match {
      case tn: RawTypeName => List(tn.name)
      case p: Product      => p.productIterator.toList.flatMap(loop)
      case _               => Nil
    }
    loop(ref).distinct
  }

  private def rootName(target: BbinPath): Option[String] =
    target.segments.headOption.collect { case BbinPathSeg.Field(n) => n }

  /** Top-level filled slots for `target`: the second segment of every
    * assign target rooted at `target` (`x.a.b` fills `a`; `x[0]` and bare
    * `x` fill nothing statically). */
  private def filledSlots(assigns: List[BbinAssign], target: String): Set[String] =
    assigns.collect {
      case a if rootName(a.target).contains(target) =>
        a.target.segments.drop(1).headOption.collect { case BbinPathSeg.Field(n) => n }
    }.flatten.toSet

  /** Paths relevant to a sealed let's index heuristic: its RHS plus all
    * assign targets/values rooted at it. */
  private def pathsForLet(
    stmts: List[BbinStmt],
    assigns: List[BbinAssign],
    target: String,
  ): List[List[BbinPathSeg]] = {
    val rhs    = stmts.flatMap(letRhs(_, target))
    val mine   = assigns.filter(a => rootName(a.target).contains(target))
    rhs.flatMap(pathsInExpr) ++ mine.flatMap(a => a.target.segments :: pathsInExpr(a.value))
  }

  private def letRhs(stmt: BbinStmt, target: String): List[BbinExpr] = stmt match {
    case BbinStmt.LetStmt(l) if l.name == target => List(l.expr)
    case BbinStmt.LetStmt(_)                     => Nil
    case BbinStmt.AssignStmt(_)                   => Nil
    case BbinStmt.ForStmt(f)                      => f.body.flatMap(letRhs(_, target))
    case BbinStmt.WithStmt(w)                     => w.body.flatMap(letRhs(_, target))
  }

  /** Innermost identifier root of a chain base (`x.filter(..)` roots at
    * `x`); non-identifier bases (loaders, literals) have no root. */
  private def chainRoot(base: BbinExpr): Option[String] = base match {
    case BbinExpr.Ident(n)    => Some(n)
    case BbinExpr.Chain(b, _) => chainRoot(b)
    case _                    => None
  }

  /** `.seal()` chains that are the direct RHS of a sealed `let`: the
    * let's own site already covers them, so no separate site is built. */
  private def directSealChains(stmts: List[BbinStmt]): List[BbinExpr.Chain] =
    stmts.flatMap {
      case BbinStmt.LetStmt(l) =>
        val here = l.expr match {
          case c: BbinExpr.Chain if isSealed(l.tpe) && c.ops.contains(BbinChainOp.Seal) => List(c)
          case _ => Nil
        }
        here
      case BbinStmt.AssignStmt(_) => Nil
      case BbinStmt.ForStmt(f)    => directSealChains(f.body)
      case BbinStmt.WithStmt(w)   => directSealChains(w.body)
    }

  private final case class ProvInfo(mentioned: Boolean, hasNonEmpty: Boolean, hasEmpty: Boolean)

  /** Best-effort provenance mention for `target`: a `WithProvenance`
    * chain op whose base reads the target, a `with provenance` block
    * whose body assigns to (or reads) the target, or a `leaf` whose base
    * or override reads the target. */
  private def provFor(
    target: String,
    chains: List[BbinExpr.Chain],
    withBlocks: List[BbinWith],
    leaves: List[BbinExpr.Leaf],
  ): ProvInfo = {
    var mentioned = false
    var nonEmpty  = false
    var empty     = false
    chains.foreach { c =>
      if (c.ops.exists(_.isInstanceOf[BbinChainOp.WithProvenance]) && identsInExpr(c.base).contains(target)) {
        mentioned = true
        c.ops.foreach {
          case BbinChainOp.WithProvenance(fields) =>
            if (fields.isEmpty) empty = true else nonEmpty = true
          case _ => ()
        }
      }
    }
    withBlocks.foreach { w =>
      val bodyAssigns = w.body.flatMap(assignsInStmts)
      val touches =
        bodyAssigns.exists(a => rootName(a.target).contains(target)) ||
          identsInStmts(w.body).contains(target)
      if (touches) {
        mentioned = true
        if (w.provenance.isEmpty) empty = true else nonEmpty = true
      }
    }
    leaves.foreach { l =>
      val touches =
        identsInExpr(l.base).contains(target) ||
          l.overrides.exists {
            case BbinField(_, BbinFieldValue.Ident(n)) => n == target
            case _                                     => false
          }
      if (touches) {
        mentioned = true
        if (l.overrides.isEmpty) empty = true else nonEmpty = true
      }
    }
    ProvInfo(mentioned, nonEmpty, empty)
  }

  private def assignsInStmts(stmt: BbinStmt): List[BbinAssign] = stmt match {
    case BbinStmt.LetStmt(_)    => Nil
    case BbinStmt.AssignStmt(a) => List(a)
    case BbinStmt.ForStmt(f)    => f.body.flatMap(assignsInStmts)
    case BbinStmt.WithStmt(w)   => w.body.flatMap(assignsInStmts)
  }

  private def withsInStmts(stmt: BbinStmt): List[BbinWith] = stmt match {
    case BbinStmt.LetStmt(_)    => Nil
    case BbinStmt.AssignStmt(_) => Nil
    case BbinStmt.ForStmt(f)    => f.body.flatMap(withsInStmts)
    case BbinStmt.WithStmt(w)   => w :: w.body.flatMap(withsInStmts)
  }

  private def chainsInStmts(stmt: BbinStmt): List[BbinExpr.Chain] = stmt match {
    case BbinStmt.LetStmt(l)    => chainsInExpr(l.expr)
    case BbinStmt.AssignStmt(a) => chainsInExpr(a.value)
    case BbinStmt.ForStmt(f)    =>
      chainsInExpr(f.set) ++ f.body.flatMap(chainsInStmts)
    case BbinStmt.WithStmt(w)   => w.body.flatMap(chainsInStmts)
  }

  private def leavesInStmts(stmt: BbinStmt): List[BbinExpr.Leaf] = stmt match {
    case BbinStmt.LetStmt(l)    => leavesInExpr(l.expr)
    case BbinStmt.AssignStmt(a) => leavesInExpr(a.value)
    case BbinStmt.ForStmt(f)    =>
      leavesInExpr(f.set) ++ f.body.flatMap(leavesInStmts)
    case BbinStmt.WithStmt(w)   => w.body.flatMap(leavesInStmts)
  }

  private def chainsInExpr(expr: BbinExpr): List[BbinExpr.Chain] = expr match {
    case c: BbinExpr.Chain =>
      c :: (chainsInExpr(c.base) ++ c.ops.flatMap(chainsInOp))
    case BbinExpr.BinOp(_, l, r)      => chainsInExpr(l) ++ chainsInExpr(r)
    case BbinExpr.Leaf(base, _)       => chainsInExpr(base)
    case BbinExpr.PartialSeal(target) => target.map(chainsInExpr).getOrElse(Nil)
    case _                            => Nil
  }

  private def chainsInOp(op: BbinChainOp): List[BbinExpr.Chain] = op match {
    case BbinChainOp.Filter(pred) => chainsInExpr(pred)
    case BbinChainOp.Cross(other) => chainsInExpr(other)
    case BbinChainOp.InnerJoin(other) => chainsInExpr(other)
    case BbinChainOp.OuterJoin(other) => chainsInExpr(other)
    case BbinChainOp.LeftJoin(other) => chainsInExpr(other)
    case BbinChainOp.RightJoin(other) => chainsInExpr(other)
    case BbinChainOp.Zip(other)   => chainsInExpr(other)
    case BbinChainOp.Concat(other) => chainsInExpr(other)
    case BbinChainOp.Reindex(other) => chainsInExpr(other)
    case _                        => Nil
  }

  private def leavesInExpr(expr: BbinExpr): List[BbinExpr.Leaf] = expr match {
    case l: BbinExpr.Leaf =>
      l :: leavesInExpr(l.base)
    case BbinExpr.BinOp(_, l, r)      => leavesInExpr(l) ++ leavesInExpr(r)
    case BbinExpr.PartialSeal(target) => target.map(leavesInExpr).getOrElse(Nil)
    case c: BbinExpr.Chain =>
      leavesInExpr(c.base) ++ c.ops.flatMap(leavesInOp)
    case _ => Nil
  }

  private def leavesInOp(op: BbinChainOp): List[BbinExpr.Leaf] = op match {
    case BbinChainOp.Filter(pred) => leavesInExpr(pred)
    case BbinChainOp.Cross(other) => leavesInExpr(other)
    case BbinChainOp.InnerJoin(other) => leavesInExpr(other)
    case BbinChainOp.OuterJoin(other) => leavesInExpr(other)
    case BbinChainOp.LeftJoin(other) => leavesInExpr(other)
    case BbinChainOp.RightJoin(other) => leavesInExpr(other)
    case BbinChainOp.Zip(other)   => leavesInExpr(other)
    case BbinChainOp.Concat(other) => leavesInExpr(other)
    case BbinChainOp.Reindex(other) => leavesInExpr(other)
    case _                        => Nil
  }

  /** Best-effort identifier reads of an expression. Schema paths resolve
    * against row types, not `let`s, and contribute nothing; loader
    * column maps are declarations with no reads. */
  private def identsInExpr(expr: BbinExpr): Set[String] = expr match {
    case BbinExpr.Ident(name) => Set(name)
    case BbinExpr.BinOp(_, l, r) => identsInExpr(l) ++ identsInExpr(r)
    case BbinExpr.Leaf(base, overs) =>
      identsInExpr(base) ++ overs.flatMap(f => fieldIdent(f.value))
    case BbinExpr.StructLit(row) =>
      row.fields.flatMap(f => fieldIdent(f.value)).toSet
    case BbinExpr.SetLit(set) =>
      set.entries.flatMap(e => e.row.fields.flatMap(f => fieldIdent(f.value))).toSet
    case BbinExpr.PartialSeal(target) => target.map(identsInExpr).getOrElse(Set.empty)
    case BbinExpr.Chain(base, ops) =>
      identsInExpr(base) ++ ops.flatMap(identsInOp)
    case _ => Set.empty
  }

  private def identsInOp(op: BbinChainOp): Set[String] = op match {
    case BbinChainOp.Filter(pred) => identsInExpr(pred)
    case BbinChainOp.Cross(other) => identsInExpr(other)
    case BbinChainOp.InnerJoin(other) => identsInExpr(other)
    case BbinChainOp.OuterJoin(other) => identsInExpr(other)
    case BbinChainOp.LeftJoin(other) => identsInExpr(other)
    case BbinChainOp.RightJoin(other) => identsInExpr(other)
    case BbinChainOp.Zip(other)   => identsInExpr(other)
    case BbinChainOp.Concat(other) => identsInExpr(other)
    case BbinChainOp.Reindex(other) => identsInExpr(other)
    case BbinChainOp.WithProvenance(fields) =>
      fields.flatMap(f => fieldIdent(f.value)).toSet
    case BbinChainOp.Map(_) => Set.empty
    case _                  => Set.empty
  }

  private def fieldIdent(value: BbinFieldValue): Option[String] = value match {
    case BbinFieldValue.Ident(name) => Some(name)
    case _                          => None
  }

  private def identsInStmts(stmts: List[BbinStmt]): Set[String] = stmts.flatMap {
    case BbinStmt.LetStmt(l)    => identsInExpr(l.expr)
    case BbinStmt.AssignStmt(a) => identsInExpr(a.value)
    case BbinStmt.ForStmt(f)    => identsInExpr(f.set) ++ identsInStmts(f.body)
    case BbinStmt.WithStmt(w)   =>
      w.provenance.flatMap(f => fieldIdent(f.value)) ++ identsInStmts(w.body)
  }.toSet

  /** All indexable paths in an expression: schema paths plus `Path`
    * field/map values (loader column maps included). */
  private def pathsInExpr(expr: BbinExpr): List[List[BbinPathSeg]] = expr match {
    case BbinExpr.SchemaPath(path) => List(path.segments)
    case BbinExpr.BinOp(_, l, r)   => pathsInExpr(l) ++ pathsInExpr(r)
    case BbinExpr.Leaf(base, overs) =>
      pathsInExpr(base) ++ overs.collect { case BbinField(_, BbinFieldValue.Path(s)) => s }
    case BbinExpr.StructLit(row) =>
      row.fields.collect { case BbinField(_, BbinFieldValue.Path(s)) => s }
    case BbinExpr.SetLit(_) => Nil
    case BbinExpr.PartialSeal(target) => target.map(pathsInExpr).getOrElse(Nil)
    case BbinExpr.Chain(base, ops) =>
      pathsInExpr(base) ++ ops.flatMap(pathsInOp)
    case _ => Nil
  }

  private def pathsInOp(op: BbinChainOp): List[List[BbinPathSeg]] = op match {
    case BbinChainOp.Map(args) =>
      args.collect { case BbinMapArg.Path(s) => s }
    case BbinChainOp.Filter(pred) => pathsInExpr(pred)
    case BbinChainOp.Cross(other) => pathsInExpr(other)
    case BbinChainOp.InnerJoin(other) => pathsInExpr(other)
    case BbinChainOp.OuterJoin(other) => pathsInExpr(other)
    case BbinChainOp.LeftJoin(other) => pathsInExpr(other)
    case BbinChainOp.RightJoin(other) => pathsInExpr(other)
    case BbinChainOp.Zip(other)   => pathsInExpr(other)
    case BbinChainOp.Concat(other) => pathsInExpr(other)
    case BbinChainOp.Reindex(other) => pathsInExpr(other)
    case BbinChainOp.WithProvenance(fields) =>
      fields.collect { case BbinField(_, BbinFieldValue.Path(s)) => s }
    case _ => Nil
  }
}
