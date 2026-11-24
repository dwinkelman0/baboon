package io.septimalmind.baboon.bbin.typer

import io.septimalmind.baboon.bbin.model.*
import io.septimalmind.baboon.parser.model.{RawTypeName, RawTypeRef}

/** One contributing slot of a derived slot-path, with its static weight.
  *
  * `weight = Some(1.0)` when the contribution is statically determinable
  * here: a direct copy (`let b = a`) or leaf assignment (`b = a`) outside
  * any loop. `weight = None` when the weight is runtime-determined — the
  * value flows through a loop (rows aggregated), a `filter` (rows
  * dropped), a join or `cross` (rows multiplied) — or when the derivation
  * mixes several sources (arithmetic, leaf overrides, set literals) so no
  * single source owns the whole value.
  */
final case class BbinContrib(slot: String, weight: Option[Double])

/** How an outer binding became derived without binding a new name.
  *
  * `fn` is the loop descriptor (e.g. `"for((i, r) in users)"`); `summary`
  * describes the opaqueness (e.g. `"opaque loop, no unrolling"` — loops
  * are never unrolled, so per-row contributions collapse).
  */
final case class BbinDerivedVia(fn: String, summary: String)

/** A single derived binding's lineage: data deps plus control deps.
  *
  * Phase 4 output (see `plans/inputs-ir.md`): def-use over `let`/`assign`
  * plus loop collapsing. `deps` maps each bound name to the prior
  * `let`/`import`/`for`-binder names its right-hand side reads (`Ident`
  * occurrences; assign-target roots are writes and excluded; schema paths
  * contribute nothing — they resolve against row types, not `let`s).
  * `controlDeps` maps each bound name to the names read by the `Filter`
  * predicates and `for` set-exprs that gate it. `derivedVia` records
  * loop effects that do not bind a name: an outer binding assigned inside
  * a `for` body maps to a [[BbinDerivedVia]] whose `fn` is the loop
  * descriptor (e.g. `"for((i, r) in users)"`); loops are never unrolled.
  * `contribs` maps each derived slot-path (a `let` name, or an assign
  * target rendered e.g. `acc.voltage`) to the contributing slot-paths it
  * reads, sorted, each with its static [[BbinContrib]] weight.
  */
final case class BbinLineage(
  deps: Map[String, Set[String]],
  controlDeps: Map[String, Set[String]],
  derivedVia: Map[String, BbinDerivedVia],
  contribs: Map[String, List[BbinContrib]],
)

/** Phase 4 — lineage + placement (AST-level, no IR lowering).
  *
  * `analyze` is purely structural: it never consults a `Domain` (checking
  * `SchemaPath` segments against the `using` pin is later wiring) and it
  * only sees the analyzed statements, so `import` bindings are not threaded
  * — a read of an import name appears in `deps` only if that name is also
  * bound by the analyzed statements.
  *
  * `checkPlacement` enforces the parent-capture rule
  * (`plans/inputs.md`: a capturing type instantiates only within its unique
  * lineage; below-lineage instantiation is a compile error) approximately at
  * AST level: constructing type `C` with `captures(C) = A` requires a
  * `let`/`import`/`for`-binder whose row-name is `A` earlier in program
  * order. Sealing to a non-capturing type is always OK. Seal-version
  * pinning (seal version = header `using` pin, `plans/inputs.md`):
  * [[BbinLineage.checkSealPin]] enforces the AST-level half — exactly one
  * `using` pin per file; binding that pin to actual seal artifacts stays
  * in seal planning (phase 5).
  *
  * TODO (later phase): wire `captures` to the real Domain parent-capture
  * table instead of a caller-supplied map (same pattern as
  * [[BbinSlotCheck]] `knownTypes`).
  */
object BbinLineage {

  def analyze(stmts: List[BbinStmt]): BbinLineage = {
    var deps     = Map.empty[String, Set[String]]
    var control  = Map.empty[String, Set[String]]
    var derived  = Map.empty[String, BbinDerivedVia]
    var contribs = Map.empty[String, List[BbinContrib]]
    var defined  = Set.empty[String]

    def ensure(name: String): Unit = {
      if (!deps.contains(name)) deps = deps.updated(name, Set.empty)
      if (!control.contains(name)) control = control.updated(name, Set.empty)
      if (!contribs.contains(name)) contribs = contribs.updated(name, Nil)
    }

    /** Keep only reads of already-bound names (prior `let`/`for`-binder
      * names); forward references, unknowns and implicits (`self`, `it`)
      * never enter the maps. */
    def prior(reads: Set[String], self: String): Set[String] =
      reads.intersect(defined) - self

    def addDep(name: String, reads: Set[String]): Unit = {
      ensure(name)
      deps = deps.updated(name, deps(name) ++ prior(reads, name))
    }

    def addControl(name: String, reads: Set[String]): Unit = {
      ensure(name)
      control = control.updated(name, control(name) ++ prior(reads, name))
    }

    /** Appends one [[BbinContrib]] per source read to `slotPath`'s list.
      * Sources are sorted for deterministic output; repeated derivations of
      * the same slot-path accumulate (each derivation event contributes).
      * Does not `ensure`: a nested assign target (`acc.v`) is a slot-path,
      * not a bound name, and must not leak into `deps`/`controlDeps`. */
    def addContribs(slotPath: String, reads: Set[String], weight: Option[Double]): Unit =
      contribs = contribs.updated(
        slotPath,
        contribs.getOrElse(slotPath, Nil) ++ reads.toList.sorted.map(BbinContrib(_, weight)),
      )

    /** Statically determinable weight: `Some(1.0)` only for a direct copy
      * / leaf assignment (an `Ident` value) outside any loop; through
      * loops/filters/joins/cross and mixed-source expressions the weight
      * is runtime-determined (see [[BbinContrib]]). */
    def staticWeight(value: BbinExpr, inLoop: Boolean): Option[Double] =
      if (!inLoop && value.isInstanceOf[BbinExpr.Ident]) Some(1.0) else None

    /** Every name bound by these statements, transitively (lets plus `for`
      * binders, through nested loops and `with` blocks). */
    def boundNames(stmts: List[BbinStmt]): Set[String] =
      stmts.flatMap {
        case BbinStmt.LetStmt(l)    => List(l.name)
        case BbinStmt.AssignStmt(_) => Nil
        case BbinStmt.ForStmt(f)    => List(f.idx, f.row) ++ boundNames(f.body)
        case BbinStmt.WithStmt(w)   => boundNames(w.body)
      }.toSet

    /** Assign-target roots at this loop level: direct assigns plus assigns
      * inside `with` blocks (which create no loop of their own). Assigns
      * inside a nested `for` belong to that inner loop and are excluded
      * (no unrolling, no double attribution). */
    def loopLevelTargets(stmts: List[BbinStmt]): List[String] =
      stmts.flatMap {
        case BbinStmt.AssignStmt(a) => rootName(a.target).toList
        case BbinStmt.WithStmt(w)   => loopLevelTargets(w.body)
        case _                      => Nil
      }

    def renderSet(set: BbinExpr): String = set match {
      case BbinExpr.Ident(n) => n
      case _                 => "<expr>"
    }

    def walkStmts(stmts: List[BbinStmt], inLoop: Boolean): Unit = stmts.foreach {
      case BbinStmt.LetStmt(l) =>
        addDep(l.name, identsInExpr(l.expr))
        addControl(l.name, filterPredIdents(l.expr))
        addContribs(l.name, prior(identsInExpr(l.expr), l.name), staticWeight(l.expr, inLoop))
        defined = defined + l.name
      case BbinStmt.AssignStmt(a) =>
        // The target root is a write, not a read; the value's reads accrue
        // to the target root's data deps (def-use across lets).
        rootName(a.target).foreach { r =>
          val reads = identsInExpr(a.value) - r
          addDep(r, reads)
          addControl(r, filterPredIdents(a.value))
          addContribs(renderPath(a.target), prior(reads, r), staticWeight(a.value, inLoop))
        }
      case BbinStmt.ForStmt(f) =>
        val setReads = identsInExpr(f.set).intersect(defined)
        ensure(f.idx)
        ensure(f.row)
        deps = deps.updated(f.idx, deps(f.idx) ++ setReads)
        deps = deps.updated(f.row, deps(f.row) ++ setReads)
        // Binders read the set through iteration: which rows each sees is
        // runtime-determined, so their contribution weight is never static.
        addContribs(f.idx, setReads, None)
        addContribs(f.row, setReads, None)
        defined = defined + f.idx + f.row
        // The set-expr gates everything bound inside the body.
        boundNames(f.body).foreach { n =>
          ensure(n)
          control = control.updated(n, control(n) ++ (setReads - n))
        }
        walkStmts(f.body, inLoop = true)
        val fn    = s"for((${f.idx}, ${f.row}) in ${renderSet(f.set)})"
        val via   = BbinDerivedVia(fn, "opaque loop, no unrolling")
        val inner = boundNames(f.body)
        loopLevelTargets(f.body).foreach { r =>
          if (!inner.contains(r)) derived = derived.updated(r, via)
        }
      case BbinStmt.WithStmt(w) =>
        walkStmts(w.body, inLoop)
    }

    walkStmts(stmts, inLoop = false)
    BbinLineage(deps, control, derived, contribs)
  }

  /** Placement check: for each `(name, tpe)` in program order, every row
    * name of `tpe` that appears in `captures` requires the mapped ancestor
    * row-name to have been bound earlier. Returns one
    * `"below-lineage instantiation"` error per violation. */
  def checkPlacement(scopeLets: List[(String, BbinType)], captures: Map[String, String]): List[String] = {
    val errors = Vector.newBuilder[String]
    var seen   = Set.empty[String]
    scopeLets.foreach { case (name, tpe) =>
      rowNamesOf(tpe).foreach { c =>
        captures.get(c).foreach { ancestor =>
          if (!seen.contains(ancestor))
            errors +=
              s"below-lineage instantiation of '$c' in '$name': requires ancestor '$ancestor' bound earlier in program order"
        }
      }
      seen = seen ++ rowNamesOf(tpe)
    }
    errors.result().toList
  }

  /** AST-level enforceable part of "seal version = header `using` pin"
    * (`plans/inputs.md`; `plans/inputs-ir.md` phase 4): a file must carry
    * exactly one `using` pin for its seals to have a version. Zero pins
    * leaves the seal versionless; more than one pin means multiple
    * workspaces, which is not yet supported. Resolving the surviving pin
    * to actual seal artifacts stays in seal planning (phase 5). */
  def checkSealPin(usings: List[BbinUsing]): List[String] = usings match {
    case Nil      => List("no using pin: seal has no version")
    case _ :: Nil => Nil
    case _        => List("multi-workspace not yet supported")
  }

  private def rootName(target: BbinPath): Option[String] =
    target.segments.headOption.collect { case BbinPathSeg.Field(n) => n }

  /** Renders an assign target as a slot-path for [[BbinLineage.contribs]]:
    * fields joined by `.`, indices rendered `[i]` (e.g. `acc`,
    * `acc.voltage`, `rows[0].v`). */
  private def renderPath(path: BbinPath): String =
    path.segments.zipWithIndex.map {
      case (BbinPathSeg.Field(name), 0) => name
      case (BbinPathSeg.Field(name), _) => s".$name"
      case (BbinPathSeg.Index(i), _)    => s"[$i]"
    }.mkString

  /** `Ident` occurrences read by an expression. `SchemaPath`,
    * `BbinFieldValue.Path` and `BbinMapArg.Path` segments resolve against
    * row types, not `let`s, and contribute nothing; loader column maps are
    * declarations with no reads. */
  private def identsInExpr(expr: BbinExpr): Set[String] = expr match {
    case BbinExpr.SetLit(set) =>
      set.entries.flatMap(e => identsInRow(e.row)).toSet
    case BbinExpr.Loader(_) | BbinExpr.Rel(_) | BbinExpr.SchemaPath(_) =>
      Set.empty
    case BbinExpr.Ident(name) =>
      Set(name)
    case BbinExpr.StrLit(_) | BbinExpr.IntLit(_, _) | BbinExpr.FloatLit(_, _) | BbinExpr.BoolLit(_) =>
      Set.empty
    case BbinExpr.NoneLit | BbinExpr.UndefinedLit =>
      Set.empty
    case BbinExpr.BinOp(_, l, r) =>
      identsInExpr(l) ++ identsInExpr(r)
    case BbinExpr.Leaf(base, overs) =>
      identsInExpr(base) ++ overs.flatMap(f => identsInFieldValue(f.value))
    case BbinExpr.StructLit(row) =>
      identsInRow(row)
    case BbinExpr.PartialSeal(t) =>
      t.map(identsInExpr).getOrElse(Set.empty)
    case BbinExpr.Chain(base, ops) =>
      identsInExpr(base) ++ ops.flatMap(identsInOp)
  }

  private def identsInOp(op: BbinChainOp): Set[String] = op match {
    case BbinChainOp.Map(_) =>
      Set.empty
    case BbinChainOp.Filter(pred) =>
      identsInExpr(pred)
    case BbinChainOp.Cross(other)      => identsInExpr(other)
    case BbinChainOp.InnerJoin(other)  => identsInExpr(other)
    case BbinChainOp.OuterJoin(other)  => identsInExpr(other)
    case BbinChainOp.LeftJoin(other)   => identsInExpr(other)
    case BbinChainOp.RightJoin(other)  => identsInExpr(other)
    case BbinChainOp.Zip(other)        => identsInExpr(other)
    case BbinChainOp.Concat(other)     => identsInExpr(other)
    case BbinChainOp.Reindex(other)    => identsInExpr(other)
    case BbinChainOp.Seal | BbinChainOp.Unseal | BbinChainOp.Unwrap =>
      Set.empty
    case BbinChainOp.WithProvenance(fields) =>
      fields.flatMap(f => identsInFieldValue(f.value)).toSet
  }

  private def identsInRow(row: BbinRow): Set[String] =
    row.fields.flatMap(f => identsInFieldValue(f.value)).toSet

  private def identsInFieldValue(value: BbinFieldValue): Set[String] = value match {
    case BbinFieldValue.Ident(name) => Set(name)
    case _                          => Set.empty
  }

  /** `Ident`s read by `Filter` predicates anywhere in an expression
    * (through chains and deferred operands). A filter inside a `let` chain
    * binds no name of its own, so callers attribute these to the filtered
    * set's binding (the `let` holding the chain result) instead. */
  private def filterPredIdents(expr: BbinExpr): Set[String] = expr match {
    case BbinExpr.Chain(base, ops) =>
      filterPredIdents(base) ++ ops.flatMap {
        case BbinChainOp.Filter(pred) => identsInExpr(pred) ++ filterPredIdents(pred)
        case BbinChainOp.Cross(other) => filterPredIdents(other)
        case BbinChainOp.InnerJoin(other) => filterPredIdents(other)
        case BbinChainOp.OuterJoin(other) => filterPredIdents(other)
        case BbinChainOp.LeftJoin(other) => filterPredIdents(other)
        case BbinChainOp.RightJoin(other) => filterPredIdents(other)
        case BbinChainOp.Zip(other) => filterPredIdents(other)
        case BbinChainOp.Concat(other) => filterPredIdents(other)
        case BbinChainOp.Reindex(other) => filterPredIdents(other)
        case _ => Set.empty
      }
    case BbinExpr.BinOp(_, l, r) => filterPredIdents(l) ++ filterPredIdents(r)
    case BbinExpr.Leaf(base, _)  => filterPredIdents(base)
    case BbinExpr.PartialSeal(t) => t.map(filterPredIdents).getOrElse(Set.empty)
    case _                       => Set.empty
  }

  /** Row-type names carried by a `let` type (the indexed-set scalar is not
    * a domain row and is excluded, mirroring [[BbinSlotCheck]]). */
  private def rowNamesOf(tpe: BbinType): List[String] = {
    val refs = tpe match {
      case BbinType.Plain(ref)            => List(ref)
      case BbinType.Partial(underlying)   => List(underlying)
      case BbinType.SetOf(elem)           => List(elem)
      case BbinType.IndexedSet(_, _, elem) => List(elem)
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
}
