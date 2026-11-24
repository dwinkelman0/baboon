package io.septimalmind.baboon.typer

import io.septimalmind.baboon.typer.model._

/** P5: read gate for context functions (wired via `FnPipeline.pass5`).
  *
  * Input: one [[Input]] per fn site — the extracted [[FnSite]], its typed body, the P4
  * [[Sensitivity]] and (for `migrate` only) the declared source set (`ReadTargets`).
  * Output: the same sites annotated with a materialization pass plus the validated
  * read-target sets downstream passes (P6 ordering, backend emission) may rely on.
  *
  * Pass timeline (R7):
  *   - pass 0: `migrate` fns. They read the previous-shape sources (`!prev`), which are
  *     always available, and nothing else.
  *   - pass 1: `default` / `normalize` fns. Within the pass, children materialize before
  *     parents (bottom-up). The gate only assigns the coarse pass number; the fine
  *     child-before-parent order is approximated in P6 via read-after-write overlap plus
  *     capture-deferral edges (a captor runs after the captured type's fns), or via
  *     schema type depth / declaration order when no finer edge exists.
  *   - pass 2: `computed` fns, last.
  *
  * Checks (R6, all reported as plain strings; tests assert on the message text):
  *   - every `leafReads` target (and every declared source) must resolve against the
  *     schema — unknown types / fields are errors;
  *   - `default` / `normalize` fns must not read computed slots (pass 1 runs before
  *     pass 2); the computed-slot set is derived from the inputs themselves (every
  *     slot carrying a `Computed` site);
  *   - `migrate` reads (slot granularity: owner + head segment) must be a subset of
  *     the declared sources.
  *
  * Self-reads (a fn reading its own slot) pass the gate deliberately: they are P6's
  * cycle check (`FnDepGraphBuilder`), which reports them as hard dependency cycles.
  */
object FnReadGate {

  /** One fn awaiting gating. `read` / `write` are the P4 sensitivities;
    * `declaredSources` is the migrate source set; it must be
    * empty for non-`migrate` roles (not enforced — ignored outside `migrate`).
    */
  final case class Input(
    site: FnSite,
    body: TypedBody,
    read: Sensitivity,
    write: Sensitivity,
    declaredSources: Set[SchemaPath]
  )

  /** A gated fn: coarse materialization `pass` (see [[passOf]]) plus the validated
    * `readTargets` (== the input `leafReads`, re-validated against the schema).
    * `body` is carried through untouched for P6 / backend use.
    */
  final case class Gated(
    site: FnSite,
    owner: TypeId.User,
    body: TypedBody,
    read: Sensitivity,
    write: Sensitivity,
    pass: Int,
    readTargets: Set[SchemaPath]
  )

  /** Coarse materialization pass: migrate sources first, defaults/normalizes in the
    * middle (bottom-up within the pass, see P6), computed last.
    */
  def passOf(role: FnRole): Int = role match {
    case FnRole.Migrate                 => 0
    case FnRole.Default | FnRole.Normalize => 1
    case FnRole.Computed                => 2
  }

  def roleName(role: FnRole): String = role match {
    case FnRole.Default   => "default"
    case FnRole.Normalize => "normalize"
    case FnRole.Migrate   => "migrate"
    case FnRole.Computed  => "computed"
  }

  /** Deterministic role rank used as the last tie-break key in P6. Follows the
    * declaration order of [[FnRole]].
    */
  def roleRank(role: FnRole): Int = role match {
    case FnRole.Default   => 0
    case FnRole.Normalize => 1
    case FnRole.Migrate   => 2
    case FnRole.Computed  => 3
  }

  def renderSite(site: FnSite): String =
    s"${site.id.owner}.${site.id.field}(${roleName(site.id.role)})"

  /** Slot granularity: the materialization unit is the head field of a leaf path
    * (`Basket.items.label` materializes with slot `Basket.items`).
    */
  def slotOf(p: SchemaPath): SchemaPath = p.slot

  def gate(inputs: List[Input], schema: FnSchema): Either[List[String], List[Gated]] = {
    val errors = List.newBuilder[String]

    // -- 1. resolve owners ----------------------------------------------------
    val resolved: List[Option[(Input, TypeId.User)]] = inputs.map { in =>
      schema.lookup(in.site.id.owner) match {
        case Some(t: Typedef.Dto)      => Some((in, t.id))
        case Some(t: Typedef.Contract) => Some((in, t.id))
        case Some(t: Typedef.Adt)      => Some((in, t.id))
        case _ =>
          errors += s"fn ${renderSite(in.site)}: unknown owner type `${in.site.id.owner}`"
          None
      }
    }

    // -- 2. computed slots (pass-2 materialization units) ----------------------
    val computedSlots: Set[SchemaPath] = resolved.collect {
      case Some((in, owner)) if in.site.id.role == FnRole.Computed =>
        SchemaPath.resolve(owner, List(in.site.id.field), schema)
    }.collect { case Right(p) => p }.toSet

    // -- 3. per-fn checks -------------------------------------------------------
    val out = resolved.collect { case Some((in, owner)) =>
      val tag = s"fn ${renderSite(in.site)}"

      in.read.leafReads.foreach { r =>
        resolvePath(schema, r) match {
          case Some(m) => errors += s"$tag: $m"
          case None    => ()
        }
      }
      in.declaredSources.foreach { s =>
        resolvePath(schema, s) match {
          case Some(m) => errors += s"$tag: source ${s.render}: $m"
          case None    => ()
        }
      }

      in.site.id.role match {
        case FnRole.Default | FnRole.Normalize =>
          in.read.leafReads.foreach { r =>
            if (computedSlots.contains(slotOf(r)))
              errors += s"$tag: ${roleName(in.site.id.role)} fn reads computed slot ${slotOf(r).render} " +
                s"(computed fields materialize in pass 2, after defaults)"
          }
        case FnRole.Migrate =>
          val allowed = in.declaredSources.map(slotOf)
          in.read.leafReads.foreach { r =>
            if (!allowed.contains(slotOf(r)))
              errors += s"$tag: migrate fn reads undeclared source ${r.render} (declare it in sources)"
          }
        case FnRole.Computed => ()
      }

      Gated(in.site, owner, in.body, in.read, in.write, passOf(in.site.id.role), in.read.leafReads)
    }

    val errs = errors.result().distinct
    if (errs.nonEmpty) Left(errs) else Right(out)
  }

  // -- schema path validation (mirrors the P4 extractor walk) -------------------

  private def resolvePath(schema: FnSchema, path: SchemaPath): Option[String] =
    SchemaPath.resolve(path.root, path.fieldNames, schema).left.toOption
}
