package io.septimalmind.baboon.parser.model

sealed trait RawLocatorSeg

/** Capture-path segments at AST level.
  *
  * Collections (`lst`/`map`/`set`/`opt`) are transparent in capture paths:
  * traversing through them is an ordinary `.field` step with no key required.
  * In particular `opt.Some` is implied when the path descends into the payload
  * (`None` has no child), so the canonical form omits `.Some`. An explicit
  * `.Some` still parses here as `Field("Some")` (resolved later by the typer),
  * never as `Variant`.
  */
object RawLocatorSeg {
  final case class Field(name: String) extends RawLocatorSeg
  final case class Variant(name: String) extends RawLocatorSeg
  final case class Key(name: String) extends RawLocatorSeg // "$var"
}

final case class RawStarCapture(chainName: String, steps: List[RawLocatorSeg])

/** One ordered step of a capture path: either a plain locator segment or a star
  * expansion. The parser preserves source order, so multiple and interleaved
  * stars (e.g. `a($c1* : .x).b($c2* : .y)`) round-trip faithfully.
  *
  * Collection traversal (including `opt` payload situating) stays inline as
  * plain `.field` steps — stars expand over the same flat step sequence.
  *
  * NOTE: star steps themselves stay flat for now — a star cannot contain a
  * nested star (`($c* : ($d* : .x))` does not parse). Nested stars are deferred
  * until the typer IR (`TypedCapture`) grows an ordered-path representation.
  */
sealed trait RawCaptureStep

object RawCaptureStep {
  final case class Single(seg: RawLocatorSeg) extends RawCaptureStep
  final case class Star(star: RawStarCapture) extends RawCaptureStep
}

final case class RawCapture(
  name: String,
  rootType: RawTypeRef,
  path: List[RawCaptureStep],
) {

  /** Backward-compatible view: the first star step, if any. */
  def star: Option[RawStarCapture] = path.collectFirst { case RawCaptureStep.Star(s) => s }

  /** Backward-compatible view: all plain locator segments in order. */
  def locator: List[RawLocatorSeg] = path.collect { case RawCaptureStep.Single(seg) => seg }
}

final case class RawFnParam(name: String, tpe: RawTypeRef)

final case class RawBody(letBindings: List[(String, BodyExpr)], result: BodyExpr)

/** Context-function AST node: at most ONE capture (highest scope needed).
  * Zero captures (`fn(...){...}`) is valid; two captures fail at parse time.
  * Previous-version (`!old: T`) capture-prefix syntax is deferred to a later phase.
  */
final case class RawFn(captures: List[RawCapture], params: List[RawFnParam], body: RawBody)

final case class RawMigrate(fn: RawFn, sources: List[String])
