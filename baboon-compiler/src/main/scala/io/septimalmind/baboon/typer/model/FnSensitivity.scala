package io.septimalmind.baboon.typer.model

/** Inferred read/write sensitivity of a [[TypedFn]].
  *
  * - `leafReads`: leaf [[SchemaPath]]s (value *at* path; collections transparent).
  * - `shapeReads`: container membership/shape deps — every `Constructor`
  *   traversed, keyed by owner `TypeId.User`, plus ADT owners with variant steps.
  *
  * `write` uses the same shape: struct-literal creation writes ALL fields of
  * the constructed DTO (whole-value return replaces the subtree).
  */
case class Sensitivity(
  leafReads: Set[SchemaPath] = Set.empty,
  shapeReads: Set[TypeId.User] = Set.empty
)

object Sensitivity {
  val empty: Sensitivity = Sensitivity()
}
