package io.septimalmind.baboon.typer.model

/** Dependency graph over fn-annotated fields, used for instantiation
  * ordering and cycle detection. Strictly separate from
  * `loopsOf`/`isRecursiveTypedef` (wire recursion) — own graph type,
  * own seen set, own validator step.
  */
case class FnNode(field: (TypeId.User, FieldName), isComputed: Boolean)

case class FnEdge(from: FnNode, to: FnNode, kind: DepKind)

sealed trait DepKind

object DepKind {
  case object ReadAfter extends DepKind
  case object NormalizeAfter extends DepKind
}

case class FnDepGraph(nodes: Set[FnNode] = Set.empty, edges: Set[FnEdge] = Set.empty)

object FnDepGraph {
  val empty: FnDepGraph = FnDepGraph()
}
