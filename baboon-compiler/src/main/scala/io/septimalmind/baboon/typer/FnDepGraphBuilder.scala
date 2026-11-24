package io.septimalmind.baboon.typer

import io.septimalmind.baboon.typer.model._
import io.septimalmind.baboon.typer.model.FnTypes._

import scala.collection.mutable

/** P6: dependency graph + emission order for gated fns (wired via
  * `FnPipeline.pass6`).
  *
  * Nodes are fn nodes (`owner + field + role`) plus one base field node per materialized
  * slot. An edge `B -> A` ("B after A") means B must be instantiated after A:
  *   - `ReadAfter`: B's reads overlap A's writes — same [[SchemaPath]] slot (owner +
  *     head segment) or a shape overlap (B's `shapeReads` contains an owner A writes to).
  *   - `CaptureDefer` (R8): B captures type T, so B defers to the materialization of
  *     T's fns. Exactly one annotated edge per (captor, captured-type) pair, even for
  *     star captures that fan out to many element reads.
  *   - `PassOrder`: the P5 pass timeline (migrate < default/normalize < computed); every
  *     higher-pass fn runs after every lower-pass fn.
  *   - `SlotMaterialization`: every fn runs before its own slot node, and every reader
  *     runs after the slot nodes it reads, so plain fields interleave deterministically.
  *
  * Topo sort is Kahn's algorithm with a deterministic tie-break on
  * `(TypeId, field, role)` — owner `toString`, then field name, then
  * [[FnReadGate.roleRank]] (base field slots sort before fns on the same slot).
  * Any cycle (including a fn reading its own slot) is a hard error whose message
  * carries the `A -> B -> A` node path (R10).
  */
object FnDepGraphBuilder {

  /** Fn node identity: owning DTO + field + role. */
  final case class FnNodeId(owner: TypeId.User, field: String, role: FnRole) {
    def render: String = s"${owner.name.name}.$field(${FnReadGate.roleName(role)})"
  }

  sealed trait DepNode {
    def sortKey: (String, String, Int)
    def render: String
  }
  object DepNode {
    final case class Fn(id: FnNodeId) extends DepNode {
      def sortKey: (String, String, Int) = (id.owner.toString, id.field, FnReadGate.roleRank(id.role))
      def render: String                 = id.render
    }
    final case class FieldSlot(owner: TypeId.User, field: String) extends DepNode {
      def sortKey: (String, String, Int) = (owner.toString, field, -1)
      def render: String                 = s"${owner.name.name}.$field"
    }
  }

  sealed trait DepKind {
    def render: String
  }
  object DepKind {
    case object ReadAfter extends DepKind { def render: String = "read-after" }
    case object CaptureDefer extends DepKind { def render: String = "capture-defer" }
    case object PassOrder extends DepKind { def render: String = "pass-order" }
    case object SlotMaterialization extends DepKind { def render: String = "slot" }
  }

  /** `after` runs after `before`. */
  final case class DepEdge(after: DepNode, before: DepNode, kind: DepKind)

  /** One fn in the ordering problem. `captureOwner` is the type the fn's capture
    * roots at (R8 deferral target); `pass` is the P5 pass number.
    */
  final case class Input(
    id: FnNodeId,
    read: Sensitivity,
    write: Sensitivity,
    captureOwner: Option[TypeId.User],
    pass: Int
  )

  final case class EmissionOrder(nodes: List[DepNode], edges: List[DepEdge])

  def nodeIdOf(g: FnReadGate.Gated): FnNodeId =
    FnNodeId(g.owner, g.site.id.field, g.site.id.role)

  /** Capture-deferral target of a P2 [[TypedCapture]]: the user type its root sits at. */
  def captureOwnerOf(cap: TypedCapture): Option[TypeId.User] = cap.rootType match {
    case TypeRef.Scalar(u: TypeId.User) => Some(u)
    case _                              => None
  }

  /** Lift P5 output into P6 inputs. `captures` maps each fn to its (optional) P2
    * capture; fns absent from the map (or mapped to `None`) capture nothing.
    */
  def fromGated(
    gated: List[FnReadGate.Gated],
    captures: Map[FnNodeId, Option[TypedCapture]]
  ): List[Input] =
    gated.map { g =>
      val id = nodeIdOf(g)
      Input(id, g.read, g.write, captures.get(id).flatten.flatMap(captureOwnerOf), g.pass)
    }

  def build(inputs: List[Input], schema: FnSchema): Either[List[String], EmissionOrder] = {
    val dupes = inputs.groupBy(_.id).collect { case (id, l) if l.size > 1 => id }
    if (dupes.nonEmpty)
      return Left(List(s"duplicate fn nodes: ${dupes.map(_.render).toList.sorted.mkString(", ")}"))

    val fnNodes: List[DepNode.Fn] = inputs.map(i => DepNode.Fn(i.id))

    // -- slot universe: every field of every involved owner, plus any read/written slot
    val owners: Set[TypeId.User] = inputs.map(_.id.owner).toSet
    val schemaSlots: Set[DepNode.FieldSlot] = owners.flatMap { o =>
      dtoFields(TypeRef.Scalar(o), schema).getOrElse(Nil).map(f => DepNode.FieldSlot(o, f.name.name))
    }
    val ioSlots: Set[DepNode.FieldSlot] = inputs.flatMap { i =>
      val paths = i.read.leafReads ++ i.write.leafReads
      paths.map(p => DepNode.FieldSlot(p.root, p.headName.getOrElse(""))) +
        DepNode.FieldSlot(i.id.owner, i.id.field)
    }.toSet
    // Drop degenerate owner-only slots (empty field) that carry no materialization meaning.
    val slotNodes: Set[DepNode.FieldSlot] = (schemaSlots ++ ioSlots).filter(_.field.nonEmpty)

    val byId: Map[FnNodeId, Input] = inputs.map(i => i.id -> i).toMap

    val edges = mutable.LinkedHashSet.empty[DepEdge]

    // -- read-after-write fn -> fn edges (slot granularity + shape overlap) ------
    for (b <- inputs; a <- inputs) {
      // Shape-only self-overlap is vacuous (a fn trivially
      // runs "after" itself). for-in/match over an own-DTO collection puts the
      // owner in shapeReads while the attachment-slot write shares that root,
      // so without this guard every such fn self-cycles. Same-slot self-reads
      // stay hard errors (R10): only the shape-only self case is skipped.
      if (overlaps(b.read, a.write) && (b.id != a.id || slotOverlaps(b.read, a.write)))
        edges += DepEdge(DepNode.Fn(b.id), DepNode.Fn(a.id), DepKind.ReadAfter)
    }

    // -- R8 capture deferral: single annotated edge per (captor, captured type) --
    for (b <- inputs; t <- b.captureOwner.toList; a <- inputs if a.id.owner == t && a.id != b.id) {
      edges += DepEdge(DepNode.Fn(b.id), DepNode.Fn(a.id), DepKind.CaptureDefer)
    }

    // -- pass timeline: higher passes run after lower passes ---------------------
    for (b <- inputs; a <- inputs if b.pass > a.pass) {
      edges += DepEdge(DepNode.Fn(b.id), DepNode.Fn(a.id), DepKind.PassOrder)
    }

    // -- slot materialization ------------------------------------------------------
    val slotByKey: Map[(TypeId.User, String), DepNode.FieldSlot] =
      slotNodes.map(s => (s.owner, s.field) -> s).toMap
    def slotNode(owner: TypeId.User, field: String): Option[DepNode.FieldSlot] =
      slotByKey.get((owner, field))
    for (a <- inputs; s <- slotNode(a.id.owner, a.id.field).toList) {
      edges += DepEdge(s, DepNode.Fn(a.id), DepKind.SlotMaterialization)
    }
    for (b <- inputs; r <- b.read.leafReads; s <- slotNode(r.root, r.headName.getOrElse("")).toList) {
      edges += DepEdge(DepNode.Fn(b.id), s, DepKind.SlotMaterialization)
    }

    val allNodes: List[DepNode] = (fnNodes ++ slotNodes).distinct
    kahn(allNodes, edges.toList).map(order => EmissionOrder(order, edges.toList))
  }

  /** B reads something A writes: same slot (root + head segment) or a shape overlap
    * (B's `shapeReads` holds an owner A writes to).
    */
  private def overlaps(b: Sensitivity, a: Sensitivity): Boolean =
    slotOverlaps(b, a) ||
      b.shapeReads.exists(o => a.leafReads.exists(_.root == o))

  /** Slot-granularity half of [[overlaps]], factored out so
    * the builder can keep same-slot self-reads as hard errors while ignoring
    * vacuous shape-only self-edges (see call site). */
  private def slotOverlaps(b: Sensitivity, a: Sensitivity): Boolean = {
    val bReads = b.leafReads.map(slotKey)
    val aWrites = a.leafReads.map(slotKey)
    (bReads intersect aWrites).nonEmpty
  }

  private def slotKey(p: SchemaPath): (TypeId.User, Option[String]) =
    (p.root, p.headName)

  /** Model vs builder worlds: `model.FnDepGraph` is the stable graph over
    * fn-annotated fields (fn nodes only, `ReadAfter`/`NormalizeAfter` kinds);
    * the builder adds ordering-only detail (base field-slot nodes plus
    * `CaptureDefer`/`PassOrder`/`SlotMaterialization` edges) to derive a
    * deterministic emission order. The projections below drop ordering-only
    * detail (`None` where the model has no counterpart).
    */
  def toModelNode(n: DepNode): Option[model.FnNode] = n match {
    case DepNode.Fn(id) =>
      Some(model.FnNode((id.owner, FieldName(id.field)), id.role == FnRole.Computed))
    case _: DepNode.FieldSlot => None
  }

  def toModelKind(k: DepKind): Option[model.DepKind] = k match {
    case DepKind.ReadAfter => Some(model.DepKind.ReadAfter)
    case _                 => None
  }

  // -- deterministic Kahn ----------------------------------------------------------

  private def kahn(nodes: List[DepNode], edges: List[DepEdge]): Either[List[String], List[DepNode]] = {
    implicit val keyOrd: Ordering[(String, String, Int)] =
      Ordering.Tuple3(Ordering.String, Ordering.String, Ordering.Int)
    val after  = mutable.Map.empty[DepNode, mutable.Set[DepNode]]
    val indeg  = mutable.Map.empty[DepNode, Int].withDefaultValue(0)
    nodes.foreach(n => indeg(n) = 0)
    edges.foreach { e =>
      if (!after.getOrElseUpdate(e.before, mutable.Set.empty).contains(e.after)) {
        after.getOrElseUpdate(e.before, mutable.Set.empty) += e.after
        indeg(e.after) = indeg(e.after) + 1
      }
    }
    val out       = List.newBuilder[DepNode]
    val remaining = mutable.LinkedHashSet.empty[DepNode] ++ nodes
    var ready     = nodes.filter(n => indeg(n) == 0).sortBy(_.sortKey)
    while (ready.nonEmpty) {
      val n = ready.head
      ready = ready.tail
      out += n
      remaining -= n
      val next = after.getOrElse(n, Set.empty).toList.sortBy(_.sortKey)
      next.foreach { m =>
        indeg(m) = indeg(m) - 1
        if (indeg(m) == 0 && remaining.contains(m)) ready = (ready :+ m).sortBy(_.sortKey)
      }
    }
    if (remaining.isEmpty) Right(out.result())
    else Left(List(s"fn dependency cycle: ${findCycle(remaining.toSet, edges).mkString(" -> ")}"))
  }

  /** Render-node DFS over the stalled subgraph; always finds a closed `A -> ... -> A`
    * path when Kahn stalls (every remaining node has a remaining predecessor).
    */
  private def findCycle(nodes: Set[DepNode], edges: List[DepEdge]): List[String] = {
    val succ: Map[DepNode, List[DepNode]] = {
      val m = mutable.Map.empty[DepNode, List[DepNode]].withDefaultValue(Nil)
      edges.foreach { e =>
        if (nodes.contains(e.before) && nodes.contains(e.after))
          m(e.before) = e.after :: m(e.before)
      }
      m.toMap
    }
    val ordered = nodes.toList.sortBy(_.sortKey)
    val state   = mutable.Map.empty[DepNode, Int].withDefaultValue(0) // 0 unvisited, 1 on stack, 2 done
    var found: List[DepNode] = Nil
    def visit(n: DepNode, stack: List[DepNode]): Boolean = {
      state(n) = 1
      val path = stack :+ n
      val hasCycle = succ.getOrElse(n, Nil).sortBy(_.sortKey).exists { m =>
        if (state(m) == 1) {
          found = (path.drop(path.indexOf(m)) :+ m)
          true
        } else if (state(m) == 0) visit(m, path)
        else false
      }
      state(n) = 2
      hasCycle
    }
    ordered.exists(n => state(n) == 0 && visit(n, Nil))
    if (found.nonEmpty) found.map(_.render)
    else ordered.map(_.render) // unreachable: Kahn stalls only on cycles
  }
}
