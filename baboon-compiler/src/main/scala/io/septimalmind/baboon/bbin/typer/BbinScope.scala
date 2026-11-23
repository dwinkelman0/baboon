package io.septimalmind.baboon.bbin.typer

import io.septimalmind.baboon.bbin.model.*
import io.septimalmind.baboon.bbin.parser.BbinImportResolver.ResolvedImport
import io.septimalmind.baboon.parser.model.{RawTypeName, RawTypeRef}

/** Origin of a [[ScopeEntry]] binding. */
sealed trait ScopeOrigin

object ScopeOrigin {
  case object Let extends ScopeOrigin
  case object Import extends ScopeOrigin
  case object ForBinder extends ScopeOrigin
  case object FnParam extends ScopeOrigin
}

/** A single name in scope.
  *
  * `tpe` is the normalized [[BbinType]] annotation (imports contribute their
  * effective `resolvedType`). `RawTypeRef` is retired after parsing: downstream
  * phases must read types from the scope, never from raw annotations.
  *
  * `readOnly` marks `for` binders and `fn` params, which bodies may read but
  * must not assign to or re-bind.
  */
final case class ScopeEntry(
  name: String,
  tpe: BbinType,
  readOnly: Boolean,
  origin: ScopeOrigin,
)

/** Phase 2 output: the flat let/import/binder scope plus the global
  * index name→type map (index name → rendered `idx_type`).
  */
final case class Scope(
  entries: Map[String, ScopeEntry],
  indexTypes: Map[String, String],
) {
  def get(name: String): Option[ScopeEntry] =
    entries.get(name)

  def typeOf(name: String): Option[BbinType] =
    entries.get(name).map(_.tpe)

  def isReadOnly(name: String): Boolean =
    entries.get(name).exists(_.readOnly)

  def indexTypeOf(index: String): Option[String] =
    indexTypes.get(index)
}

/** Phase 2 — scope + index table.
  *
  * Walks `let`s in order over [[io.septimalmind.baboon.bbin.model.RawBbin],
  * seeding the scope with `resolvedImports` (effective name =
  * `alias.getOrElse(name)`, type = `resolvedType`; ascription compatibility
  * was already checked upstream and is not rechecked here).
  *
  * Invariants enforced here:
  *  - any duplicate name (`let` vs `let`, binder vs outer binding, `fn`
  *    param vs outer binding) is an error — shadowing is not allowed;
  *  - an index name used with two different `idx_type`s is an error,
  *    workspace-wide: across `let`s and resolved imports carrying indexed
  *    types (same name + same type is idempotent and fine).
  *
  * `for` binders extend the scope as read-only; their types are derived
  * best-effort from the iterated set when it is a plain `Ident` over a
  * known set type (`{idx: T > E}` gives `idx: T`, `row: E`). Anything else
  * (forward reference, computed set, plain `{E}` ordinal) records a
  * synthetic placeholder — slot-typecheck (phase 3) re-derives binder
  * types with full checking; scope-build never fails on unknown names.
  */
object BbinScopeBuilder {

  private val unknownRef: RawTypeRef.Simple =
    RawTypeRef.Simple(RawTypeName("unknown"), Nil)

  private val unknownType: BbinType =
    BbinType.Plain(unknownRef)

  def build(raw: RawBbin, resolvedImports: List[ResolvedImport]): (Scope, List[String]) = {
    var entries    = Map.empty[String, ScopeEntry]
    var indexTypes = Map.empty[String, String]
    val errors     = Vector.newBuilder[String]

    def registerIndex(tpe: BbinType, where: String): Unit = tpe match {
      case BbinType.IndexedSet(idx, idxType, _) =>
        val rendered = idxType.render
        indexTypes.get(idx) match {
          case Some(prev) if prev != rendered =>
            errors += s"$where: index '$idx' conflicts: already used with idx_type '$prev', now '$rendered'"
          case _ =>
            indexTypes = indexTypes.updated(idx, rendered)
        }
      case _ => ()
    }

    def add(entry: ScopeEntry, where: String): Unit =
      entries.get(entry.name) match {
        case Some(prev) =>
          errors += s"$where: name '${entry.name}' is already bound (${describe(prev)}); shadowing is not allowed"
        case None =>
          entries = entries.updated(entry.name, entry)
          registerIndex(entry.tpe, where)
      }

    def binderTypesFor(set: BbinExpr): (BbinType, BbinType) = set match {
      case BbinExpr.Ident(n) =>
        entries.get(n).map(_.tpe) match {
          case Some(BbinType.IndexedSet(_, idxType, elem)) =>
            (BbinType.Plain(idxType), BbinType.Plain(elem))
          case Some(BbinType.SetOf(elem)) =>
            (unknownType, BbinType.Plain(elem))
          case _ =>
            (unknownType, unknownType)
        }
      case _ =>
        (unknownType, unknownType)
    }

    def walkStmts(stmts: List[BbinStmt]): Unit = stmts.foreach {
      case BbinStmt.LetStmt(l) =>
        add(ScopeEntry(l.name, l.tpe, readOnly = false, ScopeOrigin.Let), s"let '${l.name}'")
      case BbinStmt.AssignStmt(_) =>
        ()
      case BbinStmt.ForStmt(f) =>
        val (idxT, rowT) = binderTypesFor(f.set)
        add(ScopeEntry(f.idx, idxT, readOnly = true, ScopeOrigin.ForBinder), s"for binder '${f.idx}'")
        add(ScopeEntry(f.row, rowT, readOnly = true, ScopeOrigin.ForBinder), s"for binder '${f.row}'")
        walkStmts(f.body)
      case BbinStmt.WithStmt(w) =>
        walkStmts(w.body)
    }

    resolvedImports.foreach { ri =>
      val effective = ri.alias.getOrElse(ri.name)
      add(
        ScopeEntry(effective, ri.resolvedType, readOnly = false, ScopeOrigin.Import),
        s"import '${ri.name}'",
      )
    }
    walkStmts(raw.statements)

    (Scope(entries, indexTypes), errors.result().toList)
  }

  /** Push `fn` params into an existing scope as read-only bindings.
    * Shadowing an outer name is an error, mirroring `for` binders.
    */
  def extendReadOnly(scope: Scope, params: List[(String, BbinType)]): (Scope, List[String]) = {
    var entries = scope.entries
    val errors  = Vector.newBuilder[String]
    params.foreach { case (name, tpe) =>
      entries.get(name) match {
        case Some(prev) =>
          errors += s"fn param '$name' is already bound (${describe(prev)}); shadowing is not allowed"
        case None =>
          entries = entries.updated(name, ScopeEntry(name, tpe, readOnly = true, ScopeOrigin.FnParam))
      }
    }
    (scope.copy(entries = entries), errors.result().toList)
  }

  private def describe(entry: ScopeEntry): String = entry.origin match {
    case ScopeOrigin.Let       => s"let '${entry.name}'"
    case ScopeOrigin.Import    => s"import '${entry.name}'"
    case ScopeOrigin.ForBinder => s"for binder '${entry.name}'"
    case ScopeOrigin.FnParam   => s"fn param '${entry.name}'"
  }
}
