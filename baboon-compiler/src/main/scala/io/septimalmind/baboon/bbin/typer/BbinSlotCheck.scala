package io.septimalmind.baboon.bbin.typer

import io.septimalmind.baboon.bbin.model.*
import io.septimalmind.baboon.parser.model.{RawTypeName, RawTypeRef}

/** Phase 3 — slot-typecheck.
 *
 * Shallow structural checks over a single [[BbinStmt]] against the phase-2
 * [[Scope]] and a set of known row-type names:
 *
 *   - row type names referenced by the statement (let-type underlying refs,
 *     set-literal/struct-literal row names, loader row types) must be members
 *     of `knownTypes`;
 *   - `BbinType` forms are the four sealed shapes (`T`/`~T`/`{T}`/`{idx>t}`);
 *     an indexed set's `idxType` must additionally be a primitive leaf —
 *     `str` or the unsigned-int family (`u08`/`u16`/`u32`/`u64`/`usize`);
 *     anything else is rejected;
 *   - chain operators must have a plausible shape: `map` takes at least one
 *     argument, `filter` predicates must not be none/undefined, binary
 *     operators (`cross`, joins, `zip`, `concat`, `reindex`) must not take an
 *     undefined `other`, `withProvenance` requires at least one field
 *     (`seal`/`unseal` take no arguments by construction and need no check);
 *   - there is no set-level `.map`: a `map` chain op is legal only when the
 *     immediate chain base is a loader expression — iteration is native `for`
 *     (loader column `.map` is unrelated and stays);
 *   - deferred-position rule: a leading-dot schema path
 *     ([[BbinExpr.SchemaPath]], [[BbinFieldValue.Path]]) may appear only in
 *     deferred/implied-deferred positions — `filter` predicates, assign RHS
 *     values, `cross`/join/`zip` mapper fn bodies, `with_provenance` fn
 *     bodies, `for` bodies, `leaf(...)` args — or as loader `.map`
 *     column-mapping declarations (explicitly exempt). A schema path in an
 *     eager `let` RHS or in a set-literal field value requires an
 *     `fn()`/deferred row context and is rejected;
 *   - writes to read-only scope names (`for` binders, `fn` params) via the
 *     root of an assign target are rejected, as are reads of unknown
 *     identifiers.
 *
 * Checks are deliberately shallow: no type inference, no arity checking beyond
 * the shapes above. Unknown-operator errors are impossible by construction
 * (the operators form a sealed trait resolved against the method table). The
 * deferred-position rule is purely structural — it never consults a
 * `Domain`; checking `SchemaPath` segments against the `using` pin is a later
 * wiring step.
 *
 * TODO (later phase): wire `knownTypes` to the real Domain instead of a
 * caller-supplied set.
 */
object BbinSlotCheck {

  /** Implicit locals that never appear in the scope but may still be read
   * inside expressions (e.g. the current row in a `filter` predicate). */
  private val selfLike: Set[String] = Set("self", "it")

  /** Primitive leaves legal as an indexed-set index type: `str` or the
   * unsigned-int family (anonymous sets are `usize`-equivalent internally). */
  private val indexLeafTypes: Set[String] = Set("str", "u08", "u16", "u32", "u64", "usize")

  def check(scope: Scope, knownTypes: Set[String], stmt: BbinStmt): List[String] = {
    val errors = Vector.newBuilder[String]

    def fail(msg: String): Unit = {
      errors += msg
      ()
    }

    /** Plain name of a [[RawTypeName]], tolerating newtype payloads (String vs
     * NEString) by unwrapping a single `Wrapper(...)` layer. */
    def nameString(tn: RawTypeName): String = {
      val inner =
        if (tn.productArity >= 1 && tn.productElement(0) != null) tn.productElement(0).toString
        else tn.toString
      val open = inner.indexOf('(')
      val wrapped =
        open > 0 &&
          inner.endsWith(")") &&
          inner.substring(0, open).forall(c => c.isLetterOrDigit || c == '.' || c == '_')
      if (wrapped) inner.substring(open + 1, inner.length - 1) else inner
    }

    /** All row-type names referenced by a [[RawTypeRef]], collected by walking
     * the (sealed) ref structure generically so every current and future
     * variant — `Simple` with its `prefix`, collections, optionals, etc. —
     * is covered without enumerating cases. */
    def refNames(ref: RawTypeRef): List[String] = {
      def loop(v: Any): List[String] = v match {
        case tn: RawTypeName => List(nameString(tn))
        case p: Product      => p.productIterator.toList.flatMap(loop)
        case _               => Nil
      }
      loop(ref).distinct
    }

    /** Renders path segments for error messages (`.a.b[0]` form). */
    def renderPath(segments: List[BbinPathSeg]): String =
      segments.map {
        case BbinPathSeg.Field(name) => s".$name"
        case BbinPathSeg.Index(idx)  => s"[$idx]"
      }.mkString

    /** An indexed set's index type must be a primitive leaf: unqualified `str`
     * or an unsigned-int. Rows, qualified refs, constructors and `any` are all
     * rejected. */
    def isIndexLeaf(ref: RawTypeRef): Boolean = ref match {
      case RawTypeRef.Simple(name, Nil) => indexLeafTypes.contains(nameString(name))
      case _                            => false
    }

    /** Underlying row refs of a let/loader type, after enforcing the index-type
     * form. The index scalar of an indexed set is not a domain row and is not
     * checked against `knownTypes`. */
    def checkType(tpe: BbinType, where: String): Unit = {
      val refs = tpe match {
        case BbinType.Plain(ref)          => List(ref)
        case BbinType.Partial(underlying) => List(underlying)
        case BbinType.SetOf(elem)         => List(elem)
        case BbinType.IndexedSet(_, idxType, elem) =>
          if (!isIndexLeaf(idxType))
            fail(
              s"$where: index type '${idxType.render}' is not a primitive leaf (expected str, u08, u16, u32, u64 or usize)",
            )
          List(elem)
      }
      refs.flatMap(refNames).foreach { n =>
        if (!knownTypes.contains(n)) fail(s"$where: unknown row type '$n'")
      }
    }

    def checkIdent(name: String, where: String): Unit =
      if (!selfLike.contains(name) && scope.get(name).isEmpty)
        fail(s"$where: unknown identifier '$name'")

    /** Whether a `.map` chain op may ride this chain base: only loader calls.
     * `Ident` bases are resolved through scope only where trivially possible —
     * [[BbinType]] has no loader form (a loader is never let-storable as
     * such), so a resolved binding is definitively a row/set binding and never
     * a loader call; anything else falls back to the syntactic base check
     * (allow `map` only when the immediate chain base is a loader
     * expression). */
    def isLoaderChainBase(base: BbinExpr): Boolean = base match {
      case BbinExpr.Loader(_)   => true
      case BbinExpr.Ident(name) => isLoaderBinding(name)
      case _                    => false
    }

    def isLoaderBinding(name: String): Boolean =
      scope.typeOf(name).exists {
        case BbinType.Plain(_)            => false
        case BbinType.Partial(_)          => false
        case BbinType.SetOf(_)            => false
        case BbinType.IndexedSet(_, _, _) => false
      }

    /** Walks a field value in its evaluation context: `deferred = true` marks
     * per-row (fn()/implied-deferred) positions where a leading-dot schema
     * path is legal; in eager positions it is rejected. */
    def checkFieldValue(value: BbinFieldValue, where: String, deferred: Boolean): Unit = value match {
      case BbinFieldValue.Ident(name) => checkIdent(name, where)
      case BbinFieldValue.Path(segments) =>
        if (!deferred)
          fail(s"$where: schema path '${renderPath(segments)}' requires fn()/deferred row context")
      case _ => ()
    }

    def checkRow(row: BbinRow, where: String, deferred: Boolean): Unit = {
      val n = nameString(row.typeName)
      if (!knownTypes.contains(n)) fail(s"$where: unknown row type '$n'")
      row.fields.foreach(f => checkFieldValue(f.value, s"$where field '${f.name}'", deferred))
    }

    /** Walks an expression in its evaluation context. `deferred` never flips
     * back to eager inside a deferred position; the positions that imply
     * deferral are marked explicitly at their call sites below. */
    def checkExpr(expr: BbinExpr, where: String, deferred: Boolean): Unit = expr match {
      case BbinExpr.SetLit(set) =>
        // Set-literal field values construct concrete rows: always eager, even
        // inside an otherwise deferred context.
        set.entries.foreach(e => checkRow(e.row, s"$where set literal", deferred = false))
      case BbinExpr.Loader(call) =>
        checkType(call.rowType, s"$where loader")
      // Loader `.map(col, path)` args (call.maps) are column-mapping
      // declarations, explicitly exempt from the deferred-position rule.
      case BbinExpr.Rel(_) => ()
      case BbinExpr.SchemaPath(path) =>
        if (!deferred)
          fail(s"$where: schema path '${renderPath(path.segments)}' requires fn()/deferred row context")
      case BbinExpr.Ident(name) => checkIdent(name, where)
      case BbinExpr.BinOp(_, l, r) =>
        checkExpr(l, where, deferred)
        checkExpr(r, where, deferred)
      case BbinExpr.Leaf(base, overs) =>
        // `leaf(v, {...})` args are a deferred position.
        checkExpr(base, where, deferred = true)
        overs.foreach(f => checkFieldValue(f.value, s"$where override '${f.name}'", deferred = true))
      case BbinExpr.StructLit(row) => checkRow(row, where, deferred)
      case BbinExpr.PartialSeal(t) => t.foreach(checkExpr(_, where, deferred))
      case BbinExpr.Chain(base, ops) =>
        checkExpr(base, where, deferred)
        if (ops.exists(_.isInstanceOf[BbinChainOp.Map]) && !isLoaderChainBase(base))
          fail(s"$where: map is loader-only; iteration is native for")
        ops.foreach(checkOp(_, where, deferred))
      case BbinExpr.StrLit(_) | BbinExpr.IntLit(_, _) | BbinExpr.FloatLit(_, _) | BbinExpr.BoolLit(_) =>
        ()
      case BbinExpr.NoneLit | BbinExpr.UndefinedLit =>
        ()
    }

    def checkOther(other: BbinExpr, op: String, where: String, deferred: Boolean): Unit = other match {
      case BbinExpr.UndefinedLit => fail(s"$where: '$op' argument must not be undefined")
      case _                     => checkExpr(other, where, deferred)
    }

    def checkOp(op: BbinChainOp, where: String, deferred: Boolean): Unit = op match {
      case BbinChainOp.Map(args) =>
        if (args.isEmpty) fail(s"$where: 'map' requires at least one argument")
      // A `map` op only survives on a loader chain (enforced at the Chain
      // level); its args are loader column declarations — schema paths there
      // are exempt, mirroring BbinLoaderCall.maps.
      case BbinChainOp.Filter(pred) => pred match {
        case BbinExpr.NoneLit      => fail(s"$where: 'filter' predicate must not be none")
        case BbinExpr.UndefinedLit => fail(s"$where: 'filter' predicate must not be undefined")
        // Filter predicates are implied-deferred (per row).
        case _ => checkExpr(pred, where, deferred = true)
      }
      // `cross`/join/`zip` take mapper fn bodies (deferred); `concat`/`reindex`
      // take plain set arguments in the surrounding context.
      case BbinChainOp.Cross(other)     => checkOther(other, "cross", where, deferred = true)
      case BbinChainOp.InnerJoin(other) => checkOther(other, "innerJoin", where, deferred = true)
      case BbinChainOp.OuterJoin(other) => checkOther(other, "outerJoin", where, deferred = true)
      case BbinChainOp.LeftJoin(other)  => checkOther(other, "leftJoin", where, deferred = true)
      case BbinChainOp.RightJoin(other) => checkOther(other, "rightJoin", where, deferred = true)
      case BbinChainOp.Zip(other)       => checkOther(other, "zip", where, deferred = true)
      case BbinChainOp.Concat(other)    => checkOther(other, "concat", where, deferred)
      case BbinChainOp.Reindex(other)   => checkOther(other, "reindex", where, deferred)
      case BbinChainOp.Seal | BbinChainOp.Unseal | BbinChainOp.Unwrap => ()
      case BbinChainOp.WithProvenance(fields) =>
        if (fields.isEmpty) fail(s"$where: 'withProvenance' requires at least one field")
        // with_provenance fn bodies are a deferred position.
        else fields.foreach(f => checkFieldValue(f.value, s"$where provenance '${f.name}'", deferred = true))
    }

    def checkStmt(s: BbinStmt, deferred: Boolean): Unit = s match {
      case BbinStmt.LetStmt(l) =>
        checkType(l.tpe, s"let '${l.name}'")
        // A let RHS is eager unless already inside a deferred (`for`-body)
        // context; set-literal and loader RHS forms get their own treatment
        // inside checkExpr.
        checkExpr(l.expr, s"let '${l.name}'", deferred)
      case BbinStmt.AssignStmt(a) =>
        a.target.segments.headOption match {
          case Some(BbinPathSeg.Field(root)) =>
            if (scope.isReadOnly(root)) fail(s"cannot assign to read-only '$root'")
            else checkIdent(root, "assign target")
          case _ => ()
        }
        // Assign RHS values are implied-deferred (broadcast/per-row writes).
        checkExpr(a.value, "assign", deferred = true)
      case BbinStmt.ForStmt(f) =>
        checkExpr(f.set, s"for '${f.row}'", deferred)
        // For bodies run per row: deferred.
        f.body.foreach(checkStmt(_, deferred = true))
      case BbinStmt.WithStmt(w) =>
        // Provenance-field construction mirrors with_provenance fn bodies.
        w.provenance.foreach(f => checkFieldValue(f.value, "with provenance", deferred = true))
        w.body.foreach(checkStmt(_, deferred))
    }

    checkStmt(stmt, deferred = false)
    errors.result().toList
  }
}
