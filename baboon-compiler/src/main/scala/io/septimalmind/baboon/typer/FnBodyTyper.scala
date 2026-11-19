package io.septimalmind.baboon.typer

import io.septimalmind.baboon.parser.model._
import io.septimalmind.baboon.typer.model._
import FnTypes._

/** P3: type a fn body. Inputs (never inferred):
  * schema / capture / param / annotated-`let` types. Only operator and
  * method-call *results* are inferred, via closed tables.
  *
  * Rules:
  *   - every `let` needs an explicit `: T` annotation (missing => error);
  *   - assignment targets must resolve to a known slot; values must conform;
  *   - `for-in` iterables must be `lst`/`set`/`map`; the binder takes the
  *     element type (`lst/set -> T`, `map -> K`);
  *   - `for-C` init binds its (optionally annotated) value type, cond must be
  *     `bit`, step must be an assignment;
  *   - `match` scrutinees must be an ADT (or `opt[T]` for `Some`/`None`);
  *     unknown variants are errors; all arms must agree on the result type;
  *   - `if` conds must be `bit`; branches join;
  *   - `assert(cond[, msg])` conds must be `bit`, `msg` must be `str`;
  *     asserts are `void` side-conditions and never affect the result type;
  *   - the final result must conform to the expected (field) type.
  */
object FnBodyTyper {

  /** Unit type for statement-position nodes without a value (`void`).
    * Previously `bit` was reused as a placeholder; `void` is now explicit.
    */
  val unitTpe: TypeRef = TypeRef.Scalar(TypeId.Builtins.void)

  /** Seed a fn scope from params plus P2 capture bindings. */
  def seedScope(
    params: List[(String, TypeRef)],
    capBindings: List[(String, ScopeEntry)],
  ): ScopeTable =
    ScopeTable(
      params.map { case (n, t) => n -> (ScopeEntry.Param(t): ScopeEntry) }.toMap ++
        capBindings.toMap,
    )

  def typeBody(
    raw: RawBody,
    scope: ScopeTable,
    schema: FnSchema,
    expected: TypeRef,
  ): Res[TypedBody] =
    for {
      typed <- typeStmts(raw.stmts, scope, schema)
      (stmts, finalScope) = typed
      result <- typeExpr(raw.result, finalScope, schema)
      _ <-
        if (conformsLit(result, expected)) ok(())
        else fail[Unit](s"body result type ${result.tpe} does not conform to expected $expected")
    } yield TypedBody(stmts, result, result.tpe, collectAsserts(stmts))

  // -- statements -----------------------------------------------------------

  private def typeStmts(
    stmts: List[BodyStmt],
    scope: ScopeTable,
    schema: FnSchema,
  ): Res[(List[TypedStmt], ScopeTable)] = {
    val init: Res[(List[TypedStmt], ScopeTable)] = ok((Nil, scope))
    val out = stmts.foldLeft(init) { (accR, s) =>
      accR.flatMap { case (typed, sc) =>
        typeStmt(s, sc, schema).map { case (t, sc2) => (typed :+ t, sc2) }
      }
    }
    out
  }

  private def typeStmt(
    stmt: BodyStmt,
    scope: ScopeTable,
    schema: FnSchema,
  ): Res[(TypedStmt, ScopeTable)] = stmt match {
    case BodyStmt.Let(name, tpeOpt, value) =>
      tpeOpt match {
        case None =>
          fail(s"let `$name` needs an explicit type annotation")
        case Some(rawT) =>
          for {
            ann <- resolveRaw(rawT, schema).left.map(_.map(m => s"let `$name`: $m"))
            v   <- typeExpr(value, scope, schema)
            _ <-
              if (conformsLit(v, ann)) ok(())
              else fail[Unit](s"let `$name`: value type ${v.tpe} does not conform to $ann")
          } yield (TypedStmt.Let(name, ann, v), scope + (name -> ScopeEntry.LetBinding(ann)))
      }
    case BodyStmt.Effect(expr) =>
      // `for` binders never escape into the enclosing scope: they only live
      // inside their bodies (handled in `typeExpr`).
      typeExpr(expr, scope, schema).map(t => (TypedStmt.Effect(t), scope))
    case BodyStmt.Assert(cond, msg) =>
      for {
        c <- typeExpr(cond, scope, schema)
        _ <-
          if (conformsLit(c, scalar("bit"))) ok(())
          else fail[Unit](s"assert condition must be bit, got ${c.tpe}")
        m <- traverse(msg.toList.map(typeExpr(_, scope, schema)))
        _ <- m.headOption match {
          case None => ok(())
          case Some(mm) =>
            if (conformsLit(mm, scalar("str"))) ok(())
            else fail[Unit](s"assert message must be str, got ${mm.tpe}")
        }
      } yield {
        val typed = TypedAssert(c, m.headOption, collectAssertVars(c), None)
        (TypedStmt.Assert(typed), scope)
      }
  }

  /** Distinct `Path` roots involved in an assert condition, in
    * first-occurrence order, for backend trace metadata. Walks the *typed*
    * condition so only successfully-resolved roots are recorded.
    */
  def collectAssertVars(cond: TypedExpr): List[String] = {
    def go(e: TypedExpr, acc: List[String]): List[String] = e match {
      case TypedExpr.Path(root, segs, _) =>
        val fromSegs = segs.collect { case TypedPathSeg.Index(idx, _) => idx }.foldLeft(acc)((a, idx) => go(idx, a))
        if (fromSegs.contains(root)) fromSegs else fromSegs :+ root
      case TypedExpr.Call(t, _, args, _)  => args.foldLeft(go(t, acc))((a, x) => go(x, a))
      case TypedExpr.BinOp(_, l, r, _)    => go(r, go(l, acc))
      case TypedExpr.UnaryOp(_, x, _)     => go(x, acc)
      case TypedExpr.If(c, t, e, _)       => go(e, go(t, go(c, acc)))
      case TypedExpr.Assign(t, v, _)      => go(v, go(t, acc))
      case TypedExpr.ForIn(_, it, b, _)   => goBlock(b, go(it, acc))
      case TypedExpr.ForC(init, c, s, b, _) =>
        goBlock(b, go(s, go(c, go(init.value, acc))))
      case TypedExpr.Match(s, arms, _) =>
        arms.foldLeft(go(s, acc))((a, arm) => goBlock(arm.body, a))
      case TypedExpr.Block(stmts, v, _) =>
        val a2 = stmts.foldLeft(acc) {
          case (a, TypedStmt.Let(_, _, x))  => go(x, a)
          case (a, TypedStmt.Effect(x))    => go(x, a)
          case (a, TypedStmt.Assert(asrt)) =>
            val aCond = go(asrt.cond, a)
            asrt.msg.foldLeft(aCond)((x, mm) => go(mm, x))
        }
        v.foldLeft(a2)((a, x) => go(x, a))
      case TypedExpr.Lit(_, _) => acc
    }
    def goBlock(b: TypedExpr.Block, acc: List[String]): List[String] = go(b, acc)
    go(cond, Nil)
  }

  /** Every assertion in a typed statement list, including ones nested in
    * blocks, loops and match arms, in source order.
    */
  def collectAsserts(stmts: List[TypedStmt]): List[TypedAssert] = {
    def fromExpr(e: TypedExpr): List[TypedAssert] = e match {
      case TypedExpr.ForIn(_, _, b, _)    => fromBlock(b)
      case TypedExpr.ForC(_, _, _, b, _)  => fromBlock(b)
      case TypedExpr.Match(_, arms, _)    => arms.flatMap(a => fromBlock(a.body))
      case TypedExpr.Block(ss, _, _)      => fromStmts(ss)
      case TypedExpr.If(_, t, e, _)       => fromExpr(t) ++ fromExpr(e)
      case TypedExpr.BinOp(_, l, r, _)    => fromExpr(l) ++ fromExpr(r)
      case TypedExpr.UnaryOp(_, x, _)     => fromExpr(x)
      case TypedExpr.Call(t, _, args, _)  => fromExpr(t) ++ args.flatMap(fromExpr)
      case TypedExpr.Assign(t, v, _)      => fromExpr(t) ++ fromExpr(v)
      case _                              => Nil
    }
    def fromBlock(b: TypedExpr.Block): List[TypedAssert] =
      fromStmts(b.stmts) ++ b.value.toList.flatMap(fromExpr)
    def fromStmts(ss: List[TypedStmt]): List[TypedAssert] = ss.flatMap {
      case TypedStmt.Assert(a)  => List(a) ++ fromExpr(a.cond) ++ a.msg.toList.flatMap(fromExpr)
      case TypedStmt.Let(_, _, v) => fromExpr(v)
      case TypedStmt.Effect(e)  => fromExpr(e)
    }
    fromStmts(stmts)
  }

  // -- expressions ----------------------------------------------------------

  def typeExpr(raw: BodyExpr, scope: ScopeTable, schema: FnSchema): Res[TypedExpr] = raw match {
    case BodyExpr.Lit(lit) => ok(typeLit(lit))

    case BodyExpr.Path(root, segments) =>
      scope.get(root).toRight(List(s"unknown path root: $root")).flatMap { entry =>
        typePathSegs(segments, entry.tpe, scope, schema).map { segs =>
          val tpe = segs.lastOption.map {
            case TypedPathSeg.Field(_, t) => t
            case TypedPathSeg.Index(_, t) => t
          }.getOrElse(entry.tpe)
          TypedExpr.Path(root, segs, tpe)
        }
      }

    case BodyExpr.Call(target, method, args) =>
      for {
        t   <- typeExpr(target, scope, schema)
        as  <- traverse(args.map(typeExpr(_, scope, schema)))
        sig <- methodSig(method).toRight(List(s"unknown method: $method"))
        out <- checkCall(t, sig, method, as)
      } yield TypedExpr.Call(t, method, as, out)

    case BodyExpr.BinOp(op, l, r) =>
      for {
        lt <- typeExpr(l, scope, schema)
        rt <- typeExpr(r, scope, schema)
        t  <- typeBinOp(op, lt, rt)
      } yield TypedExpr.BinOp(op, lt, rt, t)

    case BodyExpr.UnaryOp(op, e) =>
      typeExpr(e, scope, schema).flatMap { t =>
        op match {
          case "!" if isBool(t.tpe)     => ok(TypedExpr.UnaryOp(op, t, t.tpe))
          case "!"                      => fail(s"unary `!` needs bit, got ${t.tpe}")
          case "-" if isNumeric(t.tpe)  => ok(TypedExpr.UnaryOp(op, t, t.tpe))
          case "-"                      => fail(s"unary `-` needs a numeric type, got ${t.tpe}")
          case _                        => fail(s"unknown unary operator: $op")
        }
      }

    case BodyExpr.If(cond, thenE, elseE) =>
      for {
        c <- typeExpr(cond, scope, schema)
        _ <-
          if (conformsLit(c, scalar("bit"))) ok(())
          else fail[Unit](s"if condition must be bit, got ${c.tpe}")
        t <- typeExpr(thenE, scope, schema)
        e <- typeExpr(elseE, scope, schema)
        j <- joinTypes(t, e, "if branches disagree")
      } yield TypedExpr.If(c, t, e, j)

    case BodyExpr.Assign(target, value) =>
      for {
        tt <- typeLvalue(target, scope, schema)
        v  <- typeExpr(value, scope, schema)
        _ <-
          if (conformsLit(v, tt.tpe)) ok(())
          else fail[Unit](s"assign type mismatch: value ${v.tpe} does not conform to ${tt.tpe}")
      } yield TypedExpr.Assign(tt, v, v.tpe)

    case BodyExpr.ForIn(varName, iterable, body) =>
      for {
        it <- typeExpr(iterable, scope, schema)
        bt <- binderType(it.tpe).toRight(List(s"for-in over non-collection: ${it.tpe}"))
        tb <- typeBlock(body, scope + (varName -> ScopeEntry.LetBinding(bt)), schema)
      } yield TypedExpr.ForIn(varName, it, tb, unitTpe)

    case BodyExpr.ForC(init, cond, step, body) =>
      for {
        iv <- typeExpr(init.value, scope, schema)
        it <- init.tpe match {
          case Some(rawT) =>
            resolveRaw(rawT, schema).left.map(_.map(m => s"for-init `${init.name}`: $m")).flatMap { ann =>
              if (conformsLit(iv, ann)) ok(ann)
              else fail[TypeRef](s"for-init `${init.name}`: value type ${iv.tpe} does not conform to $ann")
            }
          case None => ok(iv.tpe)
        }
        sc1 = scope + (init.name -> ScopeEntry.LetBinding(it))
        c  <- typeExpr(cond, sc1, schema)
        _ <-
          if (conformsLit(c, scalar("bit"))) ok(())
          else fail[Unit](s"for condition must be bit, got ${c.tpe}")
        s <- typeExpr(step, sc1, schema).flatMap {
          case a: TypedExpr.Assign => ok(a: TypedExpr)
          case other               => fail[TypedExpr](s"for step must be an assignment, got ${other.getClass.getSimpleName}")
        }
        tb <- typeBlock(body, sc1, schema)
      } yield TypedExpr.ForC(TypedForInit(init.name, it, iv), c, s, tb, unitTpe)

    case BodyExpr.Match(scrutinee, arms) =>
      for {
        s <- typeExpr(scrutinee, scope, schema)
        table <- matchTable(s.tpe, schema)
        _ <-
          if (arms.nonEmpty) ok(())
          else fail[Unit]("match needs at least one arm")
        typedArms <- traverse(arms.map(a => typeArm(a, s.tpe, table, scope, schema)))
        armTypes = typedArms.map(_.body.tpe)
        j <- armTypes.tail.foldLeft(ok(armTypes.head): Res[TypeRef]) { (accR, t) =>
          accR.flatMap(a => joinRefs(a, t, "match arms disagree"))
        }
      } yield TypedExpr.Match(s, typedArms, j)

    case b: BodyExpr.Block =>
      typeBlock(b, scope, schema)
  }

  private def typeBlock(raw: BodyExpr.Block, scope: ScopeTable, schema: FnSchema): Res[TypedExpr.Block] =
    for {
      typed <- typeStmts(raw.stmts, scope, schema)
      (stmts, sc2) = typed
      v <- traverse(raw.value.toList.map(typeExpr(_, sc2, schema)))
    } yield TypedExpr.Block(stmts, v.headOption, v.headOption.map(_.tpe).getOrElse(unitTpe))

  // -- paths ----------------------------------------------------------------

  private def typePathSegs(
    segs: List[BodyExpr.Segment],
    start: TypeRef,
    scope: ScopeTable,
    schema: FnSchema,
  ): Res[List[TypedPathSeg]] = {
    val init: Res[(List[TypedPathSeg], TypeRef)] = ok((Nil, start))
    segs.foldLeft(init) { (accR, seg) =>
      accR.flatMap { case (out, cur) =>
        seg match {
          case BodyExpr.Segment.Variant(name) =>
            fail(s"variant segment `$name` is only allowed in match patterns")
          case BodyExpr.Segment.Field(name) =>
            descendBodyField(name, cur, schema).map { case (t, _) =>
              (out :+ TypedPathSeg.Field(name, t), t)
            }
          case BodyExpr.Segment.Index(idx) =>
            for {
              it <- typeExpr(idx, scope, schema)
              t  <- indexInto(cur, it)
            } yield (out :+ TypedPathSeg.Index(it, t), t)
        }
      }
    }.map(_._1)
  }

  /** Body field traversal: `opt` is transparent (explicit `.Some` situates
    * into the payload); `lst`/`set`/`map` stay intact as values unless a
    * further `.field` descends into the element type (capture-path parity).
    */
  private def descendBodyField(name: String, cur: TypeRef, schema: FnSchema): Res[(TypeRef, String)] = {
    cur match {
      case t if isOpt(t) && name == "Some" =>
        elemType(t).toRight(List(s"cannot situate into $t")).map(next => (next, ownerName(t)))
      case t if isOpt(t) =>
        elemType(t).toRight(List(s"cannot situate into $t")).flatMap(et => descendBodyField(name, et, schema))
      case t if isLst(t) || isSet(t) || isMap(t) =>
        // Collection-valued field selected as a whole stays a collection;
        // a `.field` chained onto it descends into the element type.
        elemType(t).toRight(List(s"cannot situate into $t")).flatMap { et =>
          if (et == t) fail(s"no field `$name` on $t")
          else descendBodyField(name, et, schema).orElse(collectionAsValue(name, t))
        }
      case t =>
        dtoFields(t, schema) match {
          case Some(fields) =>
            fields.find(_.name.name == name) match {
              case Some(f) => ok((f.tpe, ownerName(t)))
              case None    => fail(s"unknown field `$name` on $t")
            }
          case None => fail(s"no fields on $t (field `$name`)")
        }
    }
  }

  /** Fallback: the collection itself is the selected value (used only when
    * element descent fails, so bare `self.map` keeps its `map` type).
    * Implemented by the caller treating resolution failure as "keep".
    */
  private def collectionAsValue(name: String, t: TypeRef): Res[(TypeRef, String)] =
    fail(s"unknown field `$name` on $t")

  private def indexInto(cur: TypeRef, idx: TypedExpr): Res[TypeRef] = cur match {
    case t if isLst(t) || isSet(t) =>
      if (conformsLit(idx, scalar("i32"))) elemType(t).toRight(List(s"cannot index into $t"))
      else fail(s"collection index must be i32, got ${idx.tpe}")
    case t if isMap(t) =>
      collArgs(t).headOption.toRight(List(s"cannot index into $t")).flatMap { k =>
        if (conformsLit(idx, k)) elemType(t).toRight(List(s"cannot index into $t"))
        else fail(s"map index must conform to $k, got ${idx.tpe}")
      }
    case other =>
      fail(s"cannot index into non-collection $other")
  }

  /** Lvalue paths: same resolution as value paths, but a bare root must be
    * a local (`let` / binder), never a capture root, param or star chain.
    */
  private def typeLvalue(target: BodyExpr, scope: ScopeTable, schema: FnSchema): Res[TypedExpr] =
    target match {
      case BodyExpr.Path(root, Nil) =>
        scope.get(root).toRight(List(s"unknown assign target: $root")).flatMap {
          case ScopeEntry.LetBinding(t) => ok(TypedExpr.Path(root, Nil, t): TypedExpr)
          case ScopeEntry.CaptureRoot(_) => fail(s"cannot assign to capture root: $root")
          case ScopeEntry.Param(_)       => fail(s"cannot assign to param: $root")
          case ScopeEntry.StarChain(_)   => fail(s"cannot assign to star chain: $root")
        }
      case p: BodyExpr.Path =>
        typeExpr(p, scope, schema)
      case other =>
        fail(s"assign target must be a path, got ${other.getClass.getSimpleName}")
    }

  // -- calls ------------------------------------------------------------------

  private def checkCall(
    target: TypedExpr,
    sig: MethodSig,
    method: String,
    args: List[TypedExpr],
  ): Res[TypeRef] = {
    import MethodSig._
    if (args.size != sig.arity)
      return fail(s"method `$method` takes ${sig.arity} args, got ${args.size}")
    sig match {
      case Str0(result) =>
        if (!isStr(target.tpe)) fail(s"method `$method` needs a str receiver, got ${target.tpe}")
        else ok(result)
      case Length =>
        if (!isStr(target.tpe)) fail(s"method `length` needs a str receiver, got ${target.tpe}")
        else ok(scalar("i32"))
      case Str1(result) =>
        if (!isStr(target.tpe)) fail(s"method `$method` needs a str receiver, got ${target.tpe}")
        else if (!conformsLit(args.head, scalar("str"))) fail(s"method `$method` arg must be str, got ${args.head.tpe}")
        else ok(result)
      case Join =>
        if (target.tpe != ctor(TypeId.Builtins.lst, List(scalar("str"))))
          fail(s"method `join` needs a lst[str] receiver, got ${target.tpe}")
        else if (!conformsLit(args.head, scalar("str"))) fail(s"method `join` arg must be str, got ${args.head.tpe}")
        else ok(scalar("str"))
      case Num0 =>
        if (!isNumeric(target.tpe)) fail(s"method `$method` needs a numeric receiver, got ${target.tpe}")
        else ok(target.tpe)
      case Num1 =>
        if (!isNumeric(target.tpe)) fail(s"method `$method` needs a numeric receiver, got ${target.tpe}")
        else if (!conformsLit(args.head, target.tpe)) fail(s"method `$method` arg must conform to ${target.tpe}, got ${args.head.tpe}")
        else ok(target.tpe)
    }
  }

  // -- operators ---------------------------------------------------------------

  private def typeBinOp(op: String, l: TypedExpr, r: TypedExpr): Res[TypeRef] = op match {
    case "==" | "!=" => ok(scalar("bit"))
    case "&&" | "||" =>
      if (!isBool(l.tpe)) fail(s"`$op` needs bit operands, got ${l.tpe}")
      else if (!isBool(r.tpe)) fail(s"`$op` needs bit operands, got ${r.tpe}")
      else ok(scalar("bit"))
    case "<" | "<=" | ">" | ">=" =>
      numericJoin(l.tpe, r.tpe) match {
        case Some(_) => ok(scalar("bit"))
        case None    => fail(s"`$op` needs numeric operands, got ${l.tpe} and ${r.tpe}")
      }
    case "+" | "-" | "*" | "/" | "%" =>
      numericJoin(l.tpe, r.tpe)
        .toRight(List(s"`$op` needs numeric operands, got ${l.tpe} and ${r.tpe}"))
    case _ => fail(s"unknown binary operator: $op")
  }

  private def joinTypes(a: TypedExpr, b: TypedExpr, what: String): Res[TypeRef] =
    joinRefs(a.tpe, b.tpe, what, Some((a, b)))

  private def joinRefs(a: TypeRef, b: TypeRef, what: String, exprs: Option[(TypedExpr, TypedExpr)] = None): Res[TypeRef] =
    if (a == b) ok(a)
    else
      exprs match {
        case Some((x, _)) if conformsLit(x, b) => ok(b)
        case Some((_, y)) if conformsLit(y, a) => ok(a)
        case _ =>
          if (conforms(a, actualIsIntLit = false, actualIsFloatLit = false, actualIsNull = false, b)) ok(b)
          else if (conforms(b, actualIsIntLit = false, actualIsFloatLit = false, actualIsNull = false, a)) ok(a)
          else fail(s"$what: $a vs $b")
      }

  // -- match ---------------------------------------------------------------

  /** `for-in` binder type: `lst[T]/set[T] -> T`, `map[K, V] -> K`. */
  private def binderType(t: TypeRef): Option[TypeRef] = t match {
    case x if isLst(x) || isSet(x) => elemType(x)
    case x if isMap(x)             => keyType(x)
    case _                         => None
  }

  private def matchTable(scrut: TypeRef, schema: FnSchema): Res[List[FnAdtVariant]] = scrut match {
    case t if isOpt(t) =>
      elemType(t).toRight(List(s"cannot match on $t")).map { et =>
        List(FnAdtVariant("Some", Some(et)), FnAdtVariant("None", None))
      }
    case TypeRef.Scalar(id: TypeId.User) =>
      schema.adts.get(id.name.name).toRight(List(s"match on non-ADT type: ${id.name.name}"))
    case other =>
      fail(s"match on non-ADT type: $other")
  }

  private def typeArm(
    arm: MatchArm,
    scrutTpe: TypeRef,
    table: List[FnAdtVariant],
    scope: ScopeTable,
    schema: FnSchema,
  ): Res[TypedMatchArm] = arm.pattern match {
    case MatchPattern.Wildcard =>
      typeBlock(arm.body, scope, schema).map(b => TypedMatchArm(TypedPattern.Wildcard, b))
    case MatchPattern.Variant(name, binding) =>
      table.find(_.name == name).toRight(List(s"match: unknown variant `$name` for $scrutTpe")).flatMap { v =>
        val scR: Res[ScopeTable] = (v.payload, binding) match {
          case (Some(p), Some(b)) => ok(scope + (b -> ScopeEntry.LetBinding(p)))
          case (Some(_), None)    => ok(scope)
          case (None, None)       => ok(scope)
          case (None, Some(b))    => fail[ScopeTable](s"match: variant `$name` carries no payload (binding `$b`)")
        }
        scR.flatMap(sc => typeBlock(arm.body, sc, schema).map(b => TypedMatchArm(TypedPattern.Variant(name, binding, v.payload), b)))
      }
  }

  // -- misc -----------------------------------------------------------------

  private def typeLit(lit: BodyLiteral): TypedExpr.Lit = lit match {
    case BodyLiteral.IntLit(v)   => TypedExpr.Lit(TypedLit.IntLit(v), TypedLit.IntLit(v).tpe)
    case BodyLiteral.FloatLit(v) => TypedExpr.Lit(TypedLit.FloatLit(v), TypedLit.FloatLit(v).tpe)
    case BodyLiteral.BoolLit(v)  => TypedExpr.Lit(TypedLit.BoolLit(v), TypedLit.BoolLit(v).tpe)
    case BodyLiteral.StrLit(v)   => TypedExpr.Lit(TypedLit.StrLit(v), TypedLit.StrLit(v).tpe)
    case BodyLiteral.NullLit     => TypedExpr.Lit(TypedLit.NullLit(), TypedLit.NullLit().tpe)
  }

  private def ownerName(t: TypeRef): String = t match {
    case TypeRef.Scalar(id: TypeId.User) => id.name.name
    case other                           => other.toString
  }
}
