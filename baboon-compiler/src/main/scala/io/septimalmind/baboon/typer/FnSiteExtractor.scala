package io.septimalmind.baboon.typer

import io.septimalmind.baboon.parser.model._
import io.septimalmind.baboon.typer.model._

/** P1: extract fn sites from DTO members.
  *
  * Pure: needs no schema, only member/field names. Each extracted site keeps
  * the raw fn plus its role (`Default` / `Normalize` / `Migrate` from
  * `RawField`, `Computed` from `FnProperty`). Both fn homes flow through the
  * first-class `field.fn` accessor: `@default(fn)` initializers (on
  * `FieldDef`) and `= fn` computed initializers (on `FnProperty`).
  *
  * Defense-in-depth checks (the parser already enforces single-capture):
  *   - at most one capture per fn;
  *   - param names are pairwise distinct;
  *   - neither params nor top-level `let`s shadow the capture name.
  */
object FnSiteExtractor {

  /** Extract every fn site from one DTO's member list. `owner` is a
    * placeholder (dotted path or `"?"`) until the translator bridge lands.
    */
  def extract(owner: String, members: List[RawDtoMember]): Either[List[String], List[FnSite]] = {
    val sites = members.collect {
      case RawDtoMember.FieldDef(field, _) =>
        List(
          field.fn.map(fn => FnSite(FnSiteId(owner, field.name.name, FnRole.Default), fn)),
          field.normalize.map(fn => FnSite(FnSiteId(owner, field.name.name, FnRole.Normalize), fn)),
          field.migrate.map(m => FnSite(FnSiteId(owner, field.name.name, FnRole.Migrate), m.fn)),
        ).flatten
      case RawDtoMember.FnProperty(field, _) =>
        // Single source of truth: computed fns live in `field.fn`
        // (the parser no longer dual-encodes them into `field.default`).
        field.fn.map(fn => FnSite(FnSiteId(owner, field.name.name, FnRole.Computed), fn)).toList
    }.flatten
    val errors = sites.flatMap(s => check(s.rawFn).left.toOption.toList.flatten)
    if (errors.nonEmpty) Left(errors) else Right(sites)
  }

  /** Check one fn head + top-level lets. */
  def check(fn: RawFn): Either[List[String], Unit] = {
    val errs = List.newBuilder[String]

    if (fn.captures.size > 1)
      errs += s"fn has ${fn.captures.size} captures, at most one is allowed"

    val dupParams = fn.params.groupBy(_.name).collect { case (n, l) if l.size > 1 => n }
    dupParams.foreach(n => errs += s"duplicate param name: $n")

    fn.captures.headOption.foreach { cap =>
      if (fn.params.exists(_.name == cap.name))
        errs += s"param shadows capture name: ${cap.name}"
      val topLets = fn.body.stmts.collect { case BodyStmt.Let(n, _, _) => n }
      if (topLets.contains(cap.name))
        errs += s"top-level let shadows capture name: ${cap.name}"
    }

    val out = errs.result()
    if (out.nonEmpty) Left(out) else Right(())
  }
}
