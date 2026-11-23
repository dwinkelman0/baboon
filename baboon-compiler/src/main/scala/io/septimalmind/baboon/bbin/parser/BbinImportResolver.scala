package io.septimalmind.baboon.bbin.parser

import fastparse.*
import io.septimalmind.baboon.bbin.model.*
import io.septimalmind.baboon.parser.ParserContext
import io.septimalmind.baboon.parser.model.FSPath
import izumi.fundamentals.collections.nonempty.NEString

/** AST-level import correctness against the referenced `.bbin` file.
  *
  * Parse-level only — no typer/IR work. For each `import (name [as alias]
  * [: Type]) from rel:'...'` entry this resolves `rel:` against the
  * importing file's directory, loads + parses the referenced `.bbin` with
  * the same [[DefBbin]] grammar, finds the exported `let` by name, and
  * checks the ascribed [[BbinType]] against the declared type per the
  * compatibility table:
  *
  *  - no ascription → identical type passthrough ([[Coercion.Same]])
  *  - sealed → sealed OK, same row type ([[Coercion.Same]])
  *  - sealed → `~` OK, unseal at import by copy ([[Coercion.Unseal]])
  *  - `~` → `~` OK ([[Coercion.Same]])
  *  - `~` → sealed OK here ([[Coercion.Seal]]); completeness enforced later
  *    at seal planning/runtime, never rejected here
  *  - row-type mismatch REJECT
  *  - missing binding / missing file / unparsable target REJECT
  *
  * File access is abstracted behind a `String => Option[String]` reader so
  * tests use an in-memory map and production passes a filesystem read.
  */
object BbinImportResolver {

  /** Seal variance of a resolved import ascription.
    *
    *  - [[Coercion.Same]]: no ascription (passthrough) or ascribed type is
    *    identical to the declared type.
    *  - [[Coercion.Unseal]]: sealed declaration ascribed `~` (implicit unseal
    *    by copy, always valid).
    *  - [[Coercion.Seal]]: `~` declaration ascribed sealed (implicit seal;
    *    accepted here, completeness enforced later at seal planning/runtime).
    */
  sealed trait Coercion

  object Coercion {
    case object Same extends Coercion
    case object Unseal extends Coercion
    case object Seal extends Coercion
  }

  /** A successfully resolved import: the declared type in the target file,
    * the effective (ascribed or passthrough) type, and the coercion between
    * them.
    */
  final case class ResolvedImport(
    name: String,
    alias: Option[String],
    declared: BbinType,
    resolvedType: BbinType,
    coercion: Coercion,
    from: String,
    targetFile: String,
  )

  /** Resolve all imports of `importer`, returning per-import results alongside
    * a (possibly empty) error list. Errors cover missing files, unparsable
    * targets, missing bindings and incompatible ascriptions.
    */
  def resolve(
    importerPath: String,
    importer: RawBbin,
    read: String => Option[String],
  ): (List[ResolvedImport], List[String]) = {
    val (resolved, errors) = importer.imports.map { imp =>
      val targetFile = resolveRel(importerPath, imp.from)
      read(targetFile) match {
        case None =>
          (
            Nil,
            List(
              s"import from rel:'${imp.from}': missing file '$targetFile' (resolved against '$importerPath')"
            )
          )
        case Some(content) =>
          parseTarget(targetFile, content) match {
            case Left(err) =>
              (Nil, List(err))
            case Right(target) =>
              imp.names.foldLeft((Nil: List[ResolvedImport], Nil: List[String])) {
                case ((ok, errs), item) =>
                  target.statements.collectFirst {
                    case BbinStmt.LetStmt(l) if l.name == item.name => l
                  } match {
                    case None =>
                      (
                        ok,
                        errs :+ s"import '${item.name}' from rel:'${imp.from}': missing binding '${item.name}' in '$targetFile'"
                      )
                    case Some(exported) =>
                      item.ascribed match {
                        case None =>
                          (
                            ok :+ ResolvedImport(
                              item.name,
                              item.alias,
                              exported.tpe,
                              exported.tpe,
                              Coercion.Same,
                              imp.from,
                              targetFile,
                            ),
                            errs
                          )
                        case Some(ascribed) =>
                          coercionFor(exported.tpe, ascribed) match {
                            case Some(coercion) =>
                              (
                                ok :+ ResolvedImport(
                                  item.name,
                                  item.alias,
                                  exported.tpe,
                                  ascribed,
                                  coercion,
                                  imp.from,
                                  targetFile,
                                ),
                                errs
                              )
                            case None =>
                              (
                                ok,
                                errs :+ (
                                  s"import '${item.name}' from rel:'${imp.from}': incompatible ascription " +
                                    s"${render(ascribed)} for declared ${render(exported.tpe)} of '${item.name}' in '$targetFile'"
                                )
                              )
                          }
                      }
                  }
              }
          }
      }
    }.unzip
    (resolved.flatten, errors.flatten)
  }

  /** Check all imports of `importer`. Returns a (possibly empty) error list. */
  def resolveAndCheck(
    importerPath: String,
    importer: RawBbin,
    read: String => Option[String],
  ): List[String] =
    resolve(importerPath, importer, read)._2

  /** Compatibility table: sealed→sealed OK, sealed→~ OK (unseal),
    * ~→~ OK, ~→sealed OK (seal; completeness enforced later),
    * anything else (row mismatch, set-shape mismatch) REJECT.
    */
  def isCompatible(actual: BbinType, ascribed: BbinType): Boolean =
    coercionFor(actual, ascribed).isDefined

  /** Coercion between a declared type and its ascription, or `None` when the
    * ascription is incompatible. Variance applies only to the `~` marker on
    * the same row type; set shapes require exact equality.
    */
  def coercionFor(actual: BbinType, ascribed: BbinType): Option[Coercion] =
    (actual, ascribed) match {
      case (BbinType.Plain(a), BbinType.Plain(b)) if a == b       => Some(Coercion.Same)
      case (BbinType.Plain(a), BbinType.Partial(b)) if a == b     => Some(Coercion.Unseal)
      case (BbinType.Partial(a), BbinType.Partial(b)) if a == b   => Some(Coercion.Same)
      case (BbinType.Partial(a), BbinType.Plain(b)) if a == b     => Some(Coercion.Seal)
      case (BbinType.SetOf(a), BbinType.SetOf(b)) if a == b       => Some(Coercion.Same)
      case (
            BbinType.IndexedSet(i1, t1, e1),
            BbinType.IndexedSet(i2, t2, e2),
          ) if i1 == i2 && t1 == t2 && e1 == e2 =>
        Some(Coercion.Same)
      case _ => None
    }

  /** Resolve a `rel:` target against the importing file's directory. */
  def resolveRel(importerPath: String, from: String): String = {
    val baseDir =
      importerPath.lastIndexOf('/') match {
        case -1 => ""
        case i  => importerPath.substring(0, i)
      }
    val combined =
      if (baseDir.isEmpty) from
      else if (from.startsWith("/")) from
      else baseDir + "/" + from
    normalize(combined)
  }

  private def normalize(path: String): String = {
    val absolute = path.startsWith("/")
    val parts    = path.split("/").toList
    val out = parts.foldLeft(List.empty[String]) {
      case (acc, "" | ".")  => acc
      case (Nil, "..")      => Nil
      case (acc, "..")      => acc.dropRight(1)
      case (acc, seg)       => acc :+ seg
    }
    val joined = out.mkString("/")
    if (absolute) "/" + joined else joined
  }

  private def parseTarget(resolved: String, content: String): Either[String, RawBbin] = {
    val fsPath =
      try FSPath.parse(NEString.unsafeFrom(if (resolved.isEmpty) "target.bbin" else resolved))
      catch { case _: Throwable => FSPath.parse(NEString.unsafeFrom("target.bbin")) }
    val context = ParserContext(fsPath, content)
    val defn    = new DefBbin(context)
    fastparse.parse(content, defn.bbinFile(_)) match {
      case Parsed.Success((usings, imports, stmts), idx) if idx == content.length =>
        Right(RawBbin(fsPath, usings, imports, stmts))
      case Parsed.Success(_, idx) =>
        Left(s"import target '$resolved': parse left unconsumed input at offset $idx")
      case f: Parsed.Failure =>
        Left(s"import target '$resolved': parse failed: ${f.msg}")
    }
  }

  private def render(tpe: BbinType): String = tpe match {
    case BbinType.Plain(ref)                 => ref.render
    case BbinType.Partial(ref)               => s"~${ref.render}"
    case BbinType.SetOf(elem)                => s"{${elem.render}}"
    case BbinType.IndexedSet(idx, ti, elem)  => s"{$idx: ${ti.render} > ${elem.render}}"
  }
}
