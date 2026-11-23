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
  *  - sealed → sealed OK (same row type)
  *  - sealed → `~` OK (unseal at import by copy — the imported binding is mutable)
  *  - `~` → `~` OK (same row type)
  *  - `~` → sealed REJECT (needs `.seal()`, never implicit)
  *  - row-type mismatch REJECT
  *  - missing binding / missing file / unparsable target REJECT
  *
  * File access is abstracted behind a `String => Option[String]` reader so
  * tests use an in-memory map and production passes a filesystem read.
  */
object BbinImportResolver {

  /** Check all imports of `importer`. Returns a (possibly empty) error list. */
  def resolveAndCheck(
    importerPath: String,
    importer: RawBbin,
    read: String => Option[String],
  ): List[String] = {
    importer.imports.flatMap { imp =>
      val resolved = resolveRel(importerPath, imp.from)
      read(resolved) match {
        case None =>
          List(s"import from rel:'${imp.from}': missing file '$resolved' (resolved against '$importerPath')")
        case Some(content) =>
          parseTarget(resolved, content) match {
            case Left(err) =>
              List(err)
            case Right(target) =>
              imp.names.flatMap { item =>
                target.statements.collectFirst {
                  case BbinStmt.LetStmt(l) if l.name == item.name => l
                } match {
                  case None =>
                    List(
                      s"import '${item.name}' from rel:'${imp.from}': missing binding '${item.name}' in '$resolved'"
                    )
                  case Some(exported) =>
                    item.ascribed match {
                      case None =>
                        Nil
                      case Some(ascribed) =>
                        if (isCompatible(exported.tpe, ascribed)) Nil
                        else
                          List(
                            s"import '${item.name}' from rel:'${imp.from}': incompatible ascription " +
                              s"${render(ascribed)} for declared ${render(exported.tpe)} of '${item.name}' in '$resolved'"
                          )
                    }
                }
              }
          }
      }
    }
  }

  /** Compatibility table: sealed→sealed OK, sealed→~ OK, ~→~ OK,
    * ~→sealed REJECT, anything else (row mismatch, set-shape mismatch) REJECT.
    */
  def isCompatible(actual: BbinType, ascribed: BbinType): Boolean =
    (actual, ascribed) match {
      case (BbinType.Plain(a), BbinType.Plain(b))     => a == b
      case (BbinType.Plain(a), BbinType.Partial(b))   => a == b
      case (BbinType.Partial(a), BbinType.Partial(b)) => a == b
      case (BbinType.SetOf(a), BbinType.SetOf(b))     => a == b
      case (
            BbinType.IndexedSet(i1, t1, e1),
            BbinType.IndexedSet(i2, t2, e2),
          ) =>
        i1 == i2 && t1 == t2 && e1 == e2
      case _ => false
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
