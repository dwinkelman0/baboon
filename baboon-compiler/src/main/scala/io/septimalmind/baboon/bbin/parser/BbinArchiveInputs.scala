package io.septimalmind.baboon.bbin.parser

import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.parser.model.FSPath
import io.septimalmind.baboon.util.{Utf8, ZipArchiveEntry, ZipReader}
import izumi.fundamentals.collections.nonempty.{NEList, NEString}

/** The parser inputs of a bbin archive: every `*.bbin` entry is a model;
  * `*.bmo` entries exist only to be included; directory entries are ignored;
  * any other entry is an error. Entry paths are kept verbatim, so diagnostics
  * name the archive path.
  */
final case class BbinArchiveInputs(models: NEList[BaboonParser.Input], includables: List[BaboonParser.Input]) {
  def all: List[BaboonParser.Input] = models.toList ++ includables
}

object BbinArchiveInputs {
  private val ModelExtension   = ".bbin"
  private val IncludeExtension = ".bmo"
  private val Separator        = "/"
  private val DrivePrefix      = "^[A-Za-z]:".r

  def fromZip(bytes: Array[Byte]): Either[NEList[String], BbinArchiveInputs] = {
    ZipReader.read(bytes).left.map(NEList(_)).flatMap(fromEntries)
  }

  def fromEntries(entries: List[ZipArchiveEntry]): Either[NEList[String], BbinArchiveInputs] = {
    val pathErrors = entries.flatMap(entry => validatePath(entry.path).map(error => s"unsafe archive entry path '${entry.path}': $error"))
    val duplicates = entries
      .groupBy(_.path.stripSuffix(Separator)).collect {
        case (path, sameName) if sameName.size > 1 => s"duplicate archive entry path '$path'"
      }.toList.sorted
    NEList.from(pathErrors ++ duplicates) match {
      case Some(errors) => Left(errors)
      case None         => classify(entries.filterNot(_.isDirectory))
    }
  }

  // paths are validated first: FSPath.parse assumes non-empty segments
  private def classify(files: List[ZipArchiveEntry]): Either[NEList[String], BbinArchiveInputs] = {
    val classified = files.map {
      entry =>
        if (entry.path.endsWith(ModelExtension) || entry.path.endsWith(IncludeExtension)) {
          Utf8
            .decodeStrict(entry.data)
            .left.map(_ => s"archive entry '${entry.path}' is not valid UTF-8")
            .map(content => (entry.path.endsWith(ModelExtension), BaboonParser.Input(FSPath.parse(NEString.unsafeFrom(entry.path)), content)))
        } else {
          Left(s"unsupported archive entry '${entry.path}': only *$ModelExtension schemas, *$IncludeExtension includes and directories are allowed")
        }
    }
    NEList.from(classified.collect { case Left(error) => error }) match {
      case Some(errors) => Left(errors)
      case None =>
        val inputs = classified.collect { case Right(input) => input }
        NEList.from(inputs.collect { case (true, input) => input }) match {
          case None         => Left(NEList(s"the archive contains no *$ModelExtension schema"))
          case Some(models) => Right(BbinArchiveInputs(models, inputs.collect { case (false, input) => input }))
        }
    }
  }

  private def validatePath(path: String): Option[String] = {
    val segments = path.stripSuffix(Separator).split(Separator, -1).toList
    if (path.isEmpty || path == Separator) Some("empty name")
    else if (path.startsWith(Separator)) Some("absolute path")
    else if (path.contains("\\")) Some("backslash separator")
    else if (path.contains("\u0000")) Some("NUL character")
    else if (DrivePrefix.findPrefixOf(path).nonEmpty) Some("drive prefix")
    else if (segments.exists(segment => segment.isEmpty || segment == "." || segment == "..")) Some("empty, '.' or '..' segment")
    else None
  }
}
