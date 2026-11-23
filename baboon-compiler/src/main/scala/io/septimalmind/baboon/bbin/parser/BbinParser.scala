package io.septimalmind.baboon.bbin.parser

import fastparse.Parsed
import io.septimalmind.baboon.bbin.model.RawBbin
import io.septimalmind.baboon.parser.{BaboonParser, ParserContext}
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, ParserIssue}
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.NEList

trait BbinParser[F[+_, +_]] {
  def parse(input: BaboonParser.Input): F[NEList[BaboonIssue], RawBbin]
}

object BbinParser {
}

class BbinParserImpl[F[+_, +_]: Error2] extends BbinParser[F] {
  override def parse(input: BaboonParser.Input): F[NEList[BaboonIssue], RawBbin] = {
    val context = ParserContext(input.path, input.content)
    val defn    = new DefBbin(context)

    fastparse.parse(input.content, defn.bbinFile(_)) match {
      case Parsed.Success((usings, imports, statements), _) =>
        F.pure(RawBbin(input.path, usings, imports, statements))

      case failure: Parsed.Failure =>
        F.fail(NEList(ParserIssue.ParserFailed(failure, input.path)))
    }
  }
}
