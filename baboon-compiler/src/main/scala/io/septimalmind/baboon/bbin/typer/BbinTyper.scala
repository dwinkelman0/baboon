package io.septimalmind.baboon.bbin.typer

import io.septimalmind.baboon.bbin.model.RawBbin
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.typer.model.Domain
import izumi.functional.bio.Error2
import izumi.fundamentals.collections.nonempty.NEList

final case class BbinProgram(bbin: RawBbin)

trait BbinTyper[F[+_, +_]] {
  def process(schema: Domain, bbin: RawBbin): F[NEList[BaboonIssue], BbinProgram]
}

object BbinTyper {
}

class BbinTyperImpl[F[+_, +_]: Error2] extends BbinTyper[F] {
  override def process(schema: Domain, bbin: RawBbin): F[NEList[BaboonIssue], BbinProgram] = ???
}
