package io.septimalmind.baboon.bbin.translator

import io.septimalmind.baboon.bbin.typer.BbinProgram
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import izumi.functional.bio.Error2
import izumi.fundamentals.collections.nonempty.NEList

trait BbinLowering[F[+_, +_]] {
  def lower(program: BbinProgram): F[NEList[BaboonIssue], List[String]]
}

object BbinLowering {
}

class BbinLoweringImpl[F[+_, +_]: Error2] extends BbinLowering[F] {
  override def lower(program: BbinProgram): F[NEList[BaboonIssue], List[String]] = ???
}
