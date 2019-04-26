package org.combinators.ctp.repositories.inhabitation

import org.combinators.cls.interpreter._
import org.combinators.cls.types._

class InhabitationCall[R, S](repository: R, kinding: Kinding, taxonomy: Taxonomy, t: Type) {
  val rr = ReflectedRepository[R](repository, taxonomy, kinding)

  def getIhResult[S]: InhabitationResult[S] = rr.inhabit[S](t)

  def getResult(i: Int = 0): Option[S] = {
    val result = getIhResult[S]
    if (result.isEmpty) None else Some(result.interpretedTerms.index(i))
  }
}

object InhabitationCall {
  def apply[R, S](repository: R, t: Type,
                  kinding: Kinding = Kinding.empty,
                  taxonomy: Taxonomy = Taxonomy.empty) =
    new InhabitationCall[R, S](repository, kinding, taxonomy, t)
}
