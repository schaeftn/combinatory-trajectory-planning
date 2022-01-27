package org.combinators.ctp.repositories.runinhabitation

import org.combinators.cls.interpreter.ReflectedRepository
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.samplebased.{SbmpPlannerTemplateRepository, SbmpTopLevelRepository}
import org.combinators.ctp.repositories.taxkinding.CtpSemanticTypes
import org.combinators.ctp.repositories.toplevel.FileBasedTopLevelSbmp

object PrintSbmpCombinatorInfo extends App {
  lazy val repository = new FileBasedTopLevelSbmp with SbmpTopLevelRepository with SbmpPlannerTemplateRepository{
    override val parseFct: String => List[List[sphereRadius]] = parseFctSe3
  }
  lazy val cmpRepository = new CtpSemanticTypes {}

  val kindingMap = repository.sbmpFullKindingMap
  val sbmpKinding = buildKinding(kindingMap)

  println("Building reflected repository")
  lazy val Gamma = ReflectedRepository(repository, substitutionSpace = sbmpKinding)

  Gamma.combinators.foreach{ case ((a,b)) =>  println(s"Combinator: $a, type: $b")}

  println(s"Gamma.combinators.size: ${Gamma.combinators.size}")
  println(s"Gamma.combinatorComponents.size: ${Gamma.combinatorComponents.size}")

  println(s"# of allowed substitutions: ${Gamma.substitutionSpace.allowedSubstitutions.values.size}")
}
