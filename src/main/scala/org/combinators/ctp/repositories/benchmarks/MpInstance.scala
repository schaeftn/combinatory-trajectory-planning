package org.combinators.ctp.repositories.benchmarks

import com.typesafe.scalalogging.LazyLogging
import org.combinators.cls.interpreter.InhabitationResult
import org.combinators.ctp.repositories.dynrepository.{DynAlgDef, SbmpAlg}

object MpInstance extends LazyLogging {
  def apply[R <: DynAlgDef, T, S](mpAlgDef: DynAlgDef): Option[T => S] = {
    val repository = mpAlgDef.buildRepository
    logger.info("Repository built. Starting inhabitation")
    val ihResult = mpAlgDef.getIhResult(repository)

    def getResultList[R](b: repository.InhabitationBatchJob) = {
      @scala.annotation.tailrec
      def getElements(l: List[InhabitationResult[Any]], bnew: b.ResultType): List[InhabitationResult[Any]] =
        bnew match {
          case (newJob: b.ResultType, result: InhabitationResult[Any]) => getElements(result +: l, newJob)
          case a: InhabitationResult[Any] => a +: l
        }

      getElements(List.empty, b.run())
    }

    val l = getResultList(ihResult.asInstanceOf[repository.InhabitationBatchJob])
    l.foreach(i => logger.debug((if (i.isEmpty) "inhabitant not found" else "inhabitant found") +
      "," + i.target.toString()))

    if (l.isEmpty || l.last.isEmpty) {
      logger.info("Inhabitation empty")
      None
    } else {
      logger.info("Running inhabitant.")
      // l.last.interpretedTerms.index(0).asInstanceOf[T => S]
      val r = l.last.interpretedTerms.index(0).asInstanceOf[T => S]
      logger.debug("repository built, inhab running")
      Some(r)
    }
  }
}