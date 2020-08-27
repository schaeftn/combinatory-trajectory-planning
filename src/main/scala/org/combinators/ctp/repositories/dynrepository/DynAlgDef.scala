package org.combinators.ctp.repositories.dynrepository

import java.util.UUID
import com.typesafe.scalalogging.LazyLogging
import org.combinators.cls.interpreter.ReflectedRepository
import org.combinators.ctp.repositories.taxkinding.{CtpSemanticTypes, SbmpSemanticTypes}

trait DynAlgDef {
  self =>
  def getIhResult[T](r: ReflectedRepository[T]): r.InhabitationBatchJob
  def buildRepository: ReflectedRepository[EmptyClass]
}

class EmptyClass {}