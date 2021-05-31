package org.combinators.ctp.repositories.samplebased

import org.combinators.ctp.repositories._
import org.combinators.cls.interpreter.combinator
import org.combinators.ctp.repositories.python_interop.{PythonTemplateUtils, SubstitutionSchema}
import org.combinators.ctp.repositories.sbmp_uniform_valid_state_sampler

trait SbmpConfigRepository extends PythonTemplateUtils{
  @combinator object SbmpConfigUseSimplification {
    def apply: SubstitutionSchema = {
      val fileMapping: Map[String, String] = Map( sbmpStartTemplate -> sbmpMainStartFile)
      val substMap = Map("$sbmp_main.simplify$" -> "self.ss.simplifySolution(self.simplification_time)")
      SubstitutionSchema(fileMapping, substMap)
    }

    val semanticType = sbmp_use_simplification
  }

  @combinator object SbmpConfigNoSimplification {
    def apply: SubstitutionSchema = {
      val fileMapping: Map[String, String] = Map( sbmpStartTemplate -> sbmpMainStartFile)
      val substMap = Map("$sbmp_main.simplify$" -> "")
      SubstitutionSchema(fileMapping, substMap)
    }

    val semanticType = sbmp_no_simplification
  }
}
