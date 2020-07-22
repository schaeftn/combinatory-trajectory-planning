package org.combinators.ctp.repositories.samplebased

import org.combinators.ctp.repositories._
import org.combinators.cls.interpreter.combinator
import org.combinators.ctp.repositories.python_interop.{PythonTemplateUtils, SubstitutionScheme}
import org.combinators.ctp.repositories.sbmp_uniform_valid_state_sampler

trait SbmpConfigRepository extends PythonTemplateUtils{
  @combinator object SbmpConfigUseSimplification {
    def apply: SubstitutionScheme = {
      val fileMapping: Map[String, String] = Map( sbmpStartTemplate -> sbmpMainStartFile)
      val substMap = Map("$sbmp_main.simplify$" -> "self.ss.simplifySolution(self.simplification_time)")
      SubstitutionScheme(fileMapping, substMap)
    }

    val semanticType = sbmp_use_simplification
  }

  @combinator object SbmpConfigNoSimplification {
    def apply: SubstitutionScheme = {
      val fileMapping: Map[String, String] = Map( sbmpStartTemplate -> sbmpMainStartFile)
      val substMap = Map("$sbmp_main.simplify$" -> "")
      SubstitutionScheme(fileMapping, substMap)
    }

    val semanticType = sbmp_no_simplification
  }
}
