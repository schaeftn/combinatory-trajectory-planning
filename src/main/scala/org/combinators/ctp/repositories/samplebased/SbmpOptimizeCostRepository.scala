package org.combinators.ctp.repositories.samplebased

import org.combinators.cls.interpreter.combinator
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.python_interop.{PythonTemplateUtils, SubstitutionScheme}

trait SbmpOptimizeCostRepository extends PythonTemplateUtils {

  @combinator object WeightedStateIntegralStateChange {
    def apply: SubstitutionScheme = {
      val fileMapping: Map[String, String] = Map(samplingStartFileTemplate->samplingStartFile)
      val substMap = Map(
        "$sbmp_main.costs$" -> "        weights = np.array([1.0, 1.0, 1.0, 100.0, 100.0])\n        objective = WeightedEuclideanCostObjective(si, start, end, weights)"
      )
      SubstitutionScheme(fileMapping, substMap)
    }
    val semanticType = sbmp_cost_state_change_weighted

  }

  @combinator object DefaultOptimization {
    def apply: SubstitutionScheme = {
      val fileMapping: Map[String, String] = Map(samplingStartFileTemplate->samplingStartFile)
      val substMap = Map(
        "$sbmp_main.costs$" -> ""
      )
      SubstitutionScheme(fileMapping, substMap)
    }

    val semanticType = sbmp_default_cost_state :&: sbmp_opt_path_length
  }
}