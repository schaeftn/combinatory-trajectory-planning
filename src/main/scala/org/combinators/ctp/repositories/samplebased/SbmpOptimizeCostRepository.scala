package org.combinators.ctp.repositories.samplebased

import org.combinators.cls.interpreter.combinator
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.python_interop.{PythonTemplateUtils, SubstitutionScheme}

trait SbmpOptimizeCostRepository extends PythonTemplateUtils {
  @combinator object WeightedStateIntegralStateChange {
    def apply: SubstitutionScheme = {
      val fileMapping: Map[String, String] = Map(sbmpStartTemplate->sbmpMainStartFile)
      val substMap = Map(
        "$sbmp_main.cost$" -> "        weights = np.array([1.0, 1.0, 1.0, 100.0, 100.0])\n        objective = WeightedEuclideanCostObjective(si, start, end, weights)"
      )
      SubstitutionScheme(fileMapping, substMap)
    }

    val semanticType = sbmp_cost_clearance_state_change_weighted :&: sbmp_opt_integral
  }

  @combinator object DefaultOptimization {
    def apply: SubstitutionScheme = {
      val fileMapping: Map[String, String] = Map(sbmpStartTemplate->sbmpMainStartFile)
      val substMap = Map(
        "$sbmp_main.cost$" -> ""
      )
      SubstitutionScheme(fileMapping, substMap)
    }

    val semanticType = sbmp_default_cost_state :&: sbmp_opt_path_length
  }
}