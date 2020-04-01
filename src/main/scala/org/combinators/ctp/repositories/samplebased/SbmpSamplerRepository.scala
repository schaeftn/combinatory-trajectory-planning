package org.combinators.ctp.repositories.samplebased

import org.combinators.cls.interpreter.combinator
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.python_interop.{PythonTemplateUtils, SubstitutionScheme}

trait SbmpSamplerRepository extends PythonTemplateUtils {

  @combinator object SamplerUniform {
    def apply: SubstitutionScheme = {
      val fileMapping: Map[String, String] = Map( sbmpStartTemplate -> sbmpMainStartFile)
      val substMap = Map("$sbmp_main.samplerMember$" -> "Samplers.UNIFORM")
      SubstitutionScheme(fileMapping, substMap)
    }

    val semanticType = sbmp_uniform_valid_state_sampler
  }

  @combinator object SamplerObstacleBased {
    def apply: SubstitutionScheme = {
      val fileMapping: Map[String, String] = Map(sbmpStartTemplate -> sbmpMainStartFile)
      val substMap = Map("$sbmp_main.samplerMember$" -> "Samplers.OBSTACLEBASED")
      SubstitutionScheme(fileMapping, substMap)
    }

    val semanticType = sbmp_obstacle_valid_state_sampler
  }

  @combinator object SamplerGaussian {
    def apply: SubstitutionScheme = {
      val fileMapping: Map[String, String] = Map(sbmpStartTemplate -> sbmpMainStartFile)
      val substMap = Map("$sbmp_main.samplerMember$" -> "Samplers.GAUSSIAN")
      SubstitutionScheme(fileMapping, substMap)
    }

    val semanticType = sbmp_gaussian_valid_state_sampler
  }

  @combinator object SamplerMaxClearance {
    def apply: SubstitutionScheme = {
      val fileMapping: Map[String, String] = Map(sbmpStartTemplate -> sbmpMainStartFile)
      val substMap = Map("$sbmp_main.samplerMember$" -> "Samplers.MAXCLEARANCE")
      SubstitutionScheme(fileMapping, substMap)
    }

    val semanticType = sbmp_max_clearance_valid_state_sampler
  }

}