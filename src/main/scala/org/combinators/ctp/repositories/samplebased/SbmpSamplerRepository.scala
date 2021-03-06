package org.combinators.ctp.repositories.samplebased

import org.combinators.cls.interpreter.combinator
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.python_interop.{PythonTemplateUtils, SubstitutionSchema}

trait SbmpSamplerRepository extends PythonTemplateUtils {

  @combinator object SamplerUniform {
    def apply: SubstitutionSchema = {
      val fileMapping: Map[String, String] = Map( sbmpStartTemplate -> sbmpMainStartFile)
      val substMap = Map("$sbmp_main.samplerMember$" -> "Samplers.UNIFORM",
        "$sbmp_main.spaceSampler$" -> "",
        "$sbmp_main.validStateSamplerAlloc$" ->
          "self.si.setValidStateSamplerAllocator(ob.ValidStateSamplerAllocator(getSampler))")
      SubstitutionSchema(fileMapping, substMap)
    }

    val semanticType = sbmp_uniform_valid_state_sampler :&: sbmp_validStateSampler_type
  }

  @combinator object SamplerObstacleBased {
    def apply: SubstitutionSchema = {
      val fileMapping: Map[String, String] = Map(sbmpStartTemplate -> sbmpMainStartFile)
      val substMap = Map("$sbmp_main.samplerMember$" -> "Samplers.OBSTACLEBASED",
        "$sbmp_main.spaceSampler$" -> "",
        "$sbmp_main.validStateSamplerAlloc$" ->
          "self.si.setValidStateSamplerAllocator(ob.ValidStateSamplerAllocator(getSampler))")
      SubstitutionSchema(fileMapping, substMap)
    }

    val semanticType = sbmp_obstacle_valid_state_sampler :&: sbmp_validStateSampler_type
  }

  @combinator object SamplerGaussian {
    def apply: SubstitutionSchema = {
      val fileMapping: Map[String, String] = Map(sbmpStartTemplate -> sbmpMainStartFile)
      val substMap = Map("$sbmp_main.samplerMember$" -> "Samplers.GAUSSIAN",
        "$sbmp_main.spaceSampler$" -> "",
        "$sbmp_main.validStateSamplerAlloc$" ->
          "self.si.setValidStateSamplerAllocator(ob.ValidStateSamplerAllocator(getSampler))")
      SubstitutionSchema(fileMapping, substMap)
    }

    val semanticType = sbmp_gaussian_valid_state_sampler :&: sbmp_validStateSampler_type
  }

  @combinator object SamplerMaxClearance {
    def apply: SubstitutionSchema = {
      val fileMapping: Map[String, String] = Map(sbmpStartTemplate -> sbmpMainStartFile)
      val substMap = Map("$sbmp_main.samplerMember$" -> "Samplers.MAXCLEARANCE",
        "$sbmp_main.spaceSampler$" -> "",
        "$sbmp_main.validStateSamplerAlloc$" ->
          "self.si.setValidStateSamplerAllocator(ob.ValidStateSamplerAllocator(getSampler))")
      SubstitutionSchema(fileMapping, substMap)
    }

    val semanticType = sbmp_max_clearance_valid_state_sampler :&: sbmp_validStateSampler_type
  }
//
//  @combinator object SamplerValidPathOptimizer {
//    def apply: SubstitutionScheme = {
//      val fileMapping: Map[String, String] = Map(sbmpStartTemplate -> sbmpMainStartFile)
//      val substMap = Map("$sbmp_main.samplerMember$" -> "Samplers.CUSTOM",
//        "$sbmp_main.spaceSampler$" -> "",
//        "$sbmp_main.validStateSamplerAlloc$" ->
//          "self.si.setValidStateSamplerAllocator(ob.ValidStateSamplerAllocator(getSampler))")
//      SubstitutionScheme(fileMapping, substMap)
//    }
//
//    val semanticType = sbmp_valid_path_optimizer_sampler
//  }
//
//  @combinator object SamplerPathOptimizer {
//    def apply: SubstitutionScheme = {
//      val fileMapping: Map[String, String] = Map(sbmpStartTemplate -> sbmpMainStartFile)
//      val substMap = Map("$sbmp_main.samplerMember$" -> "Samplers.CUSTOM",
//        "$sbmp_main.spaceSampler$" -> "space.setStateSamplerAllocator(ob.StateSamplerAllocator(getStateSpaceSampler))",
//        "$sbmp_main.validStateSamplerAlloc$" -> "")
//      SubstitutionScheme(fileMapping, substMap)
//    }
//
//    val semanticType = sbmp_path_optimizer_sampler
//  }

  @combinator object SamplerGaussSpace {
    def apply: SubstitutionSchema = {
      val fileMapping: Map[String, String] = Map(sbmpStartTemplate -> sbmpMainStartFile)
      val substMap = Map("$sbmp_main.samplerMember$" -> "Samplers.GAUSSIAN",
        "$sbmp_main.spaceSampler$" -> "space.setStateSamplerAllocator(ob.StateSamplerAllocator(getStateSpaceSampler))",
        "$sbmp_main.validStateSamplerAlloc$" ->
          "")
      SubstitutionSchema(fileMapping, substMap)
    }

    val semanticType = sbmp_gaussian_space_sampler :&: sbmp_spaceSampler_type
  }

  @combinator object SamplerUniformSpace {
    def apply: SubstitutionSchema = {
      val fileMapping: Map[String, String] = Map(sbmpStartTemplate -> sbmpMainStartFile)
      val substMap = Map("$sbmp_main.samplerMember$" -> "Samplers.UNIFORM",
        "$sbmp_main.spaceSampler$" -> "space.setStateSamplerAllocator(ob.StateSamplerAllocator(getStateSpaceSampler))",
        "$sbmp_main.validStateSamplerAlloc$" ->
          "")
      SubstitutionSchema(fileMapping, substMap)
    }

    val semanticType = sbmp_uniform_space_sampler :&: sbmp_spaceSampler_type
  }

}