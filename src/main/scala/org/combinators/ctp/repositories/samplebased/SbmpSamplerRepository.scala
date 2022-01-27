package org.combinators.ctp.repositories.samplebased

import org.combinators.cls.interpreter.combinator
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.python_interop.{PythonTemplateUtils, SubstitutionSchema}

trait SbmpSamplerRepository extends PythonTemplateUtils {

  @combinator object ValidStateSamplerUniform {
    def apply: SubstitutionSchema = {
      val fileMapping: Map[String, String] = Map( sbmpStartTemplate -> sbmpMainStartFile)
      val substMap = Map(
        "$sbmp_main.spaceSampler$" -> "",
        "$sbmp_main.validStateSamplerAlloc$" ->
          "self.si.setValidStateSamplerAllocator(ob.ValidStateSamplerAllocator(ob.UniformValidStateSampler))")
      SubstitutionSchema(fileMapping, substMap)
    }

    val semanticType = sbmp_uniform_valid_state_sampler :&: sbmp_validStateSampler_type
  }

  @combinator object ValidStateSamplerObstacleBased {
    def apply: SubstitutionSchema = {
      val fileMapping: Map[String, String] = Map(sbmpStartTemplate -> sbmpMainStartFile)
      val substMap = Map(
        "$sbmp_main.spaceSampler$" -> "",
        "$sbmp_main.validStateSamplerAlloc$" ->
          "self.si.setValidStateSamplerAllocator(ob.ValidStateSamplerAllocator(ob.ObstacleBasedValidStateSampler))")
      SubstitutionSchema(fileMapping, substMap)
    }

    val semanticType = sbmp_obstacle_valid_state_sampler :&: sbmp_validStateSampler_type
  }

  @combinator object ValidStateSamplerGaussian {
    def apply: SubstitutionSchema = {
      val fileMapping: Map[String, String] = Map(sbmpStartTemplate -> sbmpMainStartFile)
      val substMap = Map(
        "$sbmp_main.spaceSampler$" -> "",
        "$sbmp_main.validStateSamplerAlloc$" ->
          "self.si.setValidStateSamplerAllocator(ob.ValidStateSamplerAllocator(ob.GaussianValidStateSampler))")
      SubstitutionSchema(fileMapping, substMap)
    }

    val semanticType = sbmp_gaussian_valid_state_sampler :&: sbmp_validStateSampler_type
  }

  @combinator object ValidStateSamplerMaxClearance {
    def apply: SubstitutionSchema = {
      val fileMapping: Map[String, String] = Map(sbmpStartTemplate -> sbmpMainStartFile)
      val substMap = Map(
        "$sbmp_main.spaceSampler$" -> "",
        "$sbmp_main.validStateSamplerAlloc$" ->
          "self.si.setValidStateSamplerAllocator(ob.ValidStateSamplerAllocator(ob.MaximizeClearanceValidStateSampler))")
      SubstitutionSchema(fileMapping, substMap)
    }

    val semanticType = sbmp_max_clearance_valid_state_sampler :&: sbmp_validStateSampler_type
  }

  @combinator object SamplerGaussSpace {
    def apply: SubstitutionSchema = {
      val fileMapping: Map[String, String] = Map(sbmpStartTemplate -> sbmpMainStartFile)
      val substMap = Map(
        "$sbmp_main.spaceSampler$" -> "space.setStateSamplerAllocator(ob.StateSamplerAllocator(ob.CompoundStateSampler))",
        "$sbmp_main.validStateSamplerAlloc$" ->
          "")
      SubstitutionSchema(fileMapping, substMap)
    }

    val semanticType = sbmp_gaussian_space_sampler :&: sbmp_spaceSampler_type
  }

  @combinator object SamplerGaussSpaceRv {
    def apply: SubstitutionSchema = {
      val fileMapping: Map[String, String] = Map(sbmpStartTemplate -> sbmpMainStartFile)
      val substMap = Map(
        "$sbmp_main.spaceSampler$" ->
          "space.setStateSamplerAllocator(ob.StateSamplerAllocator(ob.RealVectorStateSampler))",
        "$sbmp_main.validStateSamplerAlloc$" ->"")
      SubstitutionSchema(fileMapping, substMap)
    }

    val semanticType = sbmp_gaussian_space_sampler :&: sbmp_spaceSampler_type
  }



  @combinator object SamplerUniformSpace {
    def apply: SubstitutionSchema = {
      val fileMapping: Map[String, String] = Map(sbmpStartTemplate -> sbmpMainStartFile)
      val substMap = Map(
        "$sbmp_main.spaceSampler$" ->
          "space.setStateSamplerAllocator(ob.StateSamplerAllocator(ob.CompoundStateSampler))",
        "$sbmp_main.validStateSamplerAlloc$" ->
          "")
      SubstitutionSchema(fileMapping, substMap)
    }

    val semanticType = sbmp_uniform_space_sampler :&: sbmp_spaceSampler_type
  }

  @combinator object SamplerUniformSpaceRv {
    def apply: SubstitutionSchema = {
      val fileMapping: Map[String, String] = Map(sbmpStartTemplate -> sbmpMainStartFile)
      val substMap = Map("$sbmp_main.spaceSampler$" ->
          "space.setStateSamplerAllocator(ob.StateSamplerAllocator(ob.RealVectorStateSampler))",
        "$sbmp_main.validStateSamplerAlloc$" ->
          "")
      SubstitutionSchema(fileMapping, substMap)
    }

    val semanticType = sbmp_uniform_space_sampler :&: sbmp_spaceSampler_type
  }

}