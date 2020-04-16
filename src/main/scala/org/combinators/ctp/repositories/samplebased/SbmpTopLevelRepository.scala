package org.combinators.ctp.repositories.samplebased

import org.combinators.cls.interpreter.combinator
import org.combinators.cls.types.Constructor
import org.combinators.ctp.repositories.scene.{SceneUtils}
import org.combinators.ctp.repositories.python_interop.{PlannerScheme, PythonTemplateUtils, PythonWrapper, SubstitutionScheme}
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.toplevel._

trait SbmpTopLevelRepository extends SceneUtils with PythonTemplateUtils with SbmpInputDataRepository with SbmpOptimizeCostRepository with SbmpSamplerRepository with SbmpValidatorRepository {

  val sbmpDefaultKindingMap = Map(sbmp_planner_var -> Seq(sbmp_planner_PRM),
    sbmp_sampler_var -> Seq(sbmp_uniform_valid_state_sampler),
    sbmp_state_validator_var -> Seq(sbmp_fcl_validator),
    sbmp_motion_validator_var -> Seq(sbmp_fcl_motion_validator),
    sbmp_cost_var -> Seq(sbmp_default_cost_state),
    sbmp_optimization_objective_var -> Seq(sbmp_opt_path_length)
  )

  trait OmplPlannerTrait[A, B] {
    def apply(pScheme: PlannerScheme[A,B],
              samplerSubstScheme: SubstitutionScheme,
              stateValidatorSubstScheme: SubstitutionScheme,
              motionValidatorSubstScheme: SubstitutionScheme,
              optimizationCostSubstScheme: SubstitutionScheme,
              dataSubstScheme: (A, MpTaskStartGoal) => SubstitutionScheme
             ): (A, MpTaskStartGoal) => B = { (input: A, task: MpTaskStartGoal) =>
      println("ompltrait")
      val schemeList = List(pScheme.st, samplerSubstScheme, stateValidatorSubstScheme,
        motionValidatorSubstScheme, optimizationCostSubstScheme, dataSubstScheme(input, task))
      val newScheme = schemeList.reduce(_.merge(_))

      val pWrapper = PythonWrapper.apply(newScheme, pScheme.startFile, pScheme.pf)
      println("pWrapper starting")
      pWrapper.computeResultAndModifyInput(input)
    }

    val semanticType =
      sbmp_planner_var =>:
        sbmp_sampler_var =>:
        sbmp_state_validator_var =>:
        sbmp_motion_validator_var =>:
        sbmp_cost_var :&: sbmp_optimization_objective_var =>:
        sbmp_input_data :&: dimensionality_var =>: //ggf plus sampler, plus motion validator?
        sbmp_planning_algorithm :&: sbmp_planner_var :&: sbmp_sampler_var :&:
          sbmp_state_validator_var :&: sbmp_motion_validator_var :&: sbmp_optimization_objective_var :&:
          sbmp_cost_var :&: dimensionality_var
  }

  @combinator object OmplPlannerRefinement extends
    OmplPlannerTrait[PolySceneSegmentationRoadmapPath, PolySceneSegmentationRoadmapPath] {}

  @combinator object OmplPlannerStandard extends
    OmplPlannerTrait[SceneSRT, List[List[Float]]] {}


  trait EmptyTemplateScheme[A] {
    def apply: A => SubstitutionScheme = (_: A) => SubstitutionScheme(
      Map.empty[String, String], //no files
      Map.empty[String, String], //no substitutions
    )
  }

  @combinator object EmptyTemplateScheme extends EmptyTemplateScheme[String] {
    val semanticType = Constructor("emptyTemplate")
  }
}