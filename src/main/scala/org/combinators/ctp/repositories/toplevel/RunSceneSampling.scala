package org.combinators.ctp.repositories.toplevel



import org.combinators.cls.interpreter.{InhabitationResult, ReflectedRepository}
import org.combinators.cls.types.{Constructor}
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories.celldecomposition.CellDecompRepository
import org.combinators.ctp.repositories.graphsearch.GraphSearchRepository
import org.combinators.ctp.repositories.samplebased.SbmpTopLevelRepository
import org.combinators.ctp.repositories.scene.{SceneSRT, _}
import org.combinators.ctp.repositories.taxkinding.{CombinatorialMotionPlanning, SbmpSemanticTypes}
import org.combinators.ctp.repositories._


object RunSceneSampling extends App{
  //val ihCall  = InhabitationCall[InteropRepository, Properties](new InteropRepository{}, Constructor("p_unityConnectionProperties_type"))

  lazy val repository = new SceneRepository  with CmpTopLevel with AkkaMqttTopLevel with CellDecompRepository
    with GraphSearchRepository with SbmpTopLevelRepository{}
  lazy val cmpRepository = new CombinatorialMotionPlanning{}

  val sbmpKindingMap = Map(sbmp_planner_var -> Seq(sbmp_planner_RRT),
    sbmp_sampler_var -> Seq(sbmp_uniform_valid_state_sampler),
    sbmp_state_validator_var -> Seq(sbmp_fcl_validator),
    sbmp_motion_validator_var -> Seq(sbmp_fcl_motion_validator),
    sbmp_cost_var -> Seq(sbmp_default_cost_state),
    sbmp_optimization_objective_var -> Seq(sbmp_opt_path_length)
  )

  val sbmpKinding = buildKinding(sbmpKindingMap)

  lazy val Gamma = ReflectedRepository(repository, substitutionSpace = cmpRepository.kinding.merge(sbmpKinding))


  println(s"# of allowed substitutions: ${Gamma.substitutionSpace.allowedSubstitutions.values.size}")

  println(Gamma.substitutionSpace.allowedSubstitutions.values)

  println("kinding: " + Gamma.substitutionSpace.toString)
  println("Reflected Repository built, starting inhabitation")
  println(s"Combinators ${Gamma.combinators.size}")


  val ihBatch = Gamma.InhabitationBatchJob[Unit](sbmp_planner_RRT :&: Constructor("sampleAkka") :&:
    p_unitySceneAgent_type :&: dimensionality_three_d_t :&: cmp_path_only)
 /*   [PlannerScheme[SceneSRT, List[List[Float]]]](sbmp_planner_PRM)
    .addJob[SubstitutionScheme](sbmp_uniform_valid_state_sampler)
    .addJob[SubstitutionScheme](sbmp_fcl_validator)
    .addJob[SubstitutionScheme](sbmp_fcl_motion_validator)
    .addJob[SubstitutionScheme](sbmp_default_cost_state :&: sbmp_opt_path_length)
    .addJob[(SceneSRT, MpTaskStartGoal) => SubstitutionScheme](sbmp_input_data )
    .addJob[(SceneSRT, MpTaskStartGoal) => List[List[Float]]](sbmp_planning_algorithm)*/

  println("...")
  println("done")

  def getResultList(b: Gamma.InhabitationBatchJob) = {
    @scala.annotation.tailrec
    def getElements(l: List[InhabitationResult[Any]], bnew: b.ResultType):List[InhabitationResult[Any]] =
      bnew match {
        case (newJob:b.ResultType, result:InhabitationResult[Any]) => getElements(result +: l, newJob)
        case a: InhabitationResult[Any] => a +: l
      }
    getElements(List.empty, b.run())
  }

  val l = getResultList(ihBatch)

  l.map(i => println(i.target.toString() + "," + (if (i.isEmpty) "inhabitant not found" else "inhabitant found")))
  // l.last.interpretedTerms.index(0)

  l.last.interpretedTerms.index(0)

  println("interpreted term run")
}
