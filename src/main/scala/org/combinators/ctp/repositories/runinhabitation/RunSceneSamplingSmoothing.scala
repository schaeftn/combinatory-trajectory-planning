package org.combinators.ctp.repositories.runinhabitation

import akka.Done
import akka.stream.alpakka.mqtt.MqttMessage
import akka.stream.scaladsl.Sink
import com.typesafe.scalalogging.LazyLogging
import org.combinators.cls.interpreter.{InhabitationResult, ReflectedRepository}
import org.combinators.cls.types.{Constructor, Intersection, Type, Variable}
import org.combinators.ctp.repositories.cmp.CmpPythonRepository
import org.combinators.ctp.repositories.graphsearch.GraphSearchRepository
import org.combinators.ctp.repositories.samplebased.SbmpTopLevelRepository
import org.combinators.ctp.repositories.scene.SceneRepository
import org.combinators.ctp.repositories.taxkinding.CombinatorialMotionPlanning
import org.combinators.ctp.repositories.toplevel.{AkkaImplicits, AkkaMqttTopLevelCmp, AkkaMqttTopLevelCmpSbmp, CmpTopLevel, ProblemDefinitionFiles}
import org.combinators.ctp.repositories._
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories.python_interop.{PlannerScheme, SubstitutionScheme}
import org.combinators.ctp.repositories.runinhabitation.RunSbmpTopLevel.{Gamma, getTypeFromMap, kindingMap, resolveTypeExpression}

import scala.concurrent.Future

object RunSceneSamplingSmoothing extends App with LazyLogging {
  lazy val repository = new SceneRepository with CmpTopLevel with CmpPythonRepository
    with GraphSearchRepository with SbmpTopLevelRepository with AkkaMqttTopLevelCmpSbmp {}
  lazy val cmpRepository = new CombinatorialMotionPlanning {}

  val sbmpKindingMap = Map(sbmp_planner_var -> Seq(sbmp_planner_KPIECE1),
    sbmp_sampler_var -> Seq(sbmp_path_optimizer_sampler),
    sbmp_state_validator_var -> Seq(sbmp_fcl_validator),
    sbmp_motion_validator_var -> Seq(sbmp_discrete_motion_validator),
    sbmp_cost_var -> Seq(sbmp_default_cost_state),
    sbmp_optimization_objective_var -> Seq(sbmp_opt_path_length),
    rmc_cellGraph_var -> Seq(rmc_cg_centroidsOnly),
    rmc_connectorNodes_var -> Seq(rmc_cn_withoutConnectorNodes),
    rmc_centroidFct_var -> Seq(cFct_avg_type),
    sd_cell_type_var -> Seq(sd_cell_triangle_type),
    sd_poly_scene_cell_segmentation_var -> Seq(sd_seg_triangles_simple_type),
    dimensionality_var -> Seq(dimensionality_three_d_t),
    rmc_cellNodeAddFct_var -> Seq(rmc_cna_withoutCellNodes_type),
    rmc_startGoalFct_var -> Seq(rmc_startGoal_nn_type),
    rmc_usingCentroids_var -> Seq(rm_withCentroids_type)
  )
  val kindingMap = repository.cmpDefaultKindingMap ++ repository.sbmpDefaultKindingMap ++ sbmpKindingMap
  val kinding = buildKinding(kindingMap)

  lazy val Gamma = ReflectedRepository(repository, substitutionSpace = kinding)

  println(s"# of allowed substitutions: ${Gamma.substitutionSpace.allowedSubstitutions.values.size}")

  println(Gamma.substitutionSpace.allowedSubstitutions.values)

  println("kinding: " + Gamma.substitutionSpace.toString)
  println("Reflected Repository built, starting inhabitation")
  println(s"Combinators ${Gamma.combinators.size}")

  def getTypeFromMap(v: Variable): Type = {
    val typeList = kindingMap(v)
    if (typeList.size != 1)
      println(s"Typesize for $v is not 1: $typeList")
    typeList.head
  }

  def resolveTypeExpression(t: Type): Type = t match {
    case Intersection(a, b) => Intersection(resolveTypeExpression(a), resolveTypeExpression(b))
    case Variable(a) => getTypeFromMap(Variable(a))
    case Constructor(name, arguments@_*) => Constructor(name, arguments: _*)
  }

  val ihBatch = Gamma.InhabitationBatchJob[ProblemDefinitionFiles => List[List[Float]]](
    resolveTypeExpression(cmp_algorithm_type :&: cmp_graph_algorithm_var :&: rmc_connectorNodes_var :&: rmc_centroidFct_var :&:
      rmc_cellGraph_var :&: sd_cell_type_var :&: sd_poly_scene_cell_segmentation_var :&: dimensionality_var :&:
      rmc_cellNodeAddFct_var :&: rmc_startGoalFct_var :&: rmc_usingCentroids_var)
  )
    .addJob[((ProblemDefinitionFiles, List[List[Float]])) => List[List[Float]]](
      resolveTypeExpression(sbmp_planning_algorithm :&:
        sbmp_planner_var :&: sbmp_sampler_var :&:
        sbmp_state_validator_var :&: sbmp_motion_validator_var :&: sbmp_optimization_objective_var :&:
        sbmp_cost_var))
    .addJob[PlannerScheme[(ProblemDefinitionFiles, List[List[Float]]), List[List[Float]]]](
      resolveTypeExpression(sbmp_planner_var))
    .addJob[SubstitutionScheme](resolveTypeExpression(sbmp_sampler_var))
    .addJob[SubstitutionScheme](resolveTypeExpression(sbmp_state_validator_var :&: sbmp_state_validator_var))
    .addJob[SubstitutionScheme](resolveTypeExpression(sbmp_motion_validator_var))
    .addJob[SubstitutionScheme](resolveTypeExpression(sbmp_cost_var :&: sbmp_optimization_objective_var))
    .addJob[((ProblemDefinitionFiles, List[List[Float]])) => SubstitutionScheme](
      resolveTypeExpression(sbmp_input_data :&: dimensionality_var))
    .addJob[Sink[MqttMessage, Future[Done]]](
      resolveTypeExpression(p_mqttAkkaSink_type :&: cmp_path_only :&: dimensionality_var))
    .addJob[Unit](
      resolveTypeExpression(p_fileToAkka_type :&: dimensionality_var :&: cmp_path_only :&:
        sbmp_planner_var :&: sbmp_sampler_var :&: sbmp_state_validator_var :&: sbmp_motion_validator_var :&:
        sbmp_optimization_objective_var :&: sbmp_cost_var :&: cmp_graph_algorithm_var :&: rmc_connectorNodes_var :&:
        rmc_centroidFct_var :&: rmc_cellGraph_var :&: sd_cell_type_var :&: sd_poly_scene_cell_segmentation_var :&:
        rmc_cellNodeAddFct_var :&: rmc_startGoalFct_var :&: rmc_usingCentroids_var))

  println("...")
  println("done")

  def getResultList(b: Gamma.InhabitationBatchJob) = {
    @scala.annotation.tailrec
    def getElements(l: List[InhabitationResult[Any]], bnew: b.ResultType): List[InhabitationResult[Any]] =
      bnew match {
        case (newJob: b.ResultType, result: InhabitationResult[Any]) => getElements(result +: l, newJob)
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
