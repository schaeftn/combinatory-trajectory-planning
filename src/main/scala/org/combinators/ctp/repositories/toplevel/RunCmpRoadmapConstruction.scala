package org.combinators.ctp.repositories.toplevel

import org.combinators.cls.interpreter.{InhabitationResult, ReflectedRepository}
import org.combinators.cls.types.Constructor
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories.{cFct_jts_incentre_type, cmp_cd_cells, cmp_graph_algorithm_var, cmp_graph_dijkstra_type, dimensionality_two_d_t, dimensionality_var, mpt_start_goal_position_type, neighbourCellsNativeType, rm_withCentroids_type, rmc_cellGraph_var, rmc_cellNodeAddFct_var, rmc_centroidFct_var, rmc_cg_allVertices, rmc_cg_centroidCellVertices, rmc_cg_centroidsOnly, rmc_cn_withConnectorNodes, rmc_cn_withoutConnectorNodes, rmc_cna_withCellNodes_type, rmc_connectorNodes_var, rmc_startGoalFct_var, rmc_startGoal_cellbased_type, rmc_startGoal_nn_type, rmc_usingCentroids_var, sbmp_cost_var, sbmp_default_cost_state, sbmp_fcl_motion_validator, sbmp_fcl_validator, sbmp_motion_validator_var, sbmp_opt_path_length, sbmp_optimization_objective_var, sbmp_planner_RRT, sbmp_planner_var, sbmp_planning_algorithm, sbmp_sampler_var, sbmp_state_validator_var, sbmp_uniform_valid_state_sampler, sd_cell_triangle_type, sd_cell_type_var, sd_poly_scene_cell_segmentation_var, sd_seg_triangles_para_type, _}
import org.combinators.ctp.repositories.cmp.{CmpPythonRepository, CmpRoadmapRepository, CmpRoadmapRepository2}
import org.combinators.ctp.repositories.geometry.{GeometricRepository, GeometryUtils, PpVertexList}
import org.combinators.ctp.repositories.mptasks.MpTaskStartGoal
import org.combinators.ctp.repositories.scene._
import org.combinators.ctp.repositories.taxkinding.CombinatorialMotionPlanning
import org.combinators.ctp.repositories.toplevel.RunGraphPathInhabitationTrianglesParaSP.repository
import org.combinators.ctp.repositories.toplevel.RunSceneSamplingSmoothing.Gamma
import scalax.collection.edge.WUnDiEdge

object RunCmpRoadmapConstruction extends App {
  //val ihCall  = InhabitationCall[InteropRepository, Properties](new InteropRepository{}, Constructor("p_unityConnectionProperties_type"))

  lazy val repository = new CmpTopLevel{}


  /*
   val rmc_cellGraph_var = Variable("rmc_cellGraph_var")
  val rmc_cg_centroidsOnly = Constructor("rmc_cg_centroidsOnly")
  val rmc_cg_allVertices = Constructor("rmc_cg_allVertices")
  val rmc_cg_centroidCellVertices = Constructor("rmc_cg_centroidCellVertices")

  val rmc_connectorNodes_var = Variable("rmc_connectorNodes_var")
  val rmc_cn_withConnectorNodes = Constructor("rmc_cn_withConnectorNodes")
  val rmc_cn_withoutConnectorNodes = Constructor("rmc_cn_withoutConnectorNodes")*/

//  Map(
//    dimensionality_var -> Seq(dimensionality_two_d_t),
//    sd_cell_type_var -> Seq(sd_cell_triangle_type),
//    sd_poly_scene_cell_segmentation_var -> Seq(sd_seg_triangles_para_type),
//    rmc_cellGraph_var -> Seq(rmc_cg_allVertices),
//    rmc_usingCentroids_var -> Seq(rm_withCentroids_type),
//    rmc_centroidFct_var -> Seq(cFct_jts_incentre_type),
//    rmc_connectorNodes_var -> Seq(rmc_cn_withConnectorNodes),
//    rmc_cellNodeAddFct_var -> Seq(rmc_cna_withCellNodes_type),
//    rmc_startGoalFct_var -> Seq(rmc_startGoal_nn_type),
//    cmp_graph_algorithm_var -> Seq(cmp_graph_dijkstra_type))
//
  val cmpKinding = buildKinding(repository.cmpDefaultKindingMap ++
    Map(
      rmc_connectorNodes_var -> Seq(rmc_cn_withoutConnectorNodes),
      rmc_cellGraph_var -> Seq(rmc_cg_centroidsOnly),
      rmc_startGoalFct_var -> Seq(rmc_startGoal_nn_type),
      dimensionality_var -> Seq(dimensionality_three_d_t),
      rmc_centroidFct_var -> Seq(cFct_avg_type)))

  lazy val Gamma = ReflectedRepository(repository, substitutionSpace = cmpKinding)

  println("kinding: " + Gamma.substitutionSpace.toString)
  println("Reflected Repository built, starting inhabitation")


  /*
  *        :&: rmc_usingCentroids_var :&: rmc_centroidFct_var :&: sd_cell_type_var :&: dimensionality_var =>:
        rmc_cellNodeAddFct_type :&: rmc_cellNodeAddFct_var =>:
        rmc_connectorNodeFct_type :&: rmc_connectorNodes_var =>:
        rmc_edgeAdd_type :&: rmc_cellGraph_var =>:
        rmc_startGoalFct_type :&: rmc_startGoalFct_var :&: rmc_cellGraph_var :&: dimensionality_var =>:
  * */

  val ihBatch = Gamma.
    InhabitationBatchJob[(PolySceneCellSegmentation, MpTaskStartGoal) => PolySceneSegmentationRoadmap](cmp_cell_graph_fct).
    addJob[(PolySceneCellSegmentation, neighbourCellsNativeType) => List[List[List[Float]]]](
      cmd_centroidFct_type :&: rm_withCentroids_type :&: dimensionality_three_d_t).
    addJob[(PolySceneCellSegmentation, neighbourCellsNativeType) => List[List[List[Float]]]](rmc_cellNodeAddFct_type :&: rmc_cna_withCellNodes_type).
    addJob[(PolySceneCellSegmentation, neighbourCellsNativeType) => List[List[List[Float]]]](rmc_connectorNodeFct_type :&: rmc_cn_withConnectorNodes).
    addJob[(neighbourCellsNativeType, IndexedSeq[RmAuxDataNodes]) => IndexedSeq[WUnDiEdge[List[Float]]]](rmc_edgeAdd_type :&: rmc_cg_centroidsOnly).
    addJob[(PolySceneCellSegmentation, IndexedSeq[RmAuxDataNodes], MpTaskStartGoal) =>
      (List[List[Float]], List[WUnDiEdge[List[Float]]])](rmc_startGoalFct_type :&: rmc_startGoal_nn_type :&: rmc_cg_centroidsOnly :&: dimensionality_three_d_t)



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

  l.map(i => println((if (i.isEmpty) "inhabitant not found" else "inhabitant found") + "," + i.target.toString()))
  // l.last.interpretedTerms.index(0)

  l.last.interpretedTerms.index(0)

  println("interpreted term run")





}
