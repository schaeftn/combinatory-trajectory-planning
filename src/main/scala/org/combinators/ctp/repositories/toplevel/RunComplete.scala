package org.combinators.ctp.repositories.toplevel

import java.util.Properties

import org.combinators.cls.interpreter.{InhabitationResult, ReflectedRepository}
import org.combinators.cls.types.Constructor
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories.celldecomposition.CellDecompRepository
import org.combinators.ctp.repositories.geometry.{GeometricRepository, GeometryUtils, PpVertexList}
import org.combinators.ctp.repositories.graphsearch.GraphSearchRepository
import org.combinators.ctp.repositories.mptasks.MpTaskStartGoal
import org.combinators.ctp.repositories.samplebased.SbmpTopLevelRepository
import org.combinators.ctp.repositories.scene._
import org.combinators.ctp.repositories.taxkinding.CombinatorialMotionPlanning
import org.combinators.ctp.repositories.{cmp_cd_cells, _}
import org.locationtech.jts.util.Stopwatch
import scalax.collection.Graph
import scalax.collection.edge.WUnDiEdge

object RunComplete extends App {
  lazy val repository =  new SceneRepository  with CmpTopLevel with AkkaMqttTopLevel with CellDecompRepository
    with GraphSearchRepository with SbmpTopLevelRepository{}
  lazy val cmpRepository = new CombinatorialMotionPlanning{}

  val sbmpKindingMap = Map(sbmp_planner_var -> Seq(sbmp_planner_PRM),
    sbmp_sampler_var -> Seq(sbmp_uniform_valid_state_sampler),
    sbmp_state_validator_var -> Seq(sbmp_fcl_validator),
    sbmp_motion_validator_var -> Seq(sbmp_fcl_motion_validator),
    sbmp_cost_var -> Seq(sbmp_default_cost_state),
    sbmp_optimization_objective_var -> Seq(sbmp_opt_path_length)
  )

  val sbmpKinding = buildKinding(sbmpKindingMap)

  lazy val Gamma = ReflectedRepository(repository, substitutionSpace = cmpRepository.kinding.merge(sbmpKinding))

  println("kinding: " + Gamma.substitutionSpace.toString)
  println("Reflected Repository built, starting inhabitation")

  println(s"Combinators ${Gamma.combinators.size}")
  val watch:Stopwatch = new Stopwatch
  watch.start()

  val ihBatch = Gamma.InhabitationBatchJob[Unit](p_unitySceneAgent_type :&:
    dimensionality_three_d_t :&: cmp_scene_graph_path)
    .addJob[Unit](p_unitySceneAgent_type :&: dimensionality_three_d_t :&: cmp_scene_graph_path)
    .addJob[Unit](p_unitySceneAgent_type :&: cmp_scene_graph_path :&: cmp_graph_mst_type)
    .addJob[Unit](p_unitySceneAgent_type :&: cmp_scene_graph_path :&: sd_seg_triangles_para :&: mpt_start_goal_position_type :&: cmp_graph_a_star_type)
    .addJob[Unit](p_unitySceneAgent_type :&: cmp_scene_graph_path :&: sd_seg_triangles_simple :&:
      mpt_start_goal_position_type)
    .addJob[Unit](p_unitySceneAgent_type :&: cmp_scene_graph_path :&: sd_seg_triangles_para :&: Constructor("graphTsp"))
    .addJob[Unit](p_unitySceneAgent_type :&: cmp_cd_cells)
    .addJob[Unit](p_unitySceneAgent_type :&: cmp_scene_graph_path :&: cmp_vertical_cell_decomposition_type)
    .addJob[(SceneSRT, MpTaskStartGoal) => List[List[Float]]](sbmp_planning_algorithm)


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

  watch.stop()
  println(s"elapsed time ${watch.getTimeString}")

  l.map(i => println(i.target.toString() + "," + (if (i.isEmpty) "inhabitant not found" else "inhabitant found")))

  l.last.interpretedTerms.index(0)

  println("Done")
}
