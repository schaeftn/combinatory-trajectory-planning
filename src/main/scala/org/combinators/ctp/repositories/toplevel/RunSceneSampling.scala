package org.combinators.ctp.repositories.toplevel

import java.util.Properties

import akka.Done
import akka.stream.scaladsl.Source
import org.combinators.cls.interpreter.{InhabitationResult, ReflectedRepository}
import org.combinators.cls.types.{Constructor, Kinding}
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories.celldecomposition.CellDecompRepository
import org.combinators.ctp.repositories.geometry.{GeometricRepository, GeometryUtils, PpVertexList}
import org.combinators.ctp.repositories.graphsearch.GraphSearchRepository
import org.combinators.ctp.repositories.mptasks.MpTaskStartGoal
import org.combinators.ctp.repositories.python_interop.{PlannerScheme, PythonWrapper, SubstitutionScheme}
import org.combinators.ctp.repositories.samplebased.SbmpTopLevelRepository
import org.combinators.ctp.repositories.scene.{SceneSRT, _}
import org.combinators.ctp.repositories.taxkinding.{CombinatorialMotionPlanning, SbmpSemanticTypes}
import org.combinators.ctp.repositories.toplevel.RunGraphPathInhabitationTrianglesSP.{Gamma, ihBatch, l}
import org.combinators.ctp.repositories.{cmp_cd_cells, cmp_cell_graph, cmp_scene_triangulation_parameters, dimensionality_three_d_t, dimensionality_two_d_t, sbmp_cost_var, sbmp_input_data, sbmp_motion_validator_var, sbmp_optimization_objective_var, sbmp_planner_var, sbmp_sampler_var, sbmp_state_validator_var, sd_poly_scene_segmentation, sd_polygon_scene_type, sd_seg_cells, sd_seg_centroid_cells, sd_seg_lines, sd_seg_triangles_para, sd_seg_triangles_simple, _}
import scalax.collection.Graph
import scalax.collection.edge.WUnDiEdge

import scala.concurrent.Future

object RunSceneSampling extends App{
  //val ihCall  = InhabitationCall[InteropRepository, Properties](new InteropRepository{}, Constructor("p_unityConnectionProperties_type"))

  lazy val repository = new SbmpTopLevelRepository {}


  val map = Map(sbmp_planner_var -> Seq(sbmp_planner_PRM),
    sbmp_sampler_var -> Seq(sbmp_uniform_valid_state_sampler),
    sbmp_state_validator_var -> Seq(sbmp_fcl_validator),
    sbmp_motion_validator_var -> Seq(sbmp_fcl_motion_validator),
    sbmp_cost_var -> Seq(sbmp_default_cost_state),
    sbmp_optimization_objective_var -> Seq(sbmp_opt_path_length)
  )

  val kinding: Kinding = buildKinding(map)


  lazy val Gamma = ReflectedRepository(repository, substitutionSpace = kinding)

  println("kinding: " + Gamma.substitutionSpace.toString)
  println("Reflected Repository built, starting inhabitation")


  val ihBatch = Gamma.InhabitationBatchJob[PlannerScheme[SceneSRT, List[List[Float]]]](sbmp_planner_PRM)
    .addJob[SubstitutionScheme](sbmp_uniform_valid_state_sampler)
    .addJob[SubstitutionScheme](sbmp_fcl_validator)
    .addJob[SubstitutionScheme](sbmp_fcl_motion_validator)
    .addJob[SubstitutionScheme](sbmp_default_cost_state :&: sbmp_opt_path_length)
    .addJob[(SceneSRT, MpTaskStartGoal) => SubstitutionScheme](sbmp_input_data )
    .addJob[(SceneSRT, MpTaskStartGoal) => List[List[Float]]](sbmp_planning_algorithm)

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
