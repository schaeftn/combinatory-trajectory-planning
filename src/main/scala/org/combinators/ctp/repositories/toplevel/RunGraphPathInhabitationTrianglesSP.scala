package org.combinators.ctp.repositories.toplevel

import java.util.Properties

import akka.Done
import akka.stream.alpakka.mqtt.MqttMessage
import akka.stream.scaladsl.{Sink, Source}
import com.typesafe.scalalogging.LazyLogging
import org.combinators.cls.interpreter.{InhabitationResult, ReflectedRepository}
import org.combinators.cls.types.Constructor
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories.cmp.CmpPythonRepository
import org.combinators.ctp.repositories.geometry.{GeometricRepository, GeometryUtils}
import org.combinators.ctp.repositories.graphsearch.{GraphSearchPyRepository, GraphSearchRepository}
import org.combinators.ctp.repositories.mptasks.MpTaskStartGoal
import org.combinators.ctp.repositories.scene._
import org.combinators.ctp.repositories.taxkinding.CombinatorialMotionPlanning
import org.combinators.ctp.repositories.toplevel.RunGraphPathInhabitationTrianglesParaSP.Gamma
import org.combinators.ctp.repositories.toplevel.RunSceneSamplingSmoothing.{cmpRepository, repository}
import org.combinators.ctp.repositories._
import org.locationtech.jts.util.Stopwatch
import scalax.collection.Graph
import scalax.collection.edge.WUnDiEdge

import scala.concurrent.Future

object RunGraphPathInhabitationTrianglesSP extends App with LazyLogging with AkkaImplicits {
  //val ihCall  = InhabitationCall[InteropRepository, Properties](new InteropRepository{}, Constructor("p_unityConnectionProperties_type"))

  lazy val repository = new SceneRepository with GeometricRepository with AkkaMqttComponents
    with CmpTopLevel with AkkaMqttTopLevel with CmpPythonRepository with GeometryUtils
    with GraphSearchRepository with GraphSearchPyRepository{}
  lazy val cmpRepository = new CombinatorialMotionPlanning{}


  val sbmpKindingMap = Map(sbmp_planner_var -> Seq(sbmp_planner_RRT),
    sbmp_sampler_var -> Seq(sbmp_uniform_valid_state_sampler),
    sbmp_state_validator_var -> Seq(sbmp_fcl_validator),
    sbmp_motion_validator_var -> Seq(sbmp_fcl_motion_validator),
    sbmp_cost_var -> Seq(sbmp_default_cost_state),
    sbmp_optimization_objective_var -> Seq(sbmp_opt_path_length)
  )

  val sbmpKinding = buildKinding(sbmpKindingMap)

  lazy val Gamma = ReflectedRepository(repository, substitutionSpace = sbmpKinding)

  println("kinding: " + Gamma.substitutionSpace.toString)
  println("Reflected Repository built, starting inhabitation")

  println(s"# of combinators: ${Gamma.combinators.size}")
  val watch:Stopwatch = new Stopwatch
  watch.start()

  /*val ihBatch = Gamma.InhabitationBatchJob[Properties](p_unityConnectionProperties_type)
    .addJob[Source[Scene, Future[Done]]](p_mqttAkkaSource_type :&: sd_unity_scene_type :&: dimensionality_two_d_t)
    .addJob[Scene => PolygonScene](sd_unity_scene_type =>: sd_polygon_scene_type)
    /* .addJob[PolygonScene => CellSegmentation](sd_polygon_scene_type =>: sd_polygon_scene_type :&:

       sd_scene_segmentation :&: sd_seg_cells :&: sd_seg_triangles_simple)
 */
    .addJob[CellSegmentation => CellSegmenationCentroids](cmd_centroidFct_type)
    .addJob[CellSegmenationCentroids => Graph[List[Float], WUnDiEdge]](Constructor("tGraphbuild"))
    .addJob[(Graph[List[Float], WUnDiEdge], MpTaskStartGoal) => Graph[List[Float], WUnDiEdge]](triangle_gRefine_type)
    .addJob[(Graph[List[Float], WUnDiEdge], MpTaskStartGoal) => Seq[List[Float]]](cmp_graph_dijkstra_type)
    .addJob[(Scene, MpTaskStartGoal) => CellSegmenationPath](triangulation_path_prog_type :&: sd_seg_triangles_simple :&:
      mpt_start_goal_position_type)
    .addJob[Sink[MqttMessage, Future[Done]]](p_mqttAkkaSink_type :&: cmp_scene_graph_path :&: dimensionality_two_d_t)
    .addJob[Unit](p_unitySceneAgent_type :&: cmp_scene_graph_path :&: sd_seg_triangles_simple :&:
      mpt_start_goal_position_type)

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

  println("Done")*/
}
