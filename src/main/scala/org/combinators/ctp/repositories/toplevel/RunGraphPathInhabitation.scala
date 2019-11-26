package org.combinators.ctp.repositories.toplevel

import java.util.Properties

import akka.Done
import akka.stream.alpakka.mqtt.MqttMessage
import akka.stream.scaladsl.{Sink, Source}
import com.typesafe.scalalogging.LazyLogging
import org.combinators.cls.interpreter.ReflectedRepository
import org.combinators.cls.types.Constructor
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories.celldecomposition.CellDecompRepository
import org.combinators.ctp.repositories.geometry.{GeometricRepository, GeometryUtils, PpVertexList}
import org.combinators.ctp.repositories.graphsearch.GraphSearchRepository
import org.combinators.ctp.repositories.mptasks.MpTaskStartGoal
import org.combinators.ctp.repositories.scene._
import org.combinators.ctp.repositories.taxkinding.CombinatorialMotionPlanning
import org.combinators.ctp.repositories.{cmp_cd_cells, cmp_cell_graph, cmp_scene_graph_path, dimensionality_two_d_t, sd_poly_scene_segmentation, sd_polygon_scene_type, sd_scene_segmentation, sd_seg_cells, sd_seg_centroid_cells, _}
import scalax.collection.Graph
import scalax.collection.edge.WUnDiEdge

import scala.concurrent.Future

object RunGraphPathInhabitation extends App with LazyLogging with AkkaImplicits {
  //val ihCall  = InhabitationCall[InteropRepository, Properties](new InteropRepository{}, Constructor("p_unityConnectionProperties_type"))

  lazy val repository = new ListenerRepository with SceneRepository with GeometricRepository with AkkaMqttComponents
    with CombinatorialTopLevel with AkkaMqttTopLevel with CellDecompRepository with GeometryUtils
    with GraphSearchRepository{}
  lazy val cmpRepository = new CombinatorialMotionPlanning{}
  lazy val Gamma = ReflectedRepository(repository, substitutionSpace = cmpRepository.kinding)

  println("kinding: " + Gamma.substitutionSpace.toString)
  println("Reflected Repository built, starting inhabitation")

  println(s"Combinators ${Gamma.combinators.size}")


  /*p_unityConnectionProperties_type =>:
      p_mqttAkkaSource_type :&: sd_unity_scene_type :&: dimensionality_two_d_t =>:
      p_mqttAkkaSource_type :&: mpt_start_goal_position_type :&: dimensionality_two_d_t =>:
      (sd_unity_scene_type :&: dimensionality_two_d_t =>: cmp_scene_graph :&: dimensionality_two_d_t) =>:
      p_mqttAkkaSink_type :&: cmp_scene_graph :&: dimensionality_two_d_t =>:
      Constructor("graphTraversalFct") =>:
      p_unitySceneAgent_type :&: cmp_vertical_cell_decomposition_type :&:*/



  val ihCall = Gamma.inhabit[Properties](p_unityConnectionProperties_type)
  println("done")
  val ihCall2 = Gamma.inhabit[Source[Scene, Future[Done]]](p_mqttAkkaSource_type
    :&: sd_unity_scene_type :&: dimensionality_two_d_t)
  println("done")
  val ihCall2a = Gamma.inhabit[Source[MpTaskStartGoal, Future[Done]]](p_mqttAkkaSource_type
    :&: mpt_start_goal_position_type :&: dimensionality_two_d_t)
  println("done")
  val ihCall3 = Gamma.inhabit[(Scene, MpTaskStartGoal) => PolySceneSegmentationGraphPath]((sd_unity_scene_type :&:
    dimensionality_two_d_t =>: cmp_scene_graph_path :&: dimensionality_two_d_t))
  println("done")
  val ihCall4 = Gamma.inhabit[Sink[MqttMessage, Future[Done]]](
    p_mqttAkkaSink_type :&: cmp_scene_graph_path :&: dimensionality_two_d_t)
  println("donesemi")
  val ihCall5 = Gamma.inhabit[Unit](p_unitySceneAgent_type :&: cmp_scene_graph_path)
  println("donefinal")

  println(f"1: ${ihCall.isEmpty}, 2: ${ihCall2.isEmpty},2a: ${ihCall2a.isEmpty}, 3: ${ihCall3.isEmpty}, " +
    f"4: ${ihCall4.isEmpty}, 5: ${ihCall5.isEmpty}")

  ihCall5.size match {
    case Some(_) => {
      println(ihCall5.terms.index(0))
      println(ihCall5.grammar)

      ihCall5.interpretedTerms.index(0)
    }
    case None => ()
  }
}
