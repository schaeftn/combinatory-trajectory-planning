package org.combinators.ctp.repositories.toplevel

import java.util.Properties

import akka.Done
import akka.stream.alpakka.mqtt.MqttMessage
import akka.stream.scaladsl.{Sink, Source}
import org.combinators.cls.interpreter.ReflectedRepository
import org.combinators.cls.types.Constructor
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories.cmp.CmpPythonRepository
import org.combinators.ctp.repositories.geometry.{GeometricRepository, GeometryUtils, PpVertexList}
import org.combinators.ctp.repositories.graphsearch.{GraphSearchPyRepository, GraphSearchRepository}
import org.combinators.ctp.repositories.mptasks.MpTaskStartGoal
import org.combinators.ctp.repositories.scene._
import org.combinators.ctp.repositories.taxkinding.CombinatorialMotionPlanning
import org.combinators.ctp.repositories._
import scalax.collection.Graph
import scalax.collection.edge.WUnDiEdge

import scala.concurrent.Future

object RunGraphPathComposedFctInhabitation extends App {
  //val ihCall  = InhabitationCall[InteropRepository, Properties](new InteropRepository{}, Constructor("p_unityConnectionProperties_type"))

  lazy val repository = new SceneRepository with CmpTopLevel with AkkaMqttTopLevel with CmpPythonRepository
    with GraphSearchRepository{}
  lazy val asd = new CombinatorialMotionPlanning{}
  println("asdasd")

  val kinding = buildKinding(repository.cmpDefaultKindingMap)
  lazy val Gamma = ReflectedRepository(repository, substitutionSpace = kinding)

  println("kinding: " + Gamma.substitutionSpace.toString)
  println("Reflected Repository built, starting inhabitation")

  val ihCall = Gamma.inhabit[Scene => PolySceneSegmentationRoadmapPath](sd_unity_scene_type)
  println("Polyscene done")
 /* val ihCall2 = Gamma.inhabit[PolygonScene => PolySceneLineSegmentation](sd_polygon_scene_type =>:
    sd_polygon_scene_type :&: sd_scene_segmentation)
  println("Segmentation done")
  val ihCall2a = Gamma.inhabit[PolySceneLineSegmentation => PolySceneCellSegmentation](sd_seg_lines
    :&: cmp_sceneSegFct_type =>: sd_seg_cells :&: cmp_sceneSegFct_type)
  println("Line to Cells done")
  val ihCall3 = Gamma.inhabit[PolySceneCellSegmentation => PolySceneCellSegmentationCentroids](
    cmp_sceneSegFct_type :&: sd_seg_cells =>: cmp_sceneSegFct_type :&: sd_seg_centroid_cells)
  println("Centroids done")
  val ihCall4 = Gamma.inhabit[ PolySceneCellSegmentationCentroids => Graph[List[Float], WUnDiEdge]](
    cmp_sceneSegFct_type :&: sd_seg_centroid_cells =>: cmp_cell_graph)
  println("Cell Graph Done")
  val ihCall5 = Gamma.inhabit[(PolySceneSegmentationRoadmap, MpTaskStartGoal) => PolySceneSegmentationRoadmap](cmp_graph_vcd_gaddFct_type )
  println("AddGraph Done")
  val ihCall6 = Gamma.inhabit[(Graph[List[Float], WUnDiEdge],MpTaskStartGoal) =>
    Seq[List[Float]]](cmp_graph_dijkstra_type)
  println("Traversal Done")
  val ihCall7 = Gamma.inhabit[(Scene, MpTaskStartGoal) => PolySceneSegmentationRoadmapPath](sd_unity_scene_type :&: mpt_start_goal_position_type :&: dimensionality_two_d_t =>:
    cmp_scene_graph_path :&: dimensionality_two_d_t)
  println("Final Done")

  println(f"1: ${ihCall.isEmpty}, 2: ${ihCall2.isEmpty},2a: ${ihCall2a.isEmpty}, 3: ${ihCall3.isEmpty}, " +
    f"4: ${ihCall4.isEmpty}, 5: ${ihCall5.isEmpty}, 6: ${ihCall6.isEmpty}, 7: ${ihCall7.isEmpty}")

  ihCall7.size match {
    case Some(_) =>
      println(ihCall7.terms.index(0))
      println(ihCall7.grammar)

      ihCall7.interpretedTerms.index(0)
    case None => ()
  }*/
}
