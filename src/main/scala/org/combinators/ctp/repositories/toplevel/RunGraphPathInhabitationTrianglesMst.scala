package org.combinators.ctp.repositories.toplevel

import java.util.Properties

import akka.Done
import akka.stream.alpakka.mqtt.MqttMessage
import akka.stream.scaladsl.{Sink, Source}
import com.typesafe.scalalogging.LazyLogging
import org.combinators.cls.interpreter.{InhabitationResult, ReflectedRepository}
import org.combinators.cls.types.{Constructor, Kinding}
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories.cmp.CmpPythonRepository
import org.combinators.ctp.repositories.geometry.{GeometricRepository, GeometryUtils}
import org.combinators.ctp.repositories.graphsearch.{GraphSearchPyRepository, GraphSearchRepository}
import org.combinators.ctp.repositories.mptasks.MpTaskStartGoal
import org.combinators.ctp.repositories.scene._
import org.combinators.ctp.repositories.taxkinding.CombinatorialMotionPlanning
import org.combinators.ctp.repositories.toplevel.RunGraphPathInhabitationTrianglesParaSP.Gamma
import org.combinators.ctp.repositories.{cmp_scene_graph_path, dimensionality_two_d_t, sd_polygon_scene_type, sd_seg_triangles_para_type, _}
import org.locationtech.jts.util.Stopwatch
import scalax.collection.Graph
import scalax.collection.edge.WUnDiEdge

import scala.concurrent.Future

object RunGraphPathInhabitationTrianglesMst extends App with LazyLogging with AkkaImplicits {
  //val ihCall  = InhabitationCall[InteropRepository, Properties](new InteropRepository{}, Constructor("p_unityConnectionProperties_type"))

  lazy val repository = new SceneRepository with GeometricRepository with AkkaMqttComponents
    with CmpTopLevel with AkkaMqttTopLevel with CmpPythonRepository with GeometryUtils
    with GraphSearchRepository with GraphSearchPyRepository{}
  lazy val cmpRepository = new CombinatorialMotionPlanning{}

  /*
        p_mqttAkkaComposition_type :&: cmp_scene_graph_path :&: cmp_graph_algorithm_var :&: rmc_connectorNodes_var :&: cell_centroidFct_var :&:
      rmc_cellGraph_var :&: sd_cell_type_var :&: sd_poly_scene_cell_segmentation_var :&: dimensionality_var
      */

  val trianglesMstMap = Map(
    sd_poly_scene_cell_segmentation_var -> Seq(sd_vertical_cell_decomposition_type),
    cmp_graph_algorithm_var -> Seq(cmp_graph_dijkstra_type),
    dimensionality_var -> Seq(dimensionality_two_d_t),
    rmc_connectorNodes_var -> Seq(rmc_cn_withConnectorNodes),
    rmc_centroidFct_var -> Seq(cFct_centroids_naive_type),
    rmc_cellGraph_var -> Seq(rmc_cg_centroidCellVertices),
    sd_cell_type_var -> Seq(sd_cell_vertical_type)
  )
  val kinding: Kinding = buildKinding(trianglesMstMap)

  lazy val Gamma = ReflectedRepository(repository, substitutionSpace = kinding)

  println("kinding: " + Gamma.substitutionSpace.toString)
  println("Reflected Repository built, starting inhabitation")

  println(s"# of combinators: ${Gamma.combinators.size}")
  val watch:Stopwatch = new Stopwatch
  watch.start()

  val ihBatch = Gamma.InhabitationBatchJob[Unit](p_mqttAkkaComposition_type :&: cmp_scene_graph_path :&: sd_cell_vertical_type :&: dimensionality_two_d_t)
//    .addJob[(Scene, MpTaskStartGoal) => PolySceneSegmentationRoadmapPath](cmp_algorithm_type :&: sd_cell_triangle_type :&: dimensionality_two_d_t)
//    .addJob[PolygonScene => PolySceneCellSegmentation](cmp_sceneSegFct_type :&: sd_seg_triangles_para_type :&: dimensionality_two_d_t)

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

  l.map(i => println((if (i.isEmpty) "inhabitant not found" else "inhabitant found") + ", " +i.target.toString()))

  l.last.interpretedTerms.index(0)

  println("Done")
}
