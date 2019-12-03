package org.combinators.ctp.repositories.toplevel

import java.util.Properties

import akka.Done
import akka.stream.alpakka.mqtt.MqttMessage
import akka.stream.scaladsl.{Sink, Source}
import com.typesafe.scalalogging.LazyLogging
import org.combinators.cls.interpreter.{InhabitationResult, ReflectedRepository}
import org.combinators.cls.types.Constructor
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories.celldecomposition.CellDecompRepository
import org.combinators.ctp.repositories.geometry.{GeometricRepository, GeometryUtils}
import org.combinators.ctp.repositories.graphsearch.GraphSearchRepository
import org.combinators.ctp.repositories.mptasks.MpTaskStartGoal
import org.combinators.ctp.repositories.scene._
import org.combinators.ctp.repositories.taxkinding.CombinatorialMotionPlanning
import org.combinators.ctp.repositories.{cmp_scene_graph_path, dimensionality_three_d_t, dimensionality_two_d_t, sd_polygon_scene_type, sd_seg_cells, _}
import org.locationtech.jts.util.Stopwatch
import scalax.collection.Graph
import scalax.collection.edge.WUnDiEdge

import scala.concurrent.Future

object RunGraphPathInhabitationTetrahedralization extends App with LazyLogging with AkkaImplicits {
  //val ihCall  = InhabitationCall[InteropRepository, Properties](new InteropRepository{}, Constructor("p_unityConnectionProperties_type"))

  lazy val repository = new ListenerRepository with SceneRepository with GeometricRepository with AkkaMqttComponents
    with CombinatorialTopLevel with AkkaMqttTopLevel with CellDecompRepository with GeometryUtils
    with GraphSearchRepository{}
  lazy val cmpRepository = new CombinatorialMotionPlanning{}
  lazy val Gamma = ReflectedRepository(repository, substitutionSpace = cmpRepository.kinding)

  println("kinding: " + Gamma.substitutionSpace.toString)
  println("Reflected Repository built, starting inhabitation")

  println(s"Combinators ${Gamma.combinators.size}")
  val watch:Stopwatch = new Stopwatch
  watch.start();

  val ihBatch = Gamma.InhabitationBatchJob[Properties](p_unityConnectionProperties_type)
    .addJob[Scene => PolygonScene](
      (sd_unity_scene_type =>: sd_polygon_scene_type) :&: dimensionality_three_d_t)
    .addJob[PolygonScene => TriangleSeg](
      (sd_polygon_scene_type =>: sd_scene_segmentation) :&: dimensionality_three_d_t )
    .addJob[TriangleSeg => TriangleSegCentroids](
      Constructor("tCentroidsNd"))
    .addJob[TriangleSegCentroids => Graph[List[Float], WUnDiEdge]](
      Constructor("tGraphbuildNd"))
    .addJob[(Graph[List[Float], WUnDiEdge], MpTaskStartGoal) => Graph[List[Float], WUnDiEdge]](
      Constructor("tGraphAdd"))
    .addJob[ (Graph[List[Float], WUnDiEdge],MpTaskStartGoal) =>
      (Seq[List[Float]], Seq[WUnDiEdge[List[Float]]], Float)](Constructor("graphTraversalFct"))
    .addJob[(Scene, MpTaskStartGoal) => TriangleSegPath](Constructor("tet"))
    .addJob[Unit]( p_unitySceneAgent_type :&: dimensionality_three_d_t :&: cmp_scene_graph_path)


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
