package org.combinators.ctp.repositories.runinhabitation

import java.util.Properties

import akka.Done
import akka.stream.alpakka.mqtt.MqttMessage
import akka.stream.scaladsl.{Sink, Source}
import com.typesafe.scalalogging.LazyLogging
import org.combinators.cls.interpreter.{InhabitationResult, ReflectedRepository}
import org.combinators.cls.types.syntax._
import org.combinators.cls.types.{Arrow, Constructor, Intersection, Type, Variable}
import org.combinators.ctp.repositories.cmp.CmpTopLevelRepository
import org.combinators.ctp.repositories.{dimensionality_var, rmc_connectorNodes_var, rmc_usingCentroids_var, sd_polygon_scene_type, _}
import org.combinators.ctp.repositories.geometry.{GeometricRepository, GeometryUtils}
import org.combinators.ctp.repositories.graphsearch.{GraphSearchPyRepository, GraphSearchRepository}
import org.combinators.ctp.repositories.samplebased.SbmpTopLevelRepository
import org.combinators.ctp.repositories.scene.SceneRepository
import org.combinators.ctp.repositories.taxkinding.CtpSemanticTypes
import org.combinators.ctp.repositories.toplevel._
import org.locationtech.jts.util.Stopwatch
import scalax.collection.Graph
import scalax.collection.edge.WUnDiEdge

import scala.concurrent.Future

object RunAkkaTopLevelCmp extends App with LazyLogging with AkkaImplicits {
  lazy val repository = new SceneRepository
    with CmpTopLevelRepository with AkkaMqttTopLevelCmp with GraphSearchRepository {}

  lazy val cmpRepository = new CtpSemanticTypes {}

  val kindingMap = repository.cmpFullKindingMap
  // Tri Ex
//  val kindingMap = repository.cmpDefaultKindingMap ++
//    Map(
//      rmc_connectorNodes_var -> Seq(rmc_cn_withConnectorNodes),
//      rmc_cellNodeAddFct_var -> Seq(rmc_cna_withoutCellNodes_type),
//      rmc_cellGraph_var -> Seq(rmc_cg_centroidCellVertices),
//      rmc_usingCentroids_var -> Seq(rm_withCentroids_type),
//      rmc_startGoalFct_var -> Seq(rmc_startGoal_nn_type),
//      cmp_graph_algorithm_var -> Seq(cmp_graph_dijkstra_type),
//      rmc_centroidFct_var -> Seq(cFct_centroids_naive_type),
//      dimensionality_var -> Seq(dimensionality_two_d_t),
//      sd_poly_scene_cell_segmentation_var -> Seq(sd_seg_triangles_para_type),
//      sd_cell_type_var -> Seq(sd_cell_triangle_type)
//    )

  // Tri Ex Mst
//  val kindingMap = repository.cmpDefaultKindingMap ++
//    Map(
//      rmc_connectorNodes_var -> Seq(rmc_cn_withoutConnectorNodes),
//      rmc_cellNodeAddFct_var -> Seq(rmc_cna_withoutCellNodes_type),
//      rmc_cellGraph_var -> Seq(rmc_cg_centroidsOnly),
//      rmc_usingCentroids_var -> Seq(rm_withCentroids_type),
//      rmc_startGoalFct_var -> Seq(rmc_startGoal_nn_type),
//      cmp_graph_algorithm_var -> Seq(cmp_graph_mst_type),
//      rmc_centroidFct_var -> Seq(cFct_jts_incentre_type),
//      dimensionality_var -> Seq(dimensionality_two_d_t),
//      sd_poly_scene_cell_segmentation_var -> Seq(sd_seg_triangles_para_type),
//      sd_cell_type_var -> Seq(sd_cell_triangle_type)
//    )

// GridEx
//  val kindingMap = repository.cmpDefaultKindingMap ++
//    Map(
//      rmc_connectorNodes_var -> Seq(rmc_cn_withoutConnectorNodes),
//      rmc_cellNodeAddFct_var -> Seq(rmc_cna_withoutCellNodes_type),
//      rmc_cellGraph_var -> Seq(rmc_cg_centroidsOnly),
//      rmc_usingCentroids_var -> Seq(rm_withCentroids_type),
//      rmc_startGoalFct_var -> Seq(rmc_startGoal_nn_type),
//      cmp_graph_algorithm_var -> Seq(cmp_graph_dijkstra_type),
//      rmc_centroidFct_var -> Seq(cFct_centroids_naive_type),
//      dimensionality_var -> Seq(dimensionality_two_d_t),
//      sd_poly_scene_cell_segmentation_var -> Seq(sd_seg_grid_type),
//      sd_cell_type_var -> Seq(sd_cell_vertical_type)
//    )

  // Vcd Ex
//  val kindingMap = repository.cmpDefaultKindingMap ++
//    Map(
//      rmc_connectorNodes_var -> Seq(rmc_cn_withConnectorNodes),
//      rmc_cellNodeAddFct_var -> Seq(rmc_cna_withCellNodes_type),
//      rmc_cellGraph_var -> Seq(rmc_cg_allVertices),
//      rmc_usingCentroids_var -> Seq(rm_withCentroids_type),
//      rmc_startGoalFct_var -> Seq(rmc_startGoal_nn_type),
//      cmp_graph_algorithm_var -> Seq(cmp_graph_dijkstra_type),
//      rmc_centroidFct_var -> Seq(cFct_centroids_naive_type),
//      dimensionality_var -> Seq(dimensionality_two_d_t),
//      sd_poly_scene_cell_segmentation_var -> Seq(sd_vertical_cell_decomposition_type),
//      sd_cell_type_var -> Seq(sd_cell_vertical_type)
//    )


  // Tetra
//  val kindingMap = repository.cmpDefaultKindingMap ++
//    Map(
//      rmc_connectorNodes_var -> Seq(rmc_cn_withoutConnectorNodes),
//      rmc_cellNodeAddFct_var -> Seq(rmc_cna_withoutCellNodes_type),
//      rmc_cellGraph_var -> Seq(rmc_cg_centroidsOnly),
//      rmc_usingCentroids_var -> Seq(rm_withCentroids_type),
//      rmc_startGoalFct_var -> Seq(rmc_startGoal_nn_type),
//      cmp_graph_algorithm_var -> Seq(cmp_graph_dijkstra_type),
//      rmc_centroidFct_var -> Seq(cFct_avg_type),
//      dimensionality_var -> Seq(dimensionality_three_d_t),
//      sd_poly_scene_cell_segmentation_var -> Seq(sd_seg_triangles_para_type),
//      sd_cell_type_var -> Seq(sd_cell_triangle_type)
//    )

  val cmpKinding = buildKinding(kindingMap)

  def getTypeFromMap(v: Variable): Type = {
    val typeList = kindingMap(v)
    if (typeList.size != 1)
      println(s"Typesize for $v is not 1: $typeList")
    typeList.head
  }

  def resolveTypeExpression(t: Type): Type = t match {
    case Intersection(a, b) => Intersection(resolveTypeExpression(a), resolveTypeExpression(b))
    case Variable(a) => getTypeFromMap(Variable(a))
    case Constructor(name, arguments) => Constructor(name, arguments)
    case Arrow(s, t) => Arrow(resolveTypeExpression(s),resolveTypeExpression(t))
  }

  val watch1: Stopwatch = new Stopwatch
  watch1.start()

  println(repository)
  lazy val Gamma = ReflectedRepository(repository, substitutionSpace = cmpKinding)

  println(Gamma.combinators.foreach{ case ((a,b)) =>  println(s"Combinator: $a, type: $b")})
  println(s"# of allowed substitutions: ${Gamma.substitutionSpace.allowedSubstitutions.values.size}")

  println("kinding: " + Gamma.substitutionSpace.toString)
  println("Reflected Repository built, starting inhabitation")
  watch1.stop()
  println(s"elapsed time ${watch1.getTimeString}")
  //println(s"# of combinators: ${Gamma.combinators.size}")

  val watch: Stopwatch = new Stopwatch
  watch.start()

  val ihBatch = Gamma.InhabitationBatchJob[ProblemDefinitionFiles => Graph[List[Float], WUnDiEdge]](
    resolveTypeExpression(cmp_sceneSegFct_type :&: cmp_cell_graph_fct_type :&: sd_poly_scene_cell_segmentation_var :&:
      dimensionality_var :&:
      rmc_cellNodeAddFct_var :&: rmc_startGoalFct_var :&: rmc_usingCentroids_var :&:
      rmc_centroidFct_var :&: sd_cell_type_var :&: rmc_cellGraph_var :&: rmc_connectorNodes_var))
    .addJob[(Graph[List[Float], WUnDiEdge], MpTaskStartGoal) => Seq[List[Float]]](
      resolveTypeExpression(cmp_graph_algorithm_var))
    .addJob[Scene => PolygonScene](resolveTypeExpression((sd_unity_scene_type =>: sd_polygon_scene_type) :&: dimensionality_var))
    .addJob[PolygonScene => PolySceneCellSegmentation](resolveTypeExpression(cmp_sceneSegFct_type :&: sd_poly_scene_cell_segmentation_var :&: dimensionality_var))
    .addJob[(PolySceneCellSegmentation, MpTaskStartGoal) => PolySceneSegmentationRoadmap](
      resolveTypeExpression(cmp_cell_graph_fct_type :&: rmc_cellNodeAddFct_var :&: rmc_startGoalFct_var :&: rmc_usingCentroids_var :&:
        rmc_centroidFct_var :&: sd_cell_type_var :&: rmc_cellGraph_var :&: rmc_connectorNodes_var :&:
        dimensionality_var))
    .addJob[(Graph[List[Float], WUnDiEdge], MpTaskStartGoal) => Seq[List[Float]]
    ](resolveTypeExpression(cmp_graph_algorithm_var))
    .addJob[Sink[MqttMessage, Future[Done]]](
      resolveTypeExpression(p_mqttAkkaSink_type :&: cmp_scene_graph_path :&: dimensionality_var))
    .addJob[(Scene, MpTaskStartGoal) => PolySceneSegmentationRoadmapPath](
      resolveTypeExpression(cmp_algorithm_type :&: cmp_graph_algorithm_var :&: rmc_connectorNodes_var :&: rmc_centroidFct_var :&:
        rmc_cellGraph_var :&: sd_cell_type_var :&: sd_poly_scene_cell_segmentation_var :&: dimensionality_var :&:
        rmc_cellNodeAddFct_var :&: rmc_startGoalFct_var :&: rmc_usingCentroids_var))
    .addJob[Unit](
      resolveTypeExpression(
        p_mqttAkkaComposition_type :&: cmp_scene_graph_path :&: cmp_graph_algorithm_var :&: rmc_connectorNodes_var :&:
          rmc_centroidFct_var :&: rmc_cellGraph_var :&: sd_cell_type_var :&: sd_poly_scene_cell_segmentation_var :&:
          dimensionality_var :&: rmc_cellNodeAddFct_var :&: rmc_startGoalFct_var :&: rmc_usingCentroids_var)
    )

  def getResultList(b: Gamma.InhabitationBatchJob) = {
    @scala.annotation.tailrec
    def getElements(l: List[InhabitationResult[Any]], bnew: b.ResultType): List[InhabitationResult[Any]] =
      bnew match {
        case (newJob: b.ResultType, result: InhabitationResult[Any]) => getElements(result +: l, newJob)
        case a: InhabitationResult[Any] => a +: l
      }

    getElements(List.empty, b.run())
  }

  val res = ihBatch.run()

  val l = getResultList(ihBatch)

  watch.stop()
  println(s"elapsed time ${watch.getTimeString}")

  l.map(i => println((if (i.isEmpty) "inhabitant not found" else "inhabitant found") + "," + i.target.toString()))

  val tp = new TreePrinting {}
  println(tp.getStringForTree( l.last.terms.index(0)))
  l.last.interpretedTerms.index(0)

  println("Done")
}
