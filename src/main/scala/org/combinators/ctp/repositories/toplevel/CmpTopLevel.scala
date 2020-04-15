package org.combinators.ctp.repositories.toplevel

import java.util.Properties

import com.typesafe.scalalogging.LazyLogging
import org.combinators.cls.interpreter.combinator
import org.combinators.cls.types.Constructor
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.cmp.{CmpCellCentroidsRepository, CmpPythonRepository, CmpRoadmapRepository, CmpRoadmapRepository2}
import org.combinators.ctp.repositories.mptasks.MpTaskStartGoal
import org.combinators.ctp.repositories.scene.{PolySceneSegmentationRoadmap, _}
import org.locationtech.jts.algorithm.ConvexHull
import org.locationtech.jts.geom.impl.CoordinateArraySequence
import org.locationtech.jts.geom.{Coordinate, GeometryFactory, LineString}
import scalax.collection.Graph
import scalax.collection.edge.WUnDiEdge

trait CmpTopLevel extends LazyLogging with CmpRoadmapRepository2 with CmpPythonRepository {
  val cmpDefaultKindingMap = Map(
    dimensionality_var -> Seq(dimensionality_two_d_t),
    sd_cell_type_var -> Seq(sd_cell_triangle_type),
    sd_poly_scene_cell_segmentation_var -> Seq(sd_seg_triangles_para_type),
    rmc_cellGraph_var -> Seq(rmc_cg_allVertices),
    rmc_usingCentroids_var -> Seq(rm_withCentroids_type),
    rmc_centroidFct_var -> Seq(cFct_jts_incentre_type),
    rmc_connectorNodes_var -> Seq(rmc_cn_withConnectorNodes),
    rmc_cellNodeAddFct_var -> Seq(rmc_cna_withCellNodes_type),
    rmc_startGoalFct_var -> Seq(rmc_startGoal_nn_type),
    cmp_graph_algorithm_var -> Seq(cmp_graph_dijkstra_type))





  /*@combinator object CmpTopLevelCombinator {
    def apply(transformToPoly: Scene => PolygonScene,
              toCellSegmentation: PolygonScene => PolySceneCellSegmentation,
              toCentroids: PolySceneCellSegmentation => PolySceneCellSegmentationCentroids,
              constructRoadMap: (PolySceneCellSegmentationCentroids, MpTaskStartGoal) => PolySceneSegmentationRoadmap,
              findPath: (Graph[List[Float], WUnDiEdge], MpTaskStartGoal) => Seq[List[Float]]):
    (Scene, MpTaskStartGoal) => PolySceneSegmentationRoadmapPath = { (scene: Scene, startGoal:MpTaskStartGoal) =>
      val polyScene = transformToPoly(scene)
      val centroidSegmentation = toCentroids(toCellSegmentation(polyScene))
      val rm = constructRoadMap(centroidSegmentation, startGoal)
      rm.withPath(findPath(rm.roadmap, startGoal))
    }

    val semanticType =
      (sd_unity_scene_type =>: sd_polygon_scene_type) =>:
        cmp_sceneSegFct_type :&: sd_poly_scene_cell_segmentation_var :&: dimensionality_var =>:
        cmd_centroidFct_type :&: cell_centroidFct_var :&: sd_cell_type_var :&: dimensionality_var =>:
        cmp_cell_graph_fct :&: rmc_cellGraph_var :&: rmc_connectorNodes_var :&: dimensionality_var =>:
        cmp_graph_algorithm_var =>:
        cmp_algorithm_type :&: cmp_graph_algorithm_var :&: rmc_connectorNodes_var :&: cell_centroidFct_var :&:
          rmc_cellGraph_var :&: sd_cell_type_var :&: sd_poly_scene_cell_segmentation_var :&: dimensionality_var
  }  */
  //TODO after check RM
  @combinator object CmpTopLevelCombinator {
    def apply(transformToPoly: Scene => PolygonScene,
              toCellSegmentation: PolygonScene => PolySceneCellSegmentation,
              constructRoadMap: (PolySceneCellSegmentation, MpTaskStartGoal) => PolySceneSegmentationRoadmap,
              findPath: (Graph[List[Float], WUnDiEdge], MpTaskStartGoal) => Seq[List[Float]]):
    (Scene, MpTaskStartGoal) => PolySceneSegmentationRoadmapPath = { (scene: Scene, startGoal:MpTaskStartGoal) =>
      val polyScene = transformToPoly(scene)
      println("top starting sSeg")
      val sceneSegmentation = toCellSegmentation(polyScene)
      println("top starting rm")
      val rm = constructRoadMap(sceneSegmentation, startGoal)
      rm.withPath(findPath(rm.roadmap, startGoal))
    }

    val semanticType =
      (sd_unity_scene_type =>: sd_polygon_scene_type) =>:
        cmp_sceneSegFct_type :&: sd_poly_scene_cell_segmentation_var :&: dimensionality_var =>:
        cmp_cell_graph_fct :&: rmc_cellNodeAddFct_var :&: rmc_startGoalFct_var :&: rmc_usingCentroids_var :&:
          rmc_centroidFct_var :&: sd_cell_type_var :&: rmc_cellGraph_var :&: rmc_connectorNodes_var :&:
          dimensionality_var =>:
        cmp_graph_algorithm_var =>:
        cmp_algorithm_type :&: cmp_graph_algorithm_var :&: rmc_connectorNodes_var :&: rmc_centroidFct_var :&:
          rmc_cellGraph_var :&: sd_cell_type_var :&: sd_poly_scene_cell_segmentation_var :&: dimensionality_var :&:
          rmc_cellNodeAddFct_var :&: rmc_startGoalFct_var :&: rmc_usingCentroids_var
  }
}
