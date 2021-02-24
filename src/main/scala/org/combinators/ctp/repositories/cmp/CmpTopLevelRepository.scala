package org.combinators.ctp.repositories.cmp

import com.typesafe.scalalogging.LazyLogging
import org.combinators.cls.interpreter.combinator
import org.combinators.cls.types.Type
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.pathcoverage.JtsUtils
import org.combinators.ctp.repositories.toplevel._
import org.combinators.ctp.repositories.scene.SceneUtils
import org.locationtech.jts.io.WKTReader
import scalax.collection.Graph
import scalax.collection.edge.WUnDiEdge

import scala.io.Source

trait CmpTopLevelRepository extends LazyLogging with CmpRoadmapRepository with CmpCdRepository with SceneUtils{
  val cmpDefaultKindingMap = Map(
    dimensionality_var -> Seq(dimensionality_two_d_t),
    sd_cell_type_var -> Seq(sd_cell_triangle_type),
    sd_poly_scene_cell_segmentation_var -> Seq(sd_seg_triangles_para_type),
    rmc_cellGraph_var -> Seq(rmc_cg_allVertices),
    rmc_usingCentroids_var -> Seq(rm_withCentroids_type),
    rmc_centroidFct_var -> Seq(cFct_jts_incentre_type),
    rmc_connectorNodes_var -> Seq(rmc_cn_withConnectorNodes),
    rmc_cellNodeAddFct_var -> Seq(rmc_cna_withoutCellNodes_type),
    rmc_startGoalFct_var -> Seq(rmc_startGoal_nn_type),
    cmp_graph_algorithm_var -> Seq(cmp_graph_dijkstra_type))
  val cmpFullKindingMap = Map(
    dimensionality_var -> Seq(
      dimensionality_two_d_t,
      dimensionality_three_d_t,
      dimensionality_n_d_t),
    sd_cell_type_var -> Seq(
      sd_cell_triangle_type,
      sd_cell_vertical_type),
    sd_poly_scene_cell_segmentation_var -> Seq(
      sd_vertical_cell_decomposition_type,
      sd_seg_triangles_simple_type,
      sd_seg_triangles_para_type,
      sd_seg_grid_type
    ),
    rmc_cellGraph_var -> Seq(
      rmc_cg_centroidsOnly,
      rmc_cg_allVertices,
      rmc_cg_centroidCellVertices),
    rmc_usingCentroids_var -> Seq(
      rm_withCentroids_type,
      rm_withoutCentroids_type),
    rmc_centroidFct_var -> Seq(
      cFct_centroids_naive_type,
      cFct_jts_default_type,
      cFct_jts_incentre_type,
      cFct_avg_type,
      cFct_triangle_centroidsNd_type),
    rmc_connectorNodes_var -> Seq(
      rmc_cn_withConnectorNodes,
      rmc_cn_withoutConnectorNodes),
    rmc_cellNodeAddFct_var -> Seq(
      rmc_cna_withCellNodes_type,
      rmc_cna_withoutCellNodes_type),
    rmc_startGoalFct_var -> Seq(
      rmc_startGoal_nn_type,
      rmc_startGoal_cellbased_type),
    cmp_graph_algorithm_var -> Seq(
      cmp_graph_dijkstra_type,
      cmp_graph_a_star_type,
      cmp_graph_vbi_type,
      cmp_graph_mst_type,
      cmp_graph_tsp_type)
  )

  trait CmpTopLevelCombinatorTrait {
    def apply(transformToPoly: Scene => PolygonScene,
              toCellSegmentation: PolygonScene => PolySceneCellSegmentation,
              constructRoadMap: (PolySceneCellSegmentation, MpTaskStartGoal) => PolySceneSegmentationRoadmap,
              findPath: (Graph[List[Float], WUnDiEdge], MpTaskStartGoal) => Seq[List[Float]]):
    (Scene, MpTaskStartGoal) => PolySceneSegmentationRoadmapPath = { (scene: Scene, startGoal:MpTaskStartGoal) =>
      val polyScene = transformToPoly(scene)
      val sceneSegmentation = toCellSegmentation(polyScene)
      println("done seg")
      val rm = constructRoadMap(sceneSegmentation, startGoal)
      println("done rm")
      rm.withPath(findPath(rm.roadmap, startGoal))
    }
    val semanticType:Type

  }
  @combinator object CmpTopLevelCombinator extends CmpTopLevelCombinatorTrait {
    val semanticType =
      (sd_unity_scene_type =>: sd_polygon_scene_type) :&: dimensionality_var =>:
        cmp_sceneSegFct_type :&: sd_poly_scene_cell_segmentation_var :&: dimensionality_var =>:
        cmp_cell_graph_fct_type :&: rmc_cellNodeAddFct_var :&: rmc_startGoalFct_var :&: rmc_usingCentroids_var :&:
          rmc_centroidFct_var :&: sd_cell_type_var :&: rmc_cellGraph_var :&: rmc_connectorNodes_var :&:
          dimensionality_var =>:
        cmp_graph_algorithm_var =>:
        cmp_algorithm_type :&: cmp_graph_algorithm_var :&: rmc_connectorNodes_var :&: rmc_centroidFct_var :&:
          rmc_cellGraph_var :&: sd_cell_type_var :&: sd_poly_scene_cell_segmentation_var :&: dimensionality_var :&:
          rmc_cellNodeAddFct_var :&: rmc_startGoalFct_var :&: rmc_usingCentroids_var
  }

  @combinator object CmpTopLevelCombinatorTax extends CmpTopLevelCombinatorTrait {
    val semanticType =
      (sd_unity_scene_type =>: sd_polygon_scene_type) =>:
        cmp_sceneSegFct_type  =>:
        cmp_cell_graph_fct_type =>:
        cmp_any_graph_algorithm_type =>:
        cmp_algorithm_type
  }

//  @combinator object CmpTopLevelCombinatorFileBased {
//    def apply(toCellSegmentation: ProblemDefinitionFiles => PolySceneCellSegmentation,
//              constructRoadMap: (PolySceneCellSegmentation, MpTaskStartGoal) => PolySceneSegmentationRoadmap,
//              findPath: (Graph[List[Float], WUnDiEdge], MpTaskStartGoal) => Seq[List[Float]]):
//    ProblemDefinitionFiles => List[List[Float]] = { pdef:ProblemDefinitionFiles =>
//      println("top starting sSeg")
//      val sceneSegmentation = toCellSegmentation(pdef)
//      println("top starting rm")
//      val startGoal = getMpStartGoalFromProperties(pdef.problemProperties)
//      val rm = constructRoadMap(sceneSegmentation, startGoal)
//      findPath(rm.roadmap, startGoal).toList
//    }
//
//    val semanticType =
//        cmp_sceneSegFct_type :&: sd_poly_scene_cell_segmentation_var :&: dimensionality_var =>:
//        cmp_cell_graph_fct :&: rmc_cellNodeAddFct_var :&: rmc_startGoalFct_var :&: rmc_usingCentroids_var :&:
//          rmc_centroidFct_var :&: sd_cell_type_var :&: rmc_cellGraph_var :&: rmc_connectorNodes_var :&:
//          dimensionality_var =>:
//        cmp_graph_algorithm_var =>:
//        cmp_algorithm_type :&: cmp_graph_algorithm_var :&: rmc_connectorNodes_var :&: rmc_centroidFct_var :&:
//          rmc_cellGraph_var :&: sd_cell_type_var :&: sd_poly_scene_cell_segmentation_var :&: dimensionality_var :&:
//          rmc_cellNodeAddFct_var :&: rmc_startGoalFct_var :&: rmc_usingCentroids_var
//  }

  trait CmpTopLevelCombinatorFileBasedRmSegTrait{
    def apply(problemToRm: ProblemDefinitionFiles => Graph[List[Float], WUnDiEdge],
              findPath: (Graph[List[Float], WUnDiEdge], MpTaskStartGoal) => Seq[List[Float]]):
    ProblemDefinitionFiles => List[List[Float]] = { pdef:ProblemDefinitionFiles =>
      val rm = problemToRm(pdef)
      val startGoal = readMpStartGoalFromProperties(pdef.problemProperties)
      findPath(rm, startGoal).toList
    }
    val semanticType: Type
  }

  @combinator object CmpTopLevelCombinatorFileBasedRmSeg extends CmpTopLevelCombinatorFileBasedRmSegTrait{
    val semanticType =
      cmp_sceneSegFct_type :&: cmp_cell_graph_fct_type :&: sd_poly_scene_cell_segmentation_var :&: dimensionality_var :&:
        rmc_cellNodeAddFct_var :&: rmc_startGoalFct_var :&: rmc_usingCentroids_var :&:
        rmc_centroidFct_var :&: sd_cell_type_var :&: rmc_cellGraph_var :&: rmc_connectorNodes_var =>:
        cmp_graph_algorithm_var =>:
        cmp_algorithm_type :&: cmp_graph_algorithm_var :&: rmc_connectorNodes_var :&: rmc_centroidFct_var :&:
          rmc_cellGraph_var :&: sd_cell_type_var :&: sd_poly_scene_cell_segmentation_var :&: dimensionality_var :&:
          rmc_cellNodeAddFct_var :&: rmc_startGoalFct_var :&: rmc_usingCentroids_var
  }

  @combinator object CmpTopLevelCombinatorFileBasedRmSegTax extends CmpTopLevelCombinatorFileBasedRmSegTrait{
    val semanticType = cmp_sceneSegFct_type =>: cmp_any_graph_algorithm_type =>: cmp_any_algorithm_type
  }
}
