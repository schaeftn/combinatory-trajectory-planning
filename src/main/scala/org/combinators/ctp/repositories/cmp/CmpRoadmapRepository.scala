package org.combinators.ctp.repositories.cmp

import org.combinators.cls.interpreter.combinator
import org.combinators.cls.types.{Constructor, Variable}
import org.combinators.ctp.repositories.scene.{PolySceneCellSegmentation, PolySceneCellSegmentationCentroids, PolySceneSegmentationRoadmap}
import org.combinators.ctp.repositories.{triangle_gbuildNdFast_type, triangle_gbuildNd_type}
import scalax.collection.Graph
import scalax.collection.edge.WUnDiEdge
import org.combinators.cls.interpreter._
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.mptasks.MpTaskStartGoal

trait CmpRoadmapRepository extends CmpUtils {
  //TODO changes Centroid to Normal
  @combinator object RoadmapCombinator {
    def apply(
               initRm: (PolySceneCellSegmentation, neighbourCellsNativeType) => PolySceneSegmentationRoadmap,
               refineRm: (PolySceneSegmentationRoadmap, neighbourCellsNativeType) => PolySceneSegmentationRoadmap,
               addStartEnd: (PolySceneSegmentationRoadmap, neighbourCellsNativeType, MpTaskStartGoal) =>
                 PolySceneSegmentationRoadmap):
    (PolySceneCellSegmentation, MpTaskStartGoal) => PolySceneSegmentationRoadmap = {
      (scsc, startGoal) => {
        val neighbours = getNeighbours(scsc)
        val r1 = initRm(scsc, neighbours)
        val r2 = refineRm(r1, neighbours)
        val r3 = addStartEnd(r2, neighbours, startGoal)
        r3
      }
    }

    val semanticType = rmc_initFct_type :&: rmc_cellGraph_var =>:
      rmc_connectorNodeFct_type :&: rmc_cellGraph_var :&: rmc_connectorNodes_var :&: dimensionality_var =>:
      rmc_startGoalAddFct_type :&: rmc_cellGraph_var :&: dimensionality_var =>:
      cmp_cell_graph_fct :&: rmc_cellGraph_var :&: rmc_connectorNodes_var :&: dimensionality_var
  }

  @combinator object RmInitCentroidsOnly {
    def apply(): (PolySceneCellSegmentationCentroids, neighbourCellsNativeType) => PolySceneSegmentationRoadmap =
      (csc, neighbours) => initRoadmapCentroids(csc, neighbours)

    val semanticType = rmc_initFct_type :&: rmc_cg_centroidsOnly
  }

  @combinator object RmInitAllCellVertices {
    def apply(): (PolySceneCellSegmentationCentroids, neighbourCellsNativeType) => PolySceneSegmentationRoadmap =
      (pscsc, _) => initRoadmapAllCellVertices(pscsc)

    val semanticType = rmc_initFct_type :&: rmc_cg_allVertices
  }

  @combinator object RmInitCentroidCellVertices {
    def apply(): (PolySceneCellSegmentationCentroids, neighbourCellsNativeType) => PolySceneSegmentationRoadmap =
      (pscsc, _) => initRoadmapCentroidsToVertices(pscsc)

    val semanticType = rmc_initFct_type :&: rmc_cg_centroidCellVertices
  }

  @combinator object a2noConnectorNodes {
    def apply(): (PolySceneSegmentationRoadmap, neighbourCellsNativeType) => PolySceneSegmentationRoadmap =
      (srm, _) => srm

    val semanticType = rmc_connectorNodeFct_type :&: rmc_cn_withoutConnectorNodes :&: rmc_cellGraph_var :&: dimensionality_var
  }

  @combinator object a2_allVertices {
    def apply(): (PolySceneSegmentationRoadmap, neighbourCellsNativeType) => PolySceneSegmentationRoadmap =
      (srm, neighbours) => addConnectorNodesAllCellVertexEdges(srm, neighbours)

    val semanticType = rmc_connectorNodeFct_type :&: rmc_cn_withConnectorNodes :&: rmc_cg_allVertices :&: dimensionality_var
  }

  @combinator object a2_centroidCellVertices {
    def apply(): (PolySceneSegmentationRoadmap, neighbourCellsNativeType) => PolySceneSegmentationRoadmap =
      (srm, neighbours) => addConnectorNodesCentroidEdges(srm, neighbours)

    val semanticType = rmc_connectorNodeFct_type :&: rmc_cn_withConnectorNodes :&: rmc_cg_centroidCellVertices :&: dimensionality_var
  }

  @combinator object a3_centroidOnly {
    def apply():
    (PolySceneSegmentationRoadmap, neighbourCellsNativeType, MpTaskStartGoal) => PolySceneSegmentationRoadmap =
      (srm, _, startGoal) => addStartGoalCentroidsOnly(srm, startGoal)

    val semanticType = rmc_startGoalAddFct_type :&: rmc_cg_centroidsOnly :&: dimensionality_two_d_t
  }

  @combinator object a3_allCellVerts {
    def apply():
    (PolySceneSegmentationRoadmap, neighbourCellsNativeType, MpTaskStartGoal) => PolySceneSegmentationRoadmap =
      (srm, _, startGoal) =>
        addNodesStartEndToRoadmapAllVertices(srm, startGoal)

    val semanticType =
      rmc_startGoalAddFct_type :&: rmc_cg_allVertices :&: dimensionality_two_d_t
  }

  @combinator object a3_allCellVertsCentroid {
    def apply():
    (PolySceneSegmentationRoadmap, neighbourCellsNativeType, MpTaskStartGoal) => PolySceneSegmentationRoadmap =
      (srm, _, startGoal) => addStartGoalCentroidsOnly(srm, startGoal) //TODO Cp prüfen

    val semanticType =
      rmc_startGoalAddFct_type :&: rmc_cg_centroidCellVertices :&: dimensionality_two_d_t
  }
}
