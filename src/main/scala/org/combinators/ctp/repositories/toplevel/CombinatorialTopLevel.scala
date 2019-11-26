package org.combinators.ctp.repositories.toplevel

import com.typesafe.scalalogging.LazyLogging

import org.combinators.cls.interpreter.combinator
import org.combinators.cls.types.Constructor
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.mptasks.MpTaskStartGoal
import org.combinators.ctp.repositories.scene.{PolySceneSegmentationGraph, _}
import scalax.collection.Graph
import scalax.collection.edge.WUnDiEdge

trait CombinatorialTopLevel extends LazyLogging {
  @combinator object SceneToSegGraphFunction {
    def apply(transformToPoly: Scene => PolygonScene,
              run: PolygonScene => PolySceneLineSegmentation,
              toCellSegmentation: PolySceneLineSegmentation => PolySceneCellSegmentation,
              toCentroids: PolySceneCellSegmentation => PolySceneCellSegmentationCentroids,
              toGraph: PolySceneCellSegmentationCentroids => Graph[List[Float], WUnDiEdge]):
    (Scene => PolySceneSegmentationGraph) = { scene: Scene =>
      val centroidSegmentation = toCentroids(toCellSegmentation(run(transformToPoly(scene))))
      val graph = toGraph(centroidSegmentation)
      PolySceneSegmentationGraph(centroidSegmentation.vertices, centroidSegmentation.obstacles,
        centroidSegmentation.boundaries, centroidSegmentation.freeCells, centroidSegmentation.centroids, graph)
    }

    val semanticType =
      (sd_unity_scene_type =>: sd_polygon_scene_type) =>:
        (sd_polygon_scene_type =>: sd_polygon_scene_type :&: sd_scene_segmentation) =>:
        (sd_seg_lines :&: sd_poly_scene_segmentation =>: sd_seg_cells :&: sd_poly_scene_segmentation) =>:
        (sd_poly_scene_segmentation :&: sd_seg_cells =>: sd_poly_scene_segmentation :&: sd_seg_centroid_cells) =>:
        (sd_poly_scene_segmentation :&: sd_seg_centroid_cells =>: cmp_cell_graph) =>:
        (sd_unity_scene_type :&: dimensionality_two_d_t =>: cmp_scene_graph :&: dimensionality_two_d_t)
  }

  @combinator object SceneTaskToGraphPathFct {
    def apply(transformToPoly: Scene => PolygonScene, run: PolygonScene => PolySceneLineSegmentation,
              toCellSegmentation: PolySceneLineSegmentation => PolySceneCellSegmentation,
              toCentroids: PolySceneCellSegmentation => PolySceneCellSegmentationCentroids,
              toGraph: PolySceneCellSegmentationCentroids => Graph[List[Float], WUnDiEdge],
              addGraph: (PolySceneSegmentationGraph, MpTaskStartGoal) => PolySceneSegmentationGraph,
              toGraphPathFct: (Graph[List[Float], WUnDiEdge],MpTaskStartGoal) =>
                (Seq[List[Float]], Seq[WUnDiEdge[List[Float]]], Float)):
    ((Scene, MpTaskStartGoal) => PolySceneSegmentationGraphPath) = { (scene: Scene, mpt: MpTaskStartGoal) =>
      val centroidSegmentation = toCentroids(toCellSegmentation(run(transformToPoly(scene))))
      println("topl 0")

      val graph = toGraph(centroidSegmentation)
      println("topl 1")
      val segGraph = PolySceneSegmentationGraph(centroidSegmentation.vertices, centroidSegmentation.obstacles,
        centroidSegmentation.boundaries, centroidSegmentation.freeCells, centroidSegmentation.centroids, graph)
      println("topl 2")
      println(s"mpt: ${mpt}")
      val graphSeg = addGraph(segGraph, mpt)
      println(s"addGraph: ${graphSeg.graph}")
      val path = toGraphPathFct(graphSeg.graph, mpt)

      println(s"path._1 ${path._1}")
      println(s"path._2 ${path._2}")
      println(s"path._3 ${path._3}")
      println("topl 3")

      graphSeg.withPath(path)
/*      PolySceneSegmentationGraphPath(centroidSegmentation.vertices, centroidSegmentation.obstacles,
        centroidSegmentation.boundaries, centroidSegmentation.freeCells, graphSeg.graph, path)*/
    }

    val semanticType =
      (sd_unity_scene_type =>: sd_polygon_scene_type) =>:
        (sd_polygon_scene_type =>: sd_polygon_scene_type :&: sd_scene_segmentation) =>:
        (sd_seg_lines :&: sd_poly_scene_segmentation =>: sd_seg_cells :&: sd_poly_scene_segmentation) =>:
        (sd_poly_scene_segmentation :&: sd_seg_cells =>: sd_poly_scene_segmentation :&: sd_seg_centroid_cells) =>:
        (sd_poly_scene_segmentation :&: sd_seg_centroid_cells =>: cmp_cell_graph) =>:
        Constructor("graphAdd") =>:
        Constructor("graphTraversalFct") =>:
        (sd_unity_scene_type :&: mpt_start_goal_position_type :&: dimensionality_two_d_t =>:
          cmp_scene_graph_path :&: dimensionality_two_d_t)
  }
}
