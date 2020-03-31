package org.combinators.ctp.repositories.toplevel

import com.typesafe.scalalogging.LazyLogging
import org.combinators.cls.interpreter.combinator
import org.combinators.cls.types.Constructor
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.mptasks.MpTaskStartGoal
import org.combinators.ctp.repositories.scene.{PolySceneSegmentationGraph, _}
import org.locationtech.jts.algorithm.ConvexHull
import org.locationtech.jts.geom.impl.CoordinateArraySequence
import org.locationtech.jts.geom.{Coordinate, GeometryFactory, LineString}
import scalax.collection.Graph
import scalax.collection.edge.WUnDiEdge

trait CmpTopLevel extends LazyLogging {
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
        (sd_polygon_scene_type =>: sd_polygon_scene_type :&: sd_scene_segmentation :&: sd_seg_lines) =>:
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
                Seq[List[Float]]):
    ((Scene, MpTaskStartGoal) => PolySceneSegmentationGraphPath) = { (scene: Scene, mpt: MpTaskStartGoal) =>
      val centroidSegmentation = toCentroids(toCellSegmentation(run(transformToPoly(scene))))
      println("computed segmentation and centroids")

      val graph = toGraph(centroidSegmentation)
      println("graph built")
      val segGraph = PolySceneSegmentationGraph(centroidSegmentation.vertices, centroidSegmentation.obstacles,
        centroidSegmentation.boundaries, centroidSegmentation.freeCells, centroidSegmentation.centroids, graph)
      println(s"mpt: ${mpt}")
      val graphSeg = addGraph(segGraph, mpt)
      println(s"Graph refinement: ${graphSeg.graph}")
      val path = toGraphPathFct(graphSeg.graph, mpt)

      println(s"path._1 ${path}")


      graphSeg.withPath(path)
/*      PolySceneSegmentationGraphPath(centroidSegmentation.vertices, centroidSegmentation.obstacles,
        centroidSegmentation.boundaries, centroidSegmentation.freeCells, graphSeg.graph, path)*/
    }

    val semanticType =
      (sd_unity_scene_type =>: sd_polygon_scene_type) :&:dimensionality_two_d_t =>:
        (sd_polygon_scene_type =>: sd_polygon_scene_type :&: sd_scene_segmentation :&:sd_seg_lines) =>:
        (sd_seg_lines :&: sd_poly_scene_segmentation =>: sd_seg_cells :&: sd_poly_scene_segmentation) =>:
        (sd_poly_scene_segmentation :&: sd_seg_cells =>: sd_poly_scene_segmentation :&: sd_seg_centroid_cells) =>:
        (sd_poly_scene_segmentation :&: sd_seg_centroid_cells =>: cmp_cell_graph) =>:
        cmp_graph_vcd_gaddFct_type =>:
        cmp_graph_shortest_path_var =>:
        (sd_unity_scene_type :&: mpt_start_goal_position_type :&: dimensionality_two_d_t =>:
          cmp_scene_graph_path :&: dimensionality_two_d_t) :&: cmp_vertical_cell_decomposition_type :&: cmp_graph_shortest_path_var
  }


//  @combinator object SceneShortestPathArrows {
//    def apply(transformToPoly: (Scene, MpTaskStartGoal) => (Seq[List[Float]], Seq[WUnDiEdge[List[Float]]], Float), run: PolygonScene => PolySceneLineSegmentation,
//              toCellSegmentation: PolySceneLineSegmentation => PolySceneCellSegmentation,
//              toCentroids: PolySceneCellSegmentation => PolySceneCellSegmentationCentroids,
//              toGraph: PolySceneCellSegmentationCentroids => Graph[List[Float], WUnDiEdge],
//              addGraph: (PolySceneSegmentationGraph, MpTaskStartGoal) => PolySceneSegmentationGraph,
//              toGraphPathFct: (Graph[List[Float], WUnDiEdge],MpTaskStartGoal) =>
//                (Seq[List[Float]], Seq[WUnDiEdge[List[Float]]], Float)):
//    ((Scene, MpTaskStartGoal) => PolySceneSegmentationGraphPath) = { (scene: Scene, mpt: MpTaskStartGoal) =>
//      val centroidSegmentation = toCentroids(toCellSegmentation(run(transformToPoly(scene))))
//      println("computed segmentation and centroids")
//
//      val graph = toGraph(centroidSegmentation)
//      println("graph built")
//      val segGraph = PolySceneSegmentationGraph(centroidSegmentation.vertices, centroidSegmentation.obstacles,
//        centroidSegmentation.boundaries, centroidSegmentation.freeCells, centroidSegmentation.centroids, graph)
//      println(s"mpt: ${mpt}")
//      val graphSeg = addGraph(segGraph, mpt)
//      println(s"Graph refinement: ${graphSeg.graph}")
//      val path = toGraphPathFct(graphSeg.graph, mpt)
//
//      println(s"path._1 ${path._1}")
//      println(s"path._2 ${path._2}")
//      println(s"path._3 ${path._3}")
//
//      graphSeg.withPath(path)
///*      PolySceneSegmentationGraphPath(centroidSegmentation.vertices, centroidSegmentation.obstacles,
//        centroidSegmentation.boundaries, centroidSegmentation.freeCells, graphSeg.graph, path)*/
//    }
//
//    val semanticType =
//      (sd_unity_scene_type =>: sd_polygon_scene_type) :&:dimensionality_two_d_t =>:
//        (sd_polygon_scene_type =>: sd_polygon_scene_type :&: sd_scene_segmentation :&:sd_seg_lines) =>:
//        (sd_seg_lines :&: sd_poly_scene_segmentation =>: sd_seg_cells :&: sd_poly_scene_segmentation) =>:
//        (sd_poly_scene_segmentation :&: sd_seg_cells =>: sd_poly_scene_segmentation :&: sd_seg_centroid_cells) =>:
//        (sd_poly_scene_segmentation :&: sd_seg_centroid_cells =>: cmp_cell_graph) =>:
//        cmp_graph_vcd_gaddFct_type =>:
//        cmp_graph_shortest_path_var =>:
//        (sd_unity_scene_type :&: mpt_start_goal_position_type :&: dimensionality_two_d_t =>:
//          cmp_scene_graph_path :&: dimensionality_two_d_t) :&: cmp_vertical_cell_decomposition_type :&: cmp_graph_shortest_path_var
//  }


  @combinator object SceneTaskToGraphPathFctTriangulation {
    def apply(toPolygonScene: Scene => PolygonScene,
              run: PolygonScene => TriangleSeg,
              toCentroids: TriangleSeg => TriangleSegCentroids,
              toGraph: TriangleSegCentroids => Graph[List[Float], WUnDiEdge],
              gGraphAdd: (Graph[List[Float], WUnDiEdge], MpTaskStartGoal) => Graph[List[Float], WUnDiEdge],
              toGraphPathFct: (Graph[List[Float], WUnDiEdge], MpTaskStartGoal) => Seq[List[Float]]):
    (Scene, MpTaskStartGoal) => TriangleSegPath = { (scene: Scene, mpt: MpTaskStartGoal) =>
      val pScene = toPolygonScene(scene)
      println("scene transformed")

      val centroidSegmentation = toCentroids(run(pScene))
      println("computed segmentation and centroids")

     val graph = gGraphAdd(toGraph(centroidSegmentation), mpt)
   //   val graph = toGraph(centroidSegmentation)
      println("graph built and refined")

      println(s"mpt: ${mpt}")
      println(s"addGraph: ${graph}")
      val path: Seq[List[Float]] = toGraphPathFct(graph, mpt)
      println(s"path._1 ${path}")

      val v = TriangleSegPath(centroidSegmentation.vertices, centroidSegmentation.triangles, graph, path)

      println(s"obstacles :${pScene.obstacles}")
      println(s"vertices :${pScene.vertices}")
      println(s"vertices :${pScene.obstacles.map(i => i.map(pScene.vertices))}")


      def scenePathRefinement(s: TriangleSegPath): TriangleSegPath = {
        val vertexLists = pScene.obstacles.map(i => i.map(pScene.vertices))
        val hulls = vertexLists.map { actualCoords =>
          // generate convex hull for current batch of points
          val coords = actualCoords.map(c => new Coordinate(c(0), c(1)))
          val convexHull = new ConvexHull(coords.toArray, new GeometryFactory)
          val hullGeometry = convexHull.getConvexHull
          hullGeometry
        }

        println(s"Hulls size: ${hulls.size}")
        println(s"Hulls: $hulls")
        println("Hulls computed")

        def cFreeLine(p1:Coordinate, p2:Coordinate):Boolean = {
          val lineGeo = new LineString(new CoordinateArraySequence(Array(p1,p2)), new GeometryFactory())
          val b = !hulls.exists(_.intersects(lineGeo))
          println(s"Freeline: $p1, $p2: $b")
          b
        }

        def getList(l: Seq[Coordinate], startIndex: Int, targetIndex: Int): Seq[Coordinate] = {
          println(s"${l.size} startIndex: $startIndex, targetIndex: $targetIndex")
          if (targetIndex >= l.size)
            if(startIndex >= l.size)
              return l
            else
              return getList(l, startIndex + 1, startIndex + 3)

          println("^_^")
          if (cFreeLine(l(startIndex), l(targetIndex))) {
            println(s"l size: ${l.size}, takeRight: ${l.size - targetIndex}")
            println("d")
            val foo = l.take(startIndex+1)
            val newList = foo ++ l.takeRight(l.size - targetIndex)
            println("ff")
            getList(newList, startIndex, startIndex + 2)
          } else {
            println("asdasdasd")
            val asd = getList(l, startIndex, targetIndex + 1)
            println("00")
            asd
          }
        }

        val coordList = s.gpath.map(i => new Coordinate(i(0), i(1)))
        println("coordList computed")
        val asdList = getList(coordList, 0, 2)
        println("asdList computed")

        val p = asdList.map(i=> List(i.x.toFloat, i.y.toFloat))
        println("p: asdList mapped")
        s.withPath(p)
      }

      scenePathRefinement(v)
    }

    //TODO CHECK
    val semanticType =
      (sd_unity_scene_type =>: sd_polygon_scene_type) :&: dimensionality_two_d_t =>:
        (sd_polygon_scene_type =>: sd_polygon_scene_type :&: sd_scene_segmentation :&: sd_seg_cells :&: cmp_scene_triangulation_parameters) =>:
        triangle_centroidsFct_type =>:
        triangle_gbuildNd_type =>:
        triangle_gRefine_type =>:
        cmp_graph_shortest_path_var =>:
        triangulation_path_prog_type :&: cmp_scene_triangulation_parameters :&: mpt_start_goal_position_type :&:cmp_graph_shortest_path_var
  }


  @combinator object SceneTaskToGraphPathFctTriangulationMst {
    def apply(toPolygonScene: Scene => PolygonScene,
              run: PolygonScene => TriangleSeg,
              toCentroids: TriangleSeg => TriangleSegCentroids,
              toGraph: TriangleSegCentroids => Graph[List[Float], WUnDiEdge],
              //gGraphAdd: (Graph[List[Float], WUnDiEdge], MpTaskStartGoal) => Graph[List[Float], WUnDiEdge],
              toGraphPathFct: (Graph[List[Float], WUnDiEdge]) =>
                Seq[List[Float]]):
    Scene => TriangleSegPath = { (scene: Scene) =>
      val pScene = toPolygonScene(scene)
      println("Scene transformed")

      val centroidSegmentation = toCentroids(run(pScene))
      println("computed segmentation and centroids")

   //   val graph = gGraphAdd(toGraph(centroidSegmentation), mpt)
      val graph = toGraph(centroidSegmentation)
      println("graph built and refined")

      println(s"addGraph: ${graph}")
      val path: Seq[List[Float]] = toGraphPathFct(graph)

      println(s"path._1 ${path}")


      TriangleSegPath(centroidSegmentation.vertices, centroidSegmentation.triangles, graph, path)
    }

    val semanticType =
      (sd_unity_scene_type =>: sd_polygon_scene_type) :&: dimensionality_two_d_t =>:
        (sd_polygon_scene_type =>: sd_polygon_scene_type :&: sd_scene_segmentation :&: sd_seg_cells :&: cmp_scene_triangulation_parameters) =>:
        triangle_centroidsFct_type =>:
        triangle_gbuildNd_type =>:
       // triangle_gRefine_type =>: //TODO q&d
        //cmp_graph_dijkstra_type =>:  //TODO quick and dirty
        cmp_graph_mst_type =>:
        triangulation_path_prog_type :&: cmp_scene_triangulation_parameters :&: cmp_graph_mst_type
  }

  @combinator object SceneTaskToGraphPathFctTriangulationTsp {
    def apply(toPolygonScene: Scene => PolygonScene,
              run: PolygonScene => TriangleSeg,
              toCentroids: TriangleSeg => TriangleSegCentroids,
              toGraph: TriangleSegCentroids => Graph[List[Float], WUnDiEdge],
              //gGraphAdd: (Graph[List[Float], WUnDiEdge], MpTaskStartGoal) => Graph[List[Float], WUnDiEdge],
              toGraphPathFct: (Graph[List[Float], WUnDiEdge], MpTaskStartGoal) => Seq[List[Float]]):
    (Scene, MpTaskStartGoal) => TriangleSegPath = { (scene: Scene, mpTaskStartGoal: MpTaskStartGoal) =>
      val pScene = toPolygonScene(scene)
      println("Scene transformed")

      val centroidSegmentation = toCentroids(run(pScene))
      println("computed segmentation and centroids")

   //   val graph = gGraphAdd(toGraph(centroidSegmentation), mpt)
      val graph = toGraph(centroidSegmentation)
      println("graph built and refined")

      println(s"addGraph: ${graph}")
      val path: Seq[List[Float]] = toGraphPathFct(graph, mpTaskStartGoal)

      println(s"path: ${path}")


      TriangleSegPath(centroidSegmentation.vertices, centroidSegmentation.triangles, graph, path)
    }

    val semanticType =
      (sd_unity_scene_type =>: sd_polygon_scene_type) :&: dimensionality_two_d_t =>:
        (sd_polygon_scene_type =>: sd_polygon_scene_type :&: sd_scene_segmentation :&: sd_seg_cells :&: cmp_scene_triangulation_parameters) =>:
        triangle_centroidsFct_type =>:
        triangle_gbuildNd_type =>:
       // triangle_gRefine_type =>: //TODO q&d
        //cmp_graph_dijkstra_type =>:  //TODO quick and dirty
        Constructor("graphTsp") =>:
        triangulation_path_prog_type :&: cmp_scene_triangulation_parameters :&:  Constructor("graphTsp")

  }


  @combinator object SceneTaskToGraphPathFctTetrahedralization {
    def apply(toPolygonScene: Scene => PolygonScene,
              run: PolygonScene => TriangleSeg,
              toCentroids: TriangleSeg => TriangleSegCentroids,
              toGraph: TriangleSegCentroids => Graph[List[Float], WUnDiEdge],
              gGraphAdd: (Graph[List[Float], WUnDiEdge], MpTaskStartGoal) => Graph[List[Float], WUnDiEdge],
              toGraphPathFct: (Graph[List[Float], WUnDiEdge],MpTaskStartGoal) => Seq[List[Float]]):
    (Scene, MpTaskStartGoal) => TriangleSegPath = { (scene: Scene, mpt: MpTaskStartGoal) =>
      println(s"starting. mpTask: ${mpt.startPosition} to ${mpt.endPosition}")
      val pScene = toPolygonScene(scene)
      println("scene transformed")

      val centroidSegmentation = toCentroids(run(pScene))
      println("computed segmentation and centroids")

      val graph = gGraphAdd(toGraph(centroidSegmentation), mpt)
      println("graph built and refined")

      println(s"mpt: ${mpt}")
      println(s"addGraph: ${graph}")
      val path: Seq[List[Float]] = toGraphPathFct(graph, mpt)

      println(s"3d path._1 ${path}")

      TriangleSegPath(centroidSegmentation.vertices, centroidSegmentation.triangles, graph, path)
    }

    val semanticType =
      (sd_unity_scene_type =>: sd_polygon_scene_type) :&: dimensionality_three_d_t =>:
        (sd_polygon_scene_type =>: sd_polygon_scene_type :&: sd_scene_segmentation :&: sd_seg_cells) :&: dimensionality_three_d_t =>:
        triangle_centroidsFctNd_type =>:
        triangle_gbuildNdFast_type =>:
        triangle_gRefine_type =>:
        cmp_graph_shortest_path_var =>:
        cmp_sd_tetrahedra_type :&: cmp_graph_shortest_path_var
  }

  @combinator object SceneSample1 {
    def apply(run: String => Unit):
    (SceneSRT, MpTaskStartGoal) => List[List[Float]] = { (scene: SceneSRT, mpt: MpTaskStartGoal) =>
      println(s"starting. mpTask: ${mpt.startPosition} to ${mpt.endPosition}")
      println("fasfasd")
      val pScene: String = new SceneUtils{}.sceneSRTtoFclPythonString(scene)
      run(pScene)
      println("run, returning empty List")
      List.empty[List[Float]]
      //Samplers, return Path
    }

    val semanticType =
      Constructor("runSample") =>:
        Constructor("sampleCombinatorTop")//TODO Planner, sampler info in type expression
  }


@combinator object RunSampleBased{
  // Welcher sampler, Welcher Planner, welche configspace? Wo wird der wie definiert?
  // Über Problembeschreibung verschiedene vordefinierte probleme oder fn: configspace => collision
  def apply: String => Unit = (sceneString: String) => print(s"RunSampleBased sceneString: $sceneString")
  val semanticType = Constructor("runSample")
}


}
