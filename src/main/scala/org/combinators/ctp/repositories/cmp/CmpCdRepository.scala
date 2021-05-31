package org.combinators.ctp.repositories.cmp

import io.circe.parser.decode
import io.circe.generic.auto._
import io.circe.syntax._
import org.combinators.cls.interpreter._
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.geometry.{PpAaBb2D, PpVertexList}
import org.combinators.ctp.repositories.python_interop.{PythonTemplateUtils, PythonWrapper, SubstitutionSchema}
import org.combinators.ctp.repositories.toplevel.{CellSegmentation, MpTaskStartGoal, PolySceneCellSegmentation, PolySceneLineSegmentation, PolySceneSegmentationRoadmap, PolygonScene, ProblemDefinitionFiles, RmInput}
import org.combinators.cls.interpreter._
import org.combinators.cls.types.Constructor
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.scene.SceneUtils
import org.locationtech.jts.algorithm.ConvexHull
import org.locationtech.jts.geom.{Coordinate, Geometry, GeometryFactory, LineSegment, Point}
import scalax.collection.Graph
import scalax.collection.edge.WUnDiEdge
import scala.collection.parallel.ParSeq


trait CmpCdRepository extends PythonTemplateUtils with CmpUtils with SceneUtils {
  // Pymesh without parameters; only adds cfree triangles
  @combinator object TriangulatePoly {
    def apply: PolygonScene => PolySceneCellSegmentation = { polyScene: PolygonScene =>
      def pythonSceneString(scene: PolygonScene): String = {
        val obstacleString = (for (i <- scene.obstacles.indices) yield s"b$i").reduce((a, b) => a + ", " + b)
        val vlist = scene.obstacles.map { i => i.map(scene.vertices).map(listToPythonArray) }.map(listToPythonArray)

        scene.obstacles.indices.foreach(i => println("i print: " + scene.obstacles(i).toString()))
        val object_instances = scene.obstacles.indices.map(i =>
          s"""    b$i = SceneObjectCHull2D(${vlist(i)})""").reduce(_ + "\n" + _)

        s"$object_instances\n" +
          s"    scene_objects = [$obstacleString]\n" +
          s"    scene_size = [${scene.boundaries.mkString(", ")}]"
      }

      val fileMap = Map(cdTemplateLocationTri -> cdStartLocationTri)
      val substMap = Map("$substitute$" -> pythonSceneString(polyScene))
      val t = SubstitutionSchema(fileMap, substMap)

      val pWrapper = PythonWrapper.apply(t, cdStartLocationTri, decodeCellSegmentationFct)
      pWrapper.computeResult(polyScene)
    }

    val semanticType = cmp_sceneSegFct_type :&: sd_seg_triangles_simple_type :&: dimensionality_two_d_t
  }

  @combinator object TriangulatePolyParametrized {
    def apply: PolygonScene => PolySceneCellSegmentation = { polyScene: PolygonScene =>
      def pythonSceneString(scene: PolygonScene): String = {
        val obstacleString = (for (i <- scene.obstacles.indices) yield s"b$i").reduce((a, b) => a + ", " + b)
        val vlist = scene.obstacles.map { i => i.map(scene.vertices).map(listToPythonArray) }.map(listToPythonArray)
        val object_instances = scene.obstacles.indices.map(i =>
          s"""    b$i = SceneObjectCHull2D(${vlist(i)})""").reduce(_ + "\n" + _)

        s"$object_instances\n" +
          s"    scene_objects = [$obstacleString]\n" +
          s"    scene_size = [${scene.boundaries.mkString(", ")}]"
      }

      val fileMap = Map(cdTemplateLocationTriPara -> cdStartLocationTriPara)
      val substMap = Map("$substitute$" -> pythonSceneString(polyScene))
      val t = SubstitutionSchema(fileMap, substMap)

      val pWrapper = PythonWrapper.apply(t, cdStartLocationTriPara, decodeCellSegmentationFct)
      pWrapper.computeResult(polyScene)
    }

    val semanticType = cmp_sceneSegFct_type :&: sd_seg_triangles_para_type :&: dimensionality_two_d_t
  }

  @combinator object TetPoly {
    def apply: PolygonScene => PolySceneCellSegmentation = { polyScene: PolygonScene =>
      def pythonSceneString(scene: PolygonScene): String = {
        val obstacleString = (for (i <- scene.obstacles.indices) yield s"b$i").reduce((a, b) => a + ", " + b)
        val vlist = scene.obstacles.map { i => i.map(scene.vertices).map(listToPythonArray) }.map(listToNpArray)

        scene.obstacles.indices.foreach(i => println("i print: " + scene.obstacles(i).toString()))
        val object_instances = scene.obstacles.indices.map(i =>
          s"""    b$i = SceneObjectBox3D(${vlist(i)})""").reduce(_ + "\n" + _)

        s"$object_instances\n" +
          s"    scene_objects = [$obstacleString]\n" +
          s"    scene_size = [${scene.boundaries.mkString(", ")}]"
      }

      val fileMap = Map(cdTemplateLocationTet -> cdStartLocationTet)
      val substMap = Map("$substitute$" -> pythonSceneString(polyScene))
      val t = SubstitutionSchema(fileMap, substMap)

      val pWrapper = PythonWrapper.apply(t, cdStartLocationTet, decodeCellSegmentationFct)
      pWrapper.computeResult(polyScene)
    }

    val semanticType = cmp_sceneSegFct_type :&: sd_seg_triangles_simple_type :&: sd_seg_triangles_para_type :&: dimensionality_three_d_t
  }

  @combinator object TetFile {
    def apply: ProblemDefinitionFiles => PolySceneCellSegmentation = { pdef: ProblemDefinitionFiles =>
      val fileMap = Map(cdTemplateLocationTetFileBased -> cdStartLocationTetFileBased)
      val substMap = Map("$run_tetrahedralization_file.envFile$" -> s""""${pdef.envModelLocation}"""",
        "$run_tetrahedralization_file.boxminmax$" -> getBoxMinMaxString(pdef.problemProperties))
      val t = SubstitutionSchema(fileMap, substMap)

      def decodeCellSegmentationFile: String => PolySceneCellSegmentation = {
        resultString: String =>
          val segmentation = decode[CellSegmentation](resultString) match {
            case Left(_) =>
              println(s"Error while decoding")
              println(s"$resultString")
              CellSegmentation(List.empty[List[Float]], List.empty[List[Int]])
            case Right(s) => s
          }
          PolygonScene(List.empty[List[Float]], List.empty[List[Int]], List.empty[Float]).
            withVertices(segmentation.vertices).withFreeCells(segmentation.cells)
      }

      val pWrapper = PythonWrapper.apply(t, cdStartLocationTetFileBased, decodeCellSegmentationFile)
      pWrapper.computeResult
    }

    val semanticType = cmp_sceneSegFct_type :&: sd_seg_triangles_simple_type :&: sd_seg_triangles_para_type :&: dimensionality_three_d_t
  }

  @combinator object TetFileRm {
    def apply(addStartEnd: (PolySceneCellSegmentation, IndexedSeq[RmAuxDataNodes], MpTaskStartGoal) =>
      (List[List[Float]], List[WUnDiEdge[List[Float]]])):
    ProblemDefinitionFiles => Graph[List[Float], WUnDiEdge] = { pdef: ProblemDefinitionFiles =>
      val fileMap = Map(cdTemplateLocationTetFileBasedRm -> cdStartLocationTetFileBasedRm)
      val substMap = Map("$run_tetrahedralization_file.envFile$" -> s""""${pdef.envModelLocation}"""",
        "$run_tetrahedralization_file.boxminmax$" -> getBoxMinMaxString(pdef.problemProperties))
      val t = SubstitutionSchema(fileMap, substMap)

      def decodeCellSegmentationFile: String => RmInput = {
        resultString: String =>
          decode[RmInput](resultString) match {
            case Left(_) =>
              println(s"RmInput: Error while decoding")
              println(s"$resultString")
              RmInput(List.empty[List[Float]], List.empty[List[Int]])
            case Right(s) => s
          }
      }

      val pWrapper = PythonWrapper.apply(t, cdStartLocationTetFileBasedRm, decodeCellSegmentationFile)
      val rmInput = pWrapper.computeResult
      val edgeTuples = rmInput.adjacency.zipWithIndex.flatMap { case (a, b) => a.map(i => (b, i)) }
      val edgeSet = edgeTuples.map(i => WUnDiEdge(rmInput.centroids(i._1), rmInput.centroids(i._2))(distance(rmInput.centroids(i._1), rmInput.centroids(i._2)))).toSet
      val auxData = rmInput.centroids.zipWithIndex.map {
        case (c, index) => new RmAuxDataNodes {
          override val cellId: Int = index
          override val centroid: List[Float] = c
          override val vertices: List[List[Float]] = List.empty
          override val connxPoints: List[List[Float]] = List.empty
        }
      }.toIndexedSeq
      val (vList, eList) = addStartEnd(PolySceneCellSegmentation(), auxData, readMpStartGoalFromProperties(pdef.problemProperties))
      val g: Graph[List[Float], WUnDiEdge] = Graph.from(rmInput.centroids ++ vList, edgeSet ++ eList)
      g
    }

    val semanticType = rmc_startGoalFct_type :&: rmc_startGoal_nn_type :&: rmc_cg_centroidsOnly :&: dimensionality_three_d_t =>:
      cmp_sceneSegFct_type :&: cmp_cell_graph_fct_type :&: sd_seg_triangles_simple_type :&:
        sd_seg_triangles_para_type :&: dimensionality_three_d_t :&: rmc_cna_withoutCellNodes_type :&: rmc_startGoal_nn_type :&: rm_withCentroids_type :&:
        cFct_avg_type :&: sd_cell_triangle_type :&: rmc_cg_centroidsOnly :&: rmc_cn_withoutConnectorNodes :&: dimensionality_three_d_t
  }

  /*
    @combinator object TetMesh {
      def apply: MeshScene => PolySceneCellSegmentation = { polyScene: PolygonScene =>
        def pythonSceneString(scene: MeshScene): String = {
          val obstacleString = (for (i <- scene.obstacles.indices) yield s"b$i").reduce((a, b) => a + ", " + b)
          val verticesList: List[String] = scene.obstacles.map {
            i => i.vertices.map(listToPythonArray)
          }.map(listToPythonArray).map(listToNpArray())
          val facesList: List[String] = scene.obstacles.map { i => i.triangles }.map(listToNpArray)

          scene.obstacles.indices.foreach(i => println("i print: " + scene.obstacles(i).toString()))
          val object_instances = scene.obstacles.indices.map(i =>
            s"""    mesh$i = mesh = pymesh.form_mesh(obstacleVertices($i), obstacleFaces($i))""").reduce(_ + "\n" + _)

          s"$object_instances\n" +
            s"    scene_objects = [$obstacleString]\n" +
            s"    scene_size = [${scene.boundaries.mkString(", ")}]"
        }

        val fileMap = Map(cdTemplateLocationTet -> cdStartLocationTet)
        val substMap = Map("$substitute$" -> pythonSceneString(polyScene))
        val t = SubstitutionScheme(fileMap, substMap)

        val pWrapper = PythonWrapper.apply(t, cdStartLocationTet, decodeCellSegmentationFct)
        pWrapper.computeResultAndModifyInput(polyScene)
      }

      val semanticType = cmp_sceneSegFct_type :&: sd_seg_triangles_simple_type :&: sd_seg_triangles_para_type :&: dimensionality_three_d_t
    }
    */

  @combinator object GridCombinator {
    def apply: PolygonScene => PolySceneCellSegmentation = { polyScene: PolygonScene =>
      val (xResolution: Int, yResolution: Int) = (30, 30)
      val (xDim, yDim) = (polyScene.boundaries.head, polyScene.boundaries(1))
      val vertexLists = polyScene.obstacles.map(i => i.map(polyScene.vertices))
      val hulls: List[Geometry] = vertexLists.map { actualCoords =>
        // generate convex hull for current batch of points
        val coords = actualCoords.map(c => new Coordinate(c.head, c(1)))
        val convexHull = new ConvexHull(coords.toArray, new GeometryFactory)
        val hullGeometry = convexHull.getConvexHull
        hullGeometry
      }
      println("...")
      val (xStep, yStep): (Float, Float) = (xDim / xResolution, yDim / yResolution)
      val gf = new GeometryFactory()
      val coordMap = for {
        x <- 0 to xResolution
      } yield for {
        y <- 0 to yResolution
      } yield new Coordinate(-xDim / 2 + x * xStep, -yDim / 2 + y * yStep)

      println("...")
      val gridCells: IndexedSeq[(Geometry, List[Coordinate])] = for (x <- 0 until xResolution;
                                                                     y <- 0 until yResolution
                                                                     ) yield {
        val coords = List(coordMap(x)(y), coordMap(x + 1)(y), coordMap(x + 1)(y + 1), coordMap(x)(y + 1), coordMap(x)(y))
        val geo: Geometry = gf.createPolygon(coords.toArray)
        (geo, coords)
      }
      println("...")
      val freeGridCells = gridCells.filter(i => !hulls.exists(g => g.intersects(i._1)))
      val freeGridCellsFloatVals: IndexedSeq[List[List[Float]]] = freeGridCells.map(_._2.map(coord => List(coord.x.toFloat, coord.y.toFloat)).distinct)
      println("getting new Vertices")
      val newVertices = freeGridCellsFloatVals.flatten.distinct
      println("getting freeCells")
      val freeCells = freeGridCellsFloatVals.par.map(i => i.par.map(k => newVertices.indexOf(k) + polyScene.vertices.size).toList)
      println("...")
      polyScene.withVertices(polyScene.vertices ++ newVertices).withFreeCells(freeCells.toList)
    }

    val semanticType = cmp_sceneSegFct_type :&: sd_seg_grid_type :&: dimensionality_two_d_t
  }

  @combinator object VcdLineSweepJTS extends VcdUtils {
    def apply: PolygonScene => PolySceneCellSegmentation = { polyScene =>
      val topBoundary: Float = polyScene.boundaries(1) / 2
      val bottomBoundary: Float = -topBoundary

      val obstacleVertexLists: List[List[List[Float]]] = polyScene.obstacles.map(i => i.map(polyScene.vertices))
      val hulls: Seq[Geometry] = obstacleVertexLists.map { actualCoords =>
        // generate convex hull for current batch of points
        val coords = actualCoords.map(c => new Coordinate(c.head, c(1)))
        val convexHull = new ConvexHull(coords.toArray, new GeometryFactory)
        val hullGeometry = convexHull.getConvexHull
        hullGeometry
      }

      val (upExtVertices, downExtVertices): (Seq[List[Float]], Seq[List[Float]]) =
        getExtendableVertices(obstacleVertexLists)
      val vertexLinesUP: Seq[List[List[Float]]] =
        computeLines(upExtVertices, topBoundary, hulls, (d1, d2) => d1 <= d2)
      val vertexLinesDown: Seq[List[List[Float]]] =
        computeLines(downExtVertices, bottomBoundary, hulls, (d1, d2) => d1 >= d2)
      val vertexLineList = vertexLinesUP ++ vertexLinesDown

      val newVertices: Seq[List[Float]] = vertexLineList.flatMap(vertexLine =>
        vertexLine.filter(vertex => !polyScene.vertices.contains(vertex)))
      val polySceneSegVertices: Seq[List[Float]] = polyScene.vertices ++ newVertices

      val topVertices = newVertices.filter(vertex => vertex(1) == topBoundary)
        .map(vertex => polySceneSegVertices.indexOf(vertex)).toList ++
        polyScene.obstacles.flatMap(oVerts => oVerts.filter(v => polyScene.vertices(v)(1) == topBoundary))

      val bottomVertices = newVertices.filter(vertex => vertex(1) == bottomBoundary)
        .map(vertex => polySceneSegVertices.indexOf(vertex)).toList ++
        polyScene.obstacles.flatMap(oVerts => oVerts.filter(v => polyScene.vertices(v)(1) == bottomBoundary))

      val newObstacles = obstacleVertexLists.map { obstacle =>
        val coords = obstacle.map(vertex => new Coordinate(vertex.head, vertex(1)))
        val convexHull = new ConvexHull(coords.toArray, new GeometryFactory())
        val hullGeometry = convexHull.getConvexHull
        val newPoints = newVertices.zipWithIndex.map{
          case ((vertex, id)) => (new GeometryFactory().createPoint(new Coordinate(vertex.head, vertex(1))), id)
        }
        //changed, as Geometry.contains() yielded false negatives
        val newVerticesInHull:Seq[(Point, Int)] = newPoints.filter(p => hullGeometry.distance(p._1) < 0.0001)
        println(s"newVinObstacle $newVerticesInHull")
        obstacle ++ newVerticesInHull.map(_._2).map(newVertices)
      }

      val polySceneSegObstacles = newObstacles.map(vertexList =>
        vertexList.map(vertex => polySceneSegVertices.indexOf(vertex)))

      val lineList: Seq[List[Int]] = vertexLineList.map(vertexList =>
        vertexList.map(vertex => polySceneSegVertices.indexOf(vertex)))

      val lineSegmentation = addBoundaryLines(PolySceneLineSegmentation(polySceneSegVertices.toList,
        polySceneSegObstacles, polyScene.boundaries, topVertices, bottomVertices, lineList.toList))

      lineSegmentation.vertices.indices.foreach(id =>
        if (!(lineSegmentation.obstacles.exists(o => o.contains(id)) ||
          lineSegmentation.topVertices.contains(id) ||
          lineSegmentation.bottomVertices.contains(id)))
          println(s"v does not exist: $id , ${lineSegmentation.vertices(id)}"))

      println("""###""")
      println(lineSegmentation.asJson.toString())
      println("""###""")
      VcdLinesToCells.apply(lineSegmentation)
    }

    val semanticType = cmp_sceneSegFct_type :&: sd_vertical_cell_decomposition_type :&: dimensionality_two_d_t
  }

  def boundVerts(b: List[Float]): List[List[Float]] = {
    println(s"bounds: b")
    val (xBound, yBound) = (b.head / 2, b(1) / 2)
    List(
      List(-xBound, yBound),
      List(xBound, yBound),
      List(xBound, -yBound),
      List(-xBound, -yBound),
    )
  }
}