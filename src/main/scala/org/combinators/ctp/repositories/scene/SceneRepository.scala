package org.combinators.ctp.repositories.scene

import cats.syntax.NestedIdOps
import com.typesafe.scalalogging.LazyLogging
import io.circe.generic.JsonCodec
import org.apache.commons.math3.linear.{MatrixUtils, RealMatrix}
import org.combinators.cls.interpreter._
import org.combinators.cls.types.{Constructor, Type}
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories.geometry._
import org.combinators.ctp.repositories.scene._
import org.combinators.ctp.repositories.{cmp_sceneSegFct_type, _}
import org.combinators.ctp._
import scalaz.Tree
import org.apache.commons.math3.linear.MatrixUtils
import org.apache.commons.math3.linear.RealMatrix
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.parser.decode
import org.combinators.ctp.repositories.toplevel.{MqttCubeData, PolySceneLineSegmentation, PolygonScene, Scene, SegmentationLines2d}
import org.locationtech.jts.algorithm.ConvexHull
import org.locationtech.jts.geom.impl.CoordinateArraySequenceFactory
import org.locationtech.jts.geom.{Coordinate, CoordinateSequence, GeometryFactory, LineString, Triangle}

//@JsonCodec
//case class Scene(boundaries: List[Float], obstacles: List[MqttCubeData]) {}
//
//@JsonCodec
//case class PolygonScene(vertices: List[List[Float]], obstacles: List[List[Int]], boundaries: List[Float]) {}

trait SceneRepository extends GeometricRepository with LazyLogging {

  trait SceneToScenePolyTrait {
    def apply(cubeToVList: List[MqttCubeData] => List[PpVertexList],
              cut: (List[PpVertexList], Scene) => List[PpVertexList]): Scene => PolygonScene = { s: Scene =>
      logger.debug("scene to poly before: " + s.obstacles.foreach(print))
//      val obstacleVertexTuple = cubeToVList(s.obstacles )
      val obstacleVertexTuple = cut(cubeToVList(s.obstacles), s)
      logger.debug("scene to poly after: " + obstacleVertexTuple)

      val (_, objList, globalVertices) = obstacleVertexTuple.
        foldLeft(0: Int, List.empty[Range], List.empty[List[Float]]) {
          case ((id, obsVertices, globalVertices), obstacleVertices) =>
            (id + obstacleVertices.vertices.size,
              Range(id, id + obstacleVertices.vertices.size) +: obsVertices,
              globalVertices ++ obstacleVertices.vertices)
        }
      objList.foreach(i => logger.debug("objList: " + i))
      val objects = objList.map {
        _.toList
      }
      PolygonScene(globalVertices, objects, s.boundaries)
    }
    val semanticType :Type
  }

  @combinator object SceneToScenePoly extends SceneToScenePolyTrait {
     val semanticType = gm_CubeToPoly :&: dimensionality_var =>:
      dimensionality_var :&: cmd_obstacleSceneBoundingCutFct_type =>:
      (sd_unity_scene_type =>: sd_polygon_scene_type) :&: dimensionality_var
  }

  @combinator object SceneToScenePolyTax extends SceneToScenePolyTrait {
    val semanticType = gm_CubeToPoly =>:
      cmd_obstacleSceneBoundingCutFct_type =>:
      (sd_unity_scene_type =>: sd_polygon_scene_type)
  }

  /*
  * Cuts obstacles in case they exceed scene bounds
   */
  @combinator object ObstacleSceneBoundingCut2D {
    def apply(): (List[PpVertexList], Scene) => List[PpVertexList] = {
      (ppVertexLists, scene) =>
        val gf = new GeometryFactory()
        val pointList = List(
          new Coordinate(-scene.boundaries.head / 2, -scene.boundaries(1) / 2),
          new Coordinate(-scene.boundaries.head / 2, scene.boundaries(1) / 2),
          new Coordinate(scene.boundaries.head / 2, scene.boundaries(1) / 2),
          new Coordinate(scene.boundaries.head / 2, -scene.boundaries(1) / 2),
          new Coordinate(-scene.boundaries.head / 2, -scene.boundaries(1) / 2))

        val lString = new LineString(CoordinateArraySequenceFactory.instance.create(pointList.toArray), gf)
        val box = new ConvexHull(pointList.toArray, gf)
        ppVertexLists.map { i =>
          val coords = i.vertices.map(c => new Coordinate(c.head, c(1)))
          val currentObstacle = new ConvexHull(coords.toArray, gf)
          if (lString.intersects(currentObstacle.getConvexHull)) { //check to avoid unnecessary precision loss
            PpVertexList(
              box.getConvexHull.intersection(currentObstacle.getConvexHull).
                getCoordinates.map(i => List(i.x.toFloat, i.y.toFloat)).toList)
          }
          else {
            i
          }
        }
    }

    val semanticType = dimensionality_two_d_t :&: cmd_obstacleSceneBoundingCutFct_type
  }

  /*
  * Not cut for obstacles
   */
  @combinator object ObstacleSceneNoBoundingCut2D {
    def apply(): (List[PpVertexList], Scene) => List[PpVertexList] = {
      (ppVertexLists, _) => ppVertexLists
    }

    val semanticType = dimensionality_two_d_t :&: cmd_obstacleSceneBoundingCutFct_type
  }

  //Placeholder, not yet implemented
  @combinator object ObstacleSceneBoundingCut3D {
    def apply(): (List[PpVertexList], Scene) => List[PpVertexList] = {
      (list, _) => list
    }

    val semanticType = dimensionality_three_d_t :&: cmd_obstacleSceneBoundingCutFct_type
  }

  /*
Combinator to apply affine 2d transformation to 2d structure for a single vertex.
*/
  /*
    @combinator object ApplyAffineTransform2D {
      def apply(): (List[Float], MqttTransform) => List[Float] = {
        (p: List[Float], tMatrix: MqttTransform) => {
          val f = p :+ 1.0f
          val tMatrixList = tMatrix.transformMatrixList
          mult(tMatrixList, f).take(2)
        }
      }

      val semanticType = scene segmanetation => line
    }
  */

//  @combinator object SceneBoundariesTwoDim {
//    def apply: ((Int, Int), (Int, Int)) = ((-5, 5), (-3, 3))
//
//    val semanticType = sd_scene_boundaries_type :&: dimensionality_two_d_t
//  }
//
//  @combinator object SceneBoundariesThreeDim {
//    def apply: ((Int, Int), (Int, Int), (Int, Int)) = ((-5, 5), (-3, 3), (4, 4))
//
//    val semanticType = sd_scene_boundaries_type :&: dimensionality_three_d_t
//  }

  /*@combinator object AddSceneObstacleMeshesTwoD {
    def apply(s: Scene, o: List[scene_cube_2d_n]): Scene = ??? //s.addSceneObject(o)

    val semanticType = sd_scene_boundaries_type :&: dimensionality_two_d_t =>:
      sd_scene_descripton_obstacles :&: dimensionality_two_d_t =>:
      sd_source_native_scala_type :&: dimensionality_two_d_t
  }*/
}
