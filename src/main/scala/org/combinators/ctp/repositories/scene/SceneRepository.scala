package org.combinators.ctp.repositories.scene

import cats.syntax.NestedIdOps
import io.circe.generic.JsonCodec
import org.apache.commons.math3.linear.{MatrixUtils, RealMatrix}
import org.combinators.cls.interpreter._
import org.combinators.cls.types.{Constructor, Type}
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories.geometry._
import org.combinators.ctp.repositories.scene._
import org.combinators.ctp.repositories.{cmp_sceneSegFct_type, _}
import org.combinators.ctp._
import org.combinators.ctp.repositories.taxkinding.{CtpTaxonomy, SceneDescription}
import scalaz.Tree
import org.apache.commons.math3.linear.MatrixUtils
import org.apache.commons.math3.linear.RealMatrix
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.parser.decode
import org.locationtech.jts.geom.{Coordinate, Triangle}

//@JsonCodec
//case class Scene(boundaries: List[Float], obstacles: List[MqttCubeData]) {}
//
//@JsonCodec
//case class PolygonScene(vertices: List[List[Float]], obstacles: List[List[Int]], boundaries: List[Float]) {}

trait SceneRepository extends SceneDescription with CtpTaxonomy with GeometricRepository{
  @combinator object SceneToScenePoly {
    def apply(cubeToVList: List[MqttCubeData] => List[PpVertexList]): Scene => PolygonScene = { s: Scene =>

      print("scene to poly before: " + s.obstacles.foreach(print))
      val obstacleVertexTuple = cubeToVList(s.obstacles)

      print("scene to poly after: " + obstacleVertexTuple)

      val (_, objList, globalVertices) = obstacleVertexTuple.
        foldLeft(0, List.empty[Range], List.empty[List[Float]]) {
          case ((id, obsVertices, globalVertices), obstacleVertices) =>
            (id + obstacleVertices.vertices.size,
              Range(id, id + obstacleVertices.vertices.size) +: obsVertices ,
              globalVertices ++ obstacleVertices.vertices)
        }
      objList.foreach(i =>println("objList: " + i))
      val objects = objList.map{_.toList}
      PolygonScene(globalVertices, objects, s.boundaries)
    }

    val semanticType = gm_CubeToPoly :&: dimensionality_var =>:
      (sd_unity_scene_type =>: sd_polygon_scene_type) :&: dimensionality_var
  }

  //TODO Check?
  @combinator object TranslResult {
    def apply(): PolySceneLineSegmentation => SegmentationLines2d = { a =>
      println(a.asJson)
      SegmentationLines2d(a.lines.map { b => b.map(c => a.vertices(c)) })
    }

    val semanticType = Constructor("foo")
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

  @combinator object SceneBoundariesTwoDim {
    def apply: ((Int, Int), (Int, Int)) = ((-5, 5), (-3, 3))

    val semanticType = sd_scene_boundaries_type :&: dimensionality_two_d_t
  }

  @combinator object SceneBoundariesThreeDim {
    def apply: ((Int, Int), (Int, Int), (Int, Int)) = ((-5, 5), (-3, 3), (4, 4))

    val semanticType = sd_scene_boundaries_type :&: dimensionality_three_d_t
  }

  /*@combinator object AddSceneObstacleMeshesTwoD {
    def apply(s: Scene, o: List[scene_cube_2d_n]): Scene = ??? //s.addSceneObject(o)

    val semanticType = sd_scene_boundaries_type :&: dimensionality_two_d_t =>:
      sd_scene_descripton_obstacles :&: dimensionality_two_d_t =>:
      sd_source_native_scala_type :&: dimensionality_two_d_t
  }*/
}
