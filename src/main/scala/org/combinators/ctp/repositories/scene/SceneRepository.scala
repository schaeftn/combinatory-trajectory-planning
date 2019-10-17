package org.combinators.ctp.repositories.scene

import io.circe.generic.JsonCodec
import org.apache.commons.math3.linear.{MatrixUtils, RealMatrix}
import org.combinators.cls.interpreter._
import org.combinators.cls.types.{Constructor, Type}
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories.geometry._
import org.combinators.ctp.repositories.scene._
import org.combinators.ctp.repositories._
import org.combinators.ctp._
import org.combinators.ctp.repositories.taxkinding.{CtpTaxonomy, SceneDescription}
import scalaz.Tree
import org.apache.commons.math3.linear.MatrixUtils
import org.apache.commons.math3.linear.RealMatrix

//@JsonCodec
//case class Scene(boundaries: List[Float], obstacles: List[MqttCubeData]) {}
//
//@JsonCodec
//case class PolygonScene(vertices: List[List[Float]], obstacles: List[List[Int]], boundaries: List[Float]) {}

trait SceneRepository extends SceneDescription with CtpTaxonomy {

  @combinator object SceneToScenePoly {
    def apply(cubeToVList: List[MqttCubeData] => List[PpVertexList]): Scene => PolygonScene = { s: Scene =>
//      val asd = s.obstacles.map(_.tMatrix.map(_.map(_.toDouble).toArray).toArray)
      //TODO check with Commons
//      val realMatrix = asd.map { tMat => MatrixUtils.createRealMatrix(tMat) }
      //val obstacleVertexTuple = (for {i <- s.obstacles} yield (i, affTransform(getVertices(i.cubeSize), i.tMatrix)))

      print("scene to poly before: " + s.obstacles.foreach(print))
      val obstacleVertexTuple = cubeToVList(s.obstacles)

      print("scene to poly after: " + obstacleVertexTuple)

      val (_, objList, globalVertices) = obstacleVertexTuple.
        foldLeft(0, List.empty[Range], List.empty[List[Float]]) {
          case ((id, obsVertices, globalVertices), obstacleVertices) =>
            (id + obstacleVertices.vertices.size,  Range(id, id + obstacleVertices.vertices.size) +: obsVertices , globalVertices ++ obstacleVertices.vertices)
        }
      objList.foreach(i =>println("objList: " + i))
      val objects = objList.map{_.toList}
      PolygonScene(globalVertices, objects, s.boundaries)
    }

    val semanticType = gm_CubeToPoly :&: dimensionality_two_d_t =>:
      (sd_unity_scene_type =>: sd_polygon_scene_type)
  }


  @combinator object TranslResult {
    def apply(): PolySceneLineSegmentation => SegmentationLines2d = { a =>
      SegmentationLines2d(a.lines.map { b => b.map(c => a.vertices(c)) })
    }

    val semanticType = Constructor("foo")
  }

/*
  @combinator object SegmentationLinesToCells{
*/

/*def apply(ls: PolySceneLineSegmentation): PolySceneCellSegmentation = {
  ls.lines.
  ls.lines.map(i => i).contains(1)
  for (l <- ls.lines) yield l
}
    val semanticType = sd_polygon_scene_type :&: sd_seg_lines :&: sd_scene_segmentation =>: sd_polygon_scene_type :&: sd_seg_cells :&: sd_scene_segmentation
  }*/

  /*
Function to apply affine 2d transformation to 2d structure for a single vertex.
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



  def test() = {
    val test: List[List[Float]] = List(List(0.1f, 0.2f, 0.3f), List(0.7f, 0.4f, 0.5f))
    val matrixData = Array(Array(1d, 2d, 3d), Array(2d, 5d, 3d))
    val m: RealMatrix = MatrixUtils.createRealMatrix(matrixData)
    MatrixUtils.createRealIdentityMatrix(4)
    //  self.vertices = np.hsplit(np.array([np.matmul(t, x) for x in np.hstack((self.vertices, a))]), [2, 2])[0]
  }



  @combinator object SceneBoundariesTwoDim {
    def apply: ((Int, Int), (Int, Int)) = ((-5, 5), (-3, 3))

    val semanticType = sd_scene_boundaries_type :&: dimensionality_two_d_t
  }

  @combinator object SceneBoundariesThreeDim {
    def apply: ((Int, Int), (Int, Int), (Int, Int)) = ((-5, 5), (-3, 3), (4, 4))

    val semanticType = sd_scene_boundaries_type :&: dimensionality_three_d_t
  }

  @combinator object AddSceneObstacleMeshesTwoD {
    def apply(s: Scene, o: List[scene_cube_2d_n]): Scene = ??? //s.addSceneObject(o)

    val semanticType = sd_scene_boundaries_type :&: dimensionality_two_d_t =>:
      sd_scene_descripton_obstacles :&: dimensionality_two_d_t =>:
      sd_source_native_scala_type :&: dimensionality_two_d_t
  }

//  @combinator object AddSceneObstaclesProcedural {}

}
