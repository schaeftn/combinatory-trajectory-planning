package org.combinators.ctp.repositories.scene

import io.circe.generic.JsonCodec
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.geometry.PpPolyhedronMesh

@JsonCodec
case class Scene(boundaries: List[Float], obstacles: List[MqttCubeData]) {}

@JsonCodec
case class PolygonScene(vertices: List[List[Float]], obstacles: List[List[Int]], boundaries: List[Float]) {}

@JsonCodec
case class PolySceneLineSegmentation(vertices: List[List[Float]], obstacles: List[List[Int]],
                                     boundaries: List[Float], topVertices :List[Int],
                                     bottomVertices: List[Int], lines: List[List[Int]]) {}

@JsonCodec
case class PolySceneCellSegmentation(vertices: List[List[Float]], obstacles: List[List[Int]], boundaries: List[Float], freeCells: List[List[Int]]) {}

@JsonCodec
case class PolyLineSegmentation(vertices: List[List[Float]], lines: List[List[Int]]) {}

@JsonCodec
case class SceneSegmentation3d(vertices: List[List[Float]], faces: List[List[Int]], voxels: List[List[Int]])

@JsonCodec
case class SceneSegmentation2d(obstacles: List[MqttCubeData], lineVertices: List[List[Float]], lines: List[List[Int]])

@JsonCodec
case class SceneSegmentation2dFree(obstacles: List[MqttCubeData], cFree: List[PpPolyhedronMesh])

@JsonCodec
case class SegmentationLines2d(lines: List[List[List[Float]]])

@JsonCodec
case class SceneCfreePolygons2d(vertices: List[List[Float]], cfreePolygons: List[PpPolyhedronMesh])

@JsonCodec
case class SceneMesh(vertices: vertexArrayType2, faces: facesArrayType, voxels: voxelArrayType) {}

@JsonCodec
case class MqttCubeData(tMatrix: List[List[Float]], cubeSize: List[Float])

@JsonCodec
case class MqttTransform(transformMatrixList: List[List[Float]])

/**
 * TODO Applied Transform Matrix will expressed by semantic Types
 */
@JsonCodec
case class VolumeMesh(vertices: vertexArrayType2, faces: facesArrayType,
                      voxels: voxelArrayType, override val tMatrix: tMatrixType) extends Transformable {}

@JsonCodec
case class SurfaceMesh(vertices: vertexArrayType2, faces: facesArrayType,
                       override val tMatrix: tMatrixType) extends Transformable{}

trait Transformable {
  val tMatrix: List[Float]
}
