package org.combinators.ctp.repositories.scene

import io.circe.generic.JsonCodec
import io.circe.parser.decode
import io.circe.generic.auto._
import io.circe.syntax._
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.geometry.PpPolyhedronMesh
import scalax.collection.Graph
import scalax.collection.edge.WUnDiEdge
import scala.language.implicitConversions

@JsonCodec
case class Scene(boundaries: List[Float], obstacles: List[MqttCubeData]) {
  def empty = Scene(List.empty, List.empty)
}

@JsonCodec
case class PolygonScene(vertices: List[List[Float]], obstacles: List[List[Int]], boundaries: List[Float]) {
  self =>
  def withVertices(v: List[List[Float]]): PolygonScene = {
    PolygonScene(v, self.obstacles, self.boundaries)
  }

  def withFreeCells(l: List[List[Int]]): PolySceneCellSegmentation = {
    PolySceneCellSegmentation(self.vertices, self.obstacles, self.boundaries, l)
  }
}

@JsonCodec
case class PolySceneLineSegmentation(vertices: List[List[Float]],
                                     obstacles: List[List[Int]],
                                     boundaries: List[Float],
                                     topVertices :List[Int],
                                     bottomVertices: List[Int],
                                     lines: List[List[Int]]) {}

@JsonCodec
case class PolySceneCellSegmentation(vertices: List[List[Float]],
                                     obstacles: List[List[Int]],
                                     boundaries: List[Float],
                                     freeCells: List[List[Int]]) {self=>

}

@JsonCodec
case class PolySceneCellSegmentationCentroids(vertices: List[List[Float]],
                                              obstacles: List[List[Int]],
                                              boundaries: List[Float],
                                              freeCells: List[List[Int]],
                                              centroids: List[List[Float]]) {}

case class PolySceneSegmentationGraph(vertices: List[List[Float]],
                                      obstacles: List[List[Int]],
                                      boundaries: List[Float],
                                      freeCells: List[List[Int]],
                                      centroids: List[List[Float]],
                                      graph: Graph[List[Float], WUnDiEdge]) {
  self =>
  def withGraph(g: Graph[List[Float], WUnDiEdge]): PolySceneSegmentationGraph =
    PolySceneSegmentationGraph(self.vertices, self.obstacles, self.boundaries, self.freeCells, self.centroids, g)

  def withPath(p:(Seq[List[Float]], Seq[WUnDiEdge[List[Float]]], Float)): PolySceneSegmentationGraphPath =
    PolySceneSegmentationGraphPath(self.vertices, self.obstacles, self.boundaries, self.freeCells,self.graph, p)
}

case class PolySceneSegmentationGraphPath(
                                           vertices: List[List[Float]],
                                           obstacles: List[List[Int]],
                                           boundaries: List[Float],
                                           freeCells: List[List[Int]],
                                           graph: Graph[List[Float], WUnDiEdge],
                                           gpath: (Seq[List[Float]], Seq[WUnDiEdge[List[Float]]], Float)) {
  self =>

  def withPath(p: (Seq[List[Float]], Seq[WUnDiEdge[List[Float]]], Float)): PolySceneSegmentationGraphPath =
    PolySceneSegmentationGraphPath(self.vertices, self.obstacles, self.boundaries, self.freeCells, self.graph, p)

  def withGraph(g: Graph[List[Float], WUnDiEdge]): PolySceneSegmentationGraphPath =
    PolySceneSegmentationGraphPath(self.vertices, self.obstacles, self.boundaries, self.freeCells, g, self.gpath)

  def empty:PolySceneSegmentationGraphPath = PolySceneSegmentationGraphPath(List.empty, List.empty, List.empty,
    List.empty, Graph.empty, (Seq.empty, Seq.empty, 0.0f))
}

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
case class TriangleSeg(vertices: List[List[Float]], triangles: List[List[Int]]) {
  self =>
  def withCentroids(c: List[List[Float]]): TriangleSegCentroids =
    TriangleSegCentroids(self.vertices, self.triangles, c)
}

case class TriangleSegCentroids(vertices: List[List[Float]],
                                triangles: List[List[Int]],
                                centroids: List[List[Float]])

case class TriangleSegPath(vertices: List[List[Float]],
                           freeCells: List[List[Int]],
                           graph: Graph[List[Float], WUnDiEdge],
                           gpath: (Seq[List[Float]], Seq[WUnDiEdge[List[Float]]], Float))


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
