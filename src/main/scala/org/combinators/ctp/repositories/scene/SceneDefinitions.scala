package org.combinators.ctp.repositories.scene

import java.util.Properties

import com.typesafe.scalalogging.LazyLogging
import io.circe.generic.JsonCodec
import io.circe.parser.decode
import io.circe.generic.auto._
import io.circe.syntax._
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.geometry.PpPolyhedronMesh
import scalax.collection.Graph
import scalax.collection.edge.WUnDiEdge

import scala.language.implicitConversions

case class Scene(boundaries: List[Float], obstacles: List[MqttCubeData]) {
  def empty = Scene(List.empty, List.empty)
}

case class SceneSRT(boundaries: List[Float], obstacles: List[MqttObstacleSRT]) {
  def empty = Scene(List.empty, List.empty)
}

case class PolygonScene(vertices: List[List[Float]], obstacles: List[List[Int]], boundaries: List[Float]) {
  self =>
  def withVertices(v: List[List[Float]]): PolygonScene = {
    PolygonScene(v, self.obstacles, self.boundaries)
  }

  def withFreeCells(l: List[List[Int]]): PolySceneCellSegmentation = {
    PolySceneCellSegmentation(self.vertices, self.obstacles, self.boundaries, l)
  }
}

case class PolySceneLineSegmentation(vertices: List[List[Float]],
                                     obstacles: List[List[Int]],
                                     boundaries: List[Float],
                                     topVertices :List[Int],
                                     bottomVertices: List[Int],
                                     lines: List[List[Int]]) {}

case class PolySceneCellSegmentation(vertices: List[List[Float]],
                                     obstacles: List[List[Int]],
                                     boundaries: List[Float],
                                     freeCells: List[List[Int]]) {
  self =>
  def withCentroids(c: List[List[Float]]): PolySceneCellSegmentationCentroids =
    PolySceneCellSegmentationCentroids(self.vertices, self.obstacles, self.boundaries, self.freeCells, c)
  def withRoadmap(g: Graph[List[Float], WUnDiEdge]): PolySceneSegmentationRoadmap =
    PolySceneSegmentationRoadmap(self.vertices, self.obstacles, self.boundaries, self.freeCells, List.empty, g)
}

case class PolySceneCellSegmentationCentroids(vertices: List[List[Float]],
                                              obstacles: List[List[Int]],
                                              boundaries: List[Float],
                                              freeCells: List[List[Int]],
                                              centroids: List[List[Float]]) {
  self =>
  def withRoadmap(g: Graph[List[Float], WUnDiEdge]): PolySceneSegmentationRoadmap =
    PolySceneSegmentationRoadmap(self.vertices, self.obstacles, self.boundaries, self.freeCells, self.centroids, g)
}

case class PolySceneSegmentationRoadmap(vertices: List[List[Float]],
                                        obstacles: List[List[Int]],
                                        boundaries: List[Float],
                                        freeCells: List[List[Int]],
                                        centroids: List[List[Float]],
                                        roadmap: Graph[List[Float], WUnDiEdge]) {
  self =>
  def withRoadmap(g: Graph[List[Float], WUnDiEdge]): PolySceneSegmentationRoadmap =
    PolySceneSegmentationRoadmap(self.vertices, self.obstacles, self.boundaries, self.freeCells, self.centroids, g)

  def withPath(p:Seq[List[Float]]): PolySceneSegmentationRoadmapPath =
    PolySceneSegmentationRoadmapPath(self.vertices, self.obstacles, self.boundaries, self.freeCells,self.roadmap, p)
}

case class PolySceneSegmentationRoadmapPath(
                                             vertices: List[List[Float]],
                                             obstacles: List[List[Int]],
                                             boundaries: List[Float],
                                             freeCells: List[List[Int]],
                                             roadmap: Graph[List[Float], WUnDiEdge],
                                             gpath: Seq[List[Float]]) {
  self =>

  def withPath(p: Seq[List[Float]]): PolySceneSegmentationRoadmapPath =
    PolySceneSegmentationRoadmapPath(self.vertices, self.obstacles, self.boundaries, self.freeCells, self.roadmap, p)

  def withRoadmap(g: Graph[List[Float], WUnDiEdge]): PolySceneSegmentationRoadmapPath =
    PolySceneSegmentationRoadmapPath(self.vertices, self.obstacles, self.boundaries, self.freeCells, g, self.gpath)

  def empty:PolySceneSegmentationRoadmapPath = PolySceneSegmentationRoadmapPath(List.empty, List.empty, List.empty,
    List.empty, Graph.empty, Seq.empty[List[Float]])
}

case class PolyLineSegmentation(vertices: List[List[Float]], lines: List[List[Int]]) {}

case class CellSegmentation(vertices: List[List[Float]], cells: List[List[Int]])

case class SegmentationLines2d(lines: List[List[List[Float]]])

case class SceneCfreePolygons2d(vertices: List[List[Float]], cfreePolygons: List[PpPolyhedronMesh])


case class SceneMesh(vertices: vertexArrayType2, faces: facesArrayType, voxels: voxelArrayType) {}

case class MqttCubeData(tMatrix: List[List[Float]], cubeSize: List[Float])

case class MqttTransform(transformMatrixList: List[List[Float]])

case class MqttObstacleSRT(primitive: Int, srt: MqttTransformSRT)

case class MqttTransformSRT(localScale: List[Float], localRot: List[Float], localTranslate: List[Float])

case class PathPreds(nodes: List[Int], preds: List[Int])

/**
 * TODO Applied Transform Matrix will expressed by semantic Types
 * Not used yet
 */
case class VolumeMesh(vertices: vertexArrayType2, faces: facesArrayType,
                      voxels: voxelArrayType, override val tMatrix: tMatrixType) extends Transformable {}

// * Not used yet
case class SurfaceMesh(vertices: vertexArrayType2, faces: facesArrayType,
                       override val tMatrix: tMatrixType) extends Transformable{}

trait Transformable {
  val tMatrix: List[List[Float]]
}

