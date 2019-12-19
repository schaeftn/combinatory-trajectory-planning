package org.combinators.ctp.repositories.temp

import org.combinators.ctp.repositories.scene.{MqttCubeData, MqttTransform, PolygonScene, Scene}
import io.circe.parser.decode
import io.circe.generic.auto._
import io.circe.syntax._
import org.combinators.ctp.repositories.geometry.{GeometricRepository, PpVertexList}
import org.locationtech.jts.algorithm.ConvexHull
import org.locationtech.jts.geom.{Coordinate, GeometryFactory}


object GridCMP extends App with GeometricRepository{

  // Felix
  // erster Schritt: für Polyscene Bitmap berechnen.
  // GGf JTS verwenden oder was eigenes überlegen.
  // Axis aligned bounding boxes (AABBs) evtl für grobe Einteilung


  def aTransform: (List[List[Float]], MqttTransform) => List[List[Float]] = {
    (p: List[List[Float]], tMatrix: MqttTransform) => {
      println("Before")
      println(p)
      val f = p.map(i => i  :+ 1.0f :+ 1.0f)
      val tMatrixList = tMatrix.transformMatrixList
      println("result ApplyAffineTransform2DVertexList")
      println(multList(tMatrixList, f))
      multList(tMatrixList, f).map(i => i.take(2))
    }
  }

  val vertices = List(
    List(-0.5f, -0.5f),
    List(0.5f, -0.5f),
    List(0.5f, 0.5f),
    List(-0.5f, 0.5f))

  def cubeTransform:
  List[MqttCubeData] => List[PpVertexList] = { a =>
    (for {i <- a}
      yield aTransform(vertices, MqttTransform(i.tMatrix))).map(a => PpVertexList(a))
  }

  def sceneTransform: Scene => PolygonScene = { s: Scene =>

    print("scene to poly before: " + s.obstacles.foreach(print))
    val obstacleVertexTuple = cubeTransform(s.obstacles)

    print("scene to poly after: " + obstacleVertexTuple)

    val (_, objList, globalVertices) = obstacleVertexTuple.
      foldLeft(0, List.empty[Range], List.empty[List[Float]]) {
        case ((id, obsVertices, globalVertices), obstacleVertices) =>
          (id + obstacleVertices.vertices.size,
            Range(id, id + obstacleVertices.vertices.size) +: obsVertices,
            globalVertices ++ obstacleVertices.vertices)
      }
    objList.foreach(i => println("objList: " + i))
    val objects = objList.map {
      _.toList
    }
    PolygonScene(globalVertices, objects, s.boundaries)
  }

  val sceneString: String = """{"boundaries":[100,100,0],"obstacles":[{"tMatrix":[[53.85289,4.82381058,0.0,-49.88],[-12.1389685,4.810618,0.0,-8.34],[0.0,0.0,0.0,0.0],[0.0,0.0,0.0,1.0]],"cubeSize":[64.34709,9.13852,0.0]},{"tMatrix":[[10.2148867,0.0,0.0,26.8900013],[0.0,48.25598,0.0,-10.5599995],[0.0,0.0,0.0,0.0],[0.0,0.0,0.0,1.0]],"cubeSize":[10.2148867,48.25598,0.0]},{"tMatrix":[[10.4838343,3.62617254,0.0,-0.38],[-1.06201494,35.7962837,0.0,32.94],[0.0,0.0,0.0,0.0],[0.0,0.0,0.0,1.0]],"cubeSize":[10.537488,35.97948,0.0]},{"tMatrix":[[42.2332878,-0.3470646,0.0,-35.4600029],[-4.27824259,-3.426098,0.0,8.7300005],[0.0,0.0,0.0,0.0],[0.0,0.0,0.0,1.0]],"cubeSize":[42.44943,-3.44363236,0.0]},{"tMatrix":[[16.3090668,0.0,0.0,-36.3],[0.0,-10.5733194,0.0,39.6],[0.0,0.0,0.0,0.0],[0.0,0.0,0.0,1.0]],"cubeSize":[16.3090668,-10.5733194,0.0]},{"tMatrix":[[16.3090668,0.0,0.0,-25.2],[0.0,-10.5733194,0.0,20.6],[0.0,0.0,0.0,0.0],[0.0,0.0,0.0,1.0]],"cubeSize":[16.3090668,-10.5733194,0.0]},{"tMatrix":[[16.3090668,0.0,0.0,8.5],[0.0,-10.5733194,0.0,3.6],[0.0,0.0,0.0,0.0],[0.0,0.0,0.0,1.0]],"cubeSize":[16.3090668,-10.5733194,0.0]},{"tMatrix":[[6.57370663,0.0,0.0,2.80000019],[0.0,46.4312019,0.0,-29.1999989],[0.0,0.0,0.0,0.0],[0.0,0.0,0.0,1.0]],"cubeSize":[6.57370663,46.4312019,0.0]}]}"""
  val decScene: Scene = decode[Scene](sceneString).right.get
  val polyScene: PolygonScene = sceneTransform(decScene)

  val vertexLists = polyScene.obstacles.map(i => i.map(polyScene.vertices))
  val hulls = vertexLists.map { actualCoords =>
    // generate convex hull for current batch of points
    val coords = actualCoords.map(c => new Coordinate(c.head, c(1)))
    val convexHull = new ConvexHull(coords.toArray, new GeometryFactory)
    val hullGeometry = convexHull.getConvexHull
    hullGeometry
  }
  hulls

}
