package org.combinators.ctp.repositories.scene


import io.circe.generic.JsonCodec
import org.apache.commons.geometry.euclidean.threed.rotation.QuaternionRotation
import org.apache.commons.geometry.euclidean.threed.Vector3D
import org.combinators.ctp.repositories.mptasks.MpTaskStartGoal
import org.combinators.ctp.repositories.python_interop.PythonTemplateUtils
import org.combinators.ctp.repositories.taxkinding.{CtpTaxonomy, SceneDescription}

trait SceneUtils extends SceneDescription with PythonTemplateUtils {
  val indentStr: String = """"""
  MqttObstacleSRT
  val getPrimitive: Map[Int, String] = Map(
    0 -> "Box",
    1 -> "Sphere")

  def getFclObstacleString(globalScale: List[Float], o: MqttObstacleSRT): String = {
    o.primitive match {
      case 0 => s"fcl.Box(${o.srt.localScale(0)*globalScale(0)},${o.srt.localScale(1)*globalScale(1)},${o.srt.localScale(2)*globalScale(2)})"
      case 1 => s"fcl.Sphere(${o.srt.localScale(0)})"
    }
  }

  val cubeVertices: List[List[Float]] = List(
    List(-0.5f, -0.5f, -0.5f),
    List(0.5f, -0.5f, -0.5f),
    List(0.5f, 0.5f, -0.5f),
    List(-0.5f, 0.5f, -0.5f),
    List(-0.5f, -0.5f, 0.5f),
    List(0.5f, -0.5f, 0.5f),
    List(0.5f, 0.5f, 0.5f),
    List(-0.5f, 0.5f, 0.5f))

  val cubeTriangles: List[List[Int]] = List(
    List(0, 2, 1),
    List(0, 3, 2),
    List(5, 0, 1),
    List(5, 4, 0),
    List(2, 6, 1),
    List(3, 6, 2),
    List(5, 1, 6),
    List(5, 6, 4),
    List(0, 7, 3),
    List(0, 4, 7),
    List(3, 7, 6),
    List(6, 7, 4))

  def cgalSceneStringPolyPath(scene: PolySceneSegmentationGraphPath): String = {
    val obstacleVertexStrings: List[String] = scene.obstacles.map { i =>
      listToPythonArray(i.map(scene.vertices).
        map(coords => f"""Point_2(${coords.mkString(",")})"""))
    }
    val obstacleString = obstacleVertexStrings.zip(obstacleVertexStrings.indices.map(index => f"obstacle$index")).map { case (i, oIdentifier) => f"$oIdentifier=i" }
    val polygonList: List[String] = obstacleVertexStrings.indices.map(i => f"Polygon_2(obstacle$i)").toList
    val obstaclesList = f"obstacles = ${listToPythonArray(polygonList)}"
    obstacleString.mkString("\r\n") + "\r\n" + obstaclesList
  }

  def vertexToCgalPoint(vl:List[Float]): String = {
    s"""Point_2(${vl.mkString(",")})"""
  }

  /*
  Order
  Local scale (localScale)
  Local rotation (localRotation)
  Local translation (localPosition)
  Parent's local transforms, in the same order as above
   */
  def getFclSRTString(id: Int, globalScale: List[Float], t: MqttObstacleSRT): String = {
    s"${indentStr}q$id = ${listToNpArray(t.srt.localRot)}\r\n${indentStr}translate$id =" +
      s" ${listToNpArray(t.srt.localTranslate.zip(globalScale).map { case (a, b) => a * b })}\r\n${indentStr}transform$id = fcl.Transform(q$id, translate$id)"
  }

  def TODOalti(id: Int, globalScale:List[Float], t: MqttObstacleSRT): String = {
    s"${indentStr}q$id = ${listToNpArray(t.srt.localRot)}\r\n${indentStr}translate$id = ${listToNpArray(t.srt.localTranslate.zip(globalScale).map { case (a, b) => a * b })}\r\n${indentStr}transform$id = fcl.Transform(q$id, translate$id)"
  }

//TODO apply srt method
  def getBvModelString(id:Int, globalScale:List[Float], t:MqttObstacleSRT):String ={
    t.primitive match {
      case 0 => //Cube
        val cubeV = cubeVertices.map(_ zip t.srt.localScale).map {
          _.map { case ((a, b)) => a * b }
        }
        val q = QuaternionRotation.of(t.srt.localRot(0), t.srt.localRot(1), t.srt.localRot(2), t.srt.localRot(3)) // w, x, y ,z
        val afterQ = cubeV.map(_.map(_.toDouble)).map(a => Vector3D.of(a(0), a(1), a(2))).map(v => q.apply(v))
        val translateVector: Vector3D = Vector3D.of(t.srt.localTranslate(0), t.srt.localTranslate(1), t.srt.localTranslate(2))
        val vertexVectors = afterQ.map { a =>
          Vector3D.of(a.getX() + translateVector.getX(),
            a.getY() + translateVector.getY(), a.getZ() + translateVector.getZ())
        }
        val vertexString = s"""np.array([${vertexVectors.map(a => s"[${a.getX()}, ${a.getY()}, ${a.getZ()}]").mkString(",\r\n")}])"""
        val idListStrings = cubeTriangles.map(idList => s"""[${idList.mkString(", ")}]""")
        val triString = listToNpArray(idListStrings)
        s"""($vertexString,
           |$triString)""".stripMargin
    }
  }

  def getCollisionObjString(id: Int) = {
    s"${indentStr}colObj$id = fcl.CollisionObject(obstacle$id, transform$id)"
  }

  def vertexTriCount(id:Int):(Int,Int) = id match {
    case 0 => (8,12)
  }

  def sceneSRTtoFclModelString(scene: SceneSRT): String = {
    val idList = scene.obstacles.indices zip scene.obstacles
    val instantiationStr = idList.map { case (a, b) => s"${indentStr}(verts$a, tris$a)=${getBvModelString(a, scene.boundaries, b)}" }.mkString("\r\n")
    val vLength = scene.obstacles.map(o => vertexTriCount(o.primitive)).map(_._1).sum
    val triLength = scene.obstacles.map(o => vertexTriCount(o.primitive)).map(_._2).sum

    val outStr =
      s"""mesh = fcl.BVHModel()
    mesh.beginModel($vLength, $triLength)
${scene.obstacles.indices.map(id => s"    mesh.addSubModel(verts$id, tris$id)").mkString("\r\n")}
    print(mesh.num_tries_())
    mesh.endModel()"""

    instantiationStr + "\r\n" + outStr
  }

  def sceneSRTtoFclPythonString(scene: SceneSRT): String = {
    val idList = scene.obstacles.indices zip scene.obstacles
    val instantiationStr = idList.map { case (a, b) => s"${indentStr}obstacle$a=${getFclObstacleString(scene.boundaries, b)}" }.mkString("\r\n")
    val transformStr = idList.map { case (a, b) => getFclSRTString(a, scene.boundaries, b) }.mkString("\r\n")
    val colObjStr = idList.map { case (a, b) => getCollisionObjString(a) }.mkString("\r\n")
    val coArray = s"${indentStr}objs = ${listToPythonArray(idList.map { case (a, b) => s"colObj$a" }.toList)}"
    List(instantiationStr, transformStr, colObjStr, coArray).mkString("\r\n")
  }

  def taskGoalStartToPythonOMPLString(tg: MpTaskStartGoal):String = {
    s"""        # create a start state
       |        start = ob.State(space)
       |        startRef = start()
       |        startRef.setX(${tg.startPosition.head})
       |        startRef.setY(${tg.startPosition(1)})
       |        startRef.setZ(${tg.startPosition(2)})
       |        startRef.rotation().setIdentity()  # start[0] = 0
       |        # start[1] = 0
       |        # start[2] = 0
       |
       |        # create a goal state
       |        goal = ob.State(space)
       |        goalRef = goal()
       |        goalRef.setX(${tg.endPosition.head})
       |        goalRef.setY(${tg.endPosition(1)})
       |        goalRef.setZ(${tg.endPosition(2)})
       |        goalRef.rotation().setIdentity()""".stripMargin

  }

/*
  @JsonCodec
  case class MqttObstacleSRT(primitive: Int, srt: MqttTransformSRT)

  @JsonCodec
  case class MqttTransformSRT(localScale: List[Float], localRot: List[Float], localTranslate: List[Float])
  */

//  MqttObstacleSrt => vertices, triangles

  def srtToFclString(s: SceneSRT) = "sceneStringFcl"
}


