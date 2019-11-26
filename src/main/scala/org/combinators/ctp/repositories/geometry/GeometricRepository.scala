package org.combinators.ctp.repositories.geometry

import org.combinators.cls.interpreter._
import org.combinators.cls.types.{Kinding, Type}
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories.toplevel.UnityMeshData
import org.combinators.ctp.repositories._
import org.eclipse.paho.client.mqttv3.MqttClient
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.scene.{MqttCubeData, MqttTransform}

import math._

trait GeometricRepository extends GeometryUtils {
  @combinator object AabbGen2D {
    def apply(): PpVertexList => PpAaBb2D = { a =>
      val xCoords = a.vertices map (v => v.head)
      val yCoords = a.vertices map (v => v(1))
      val (xMin, xMax) = (xCoords.min, xCoords.max)
      val (yMin, yMax) = (yCoords.min, yCoords.max)
      PpAaBb2D((xMin, xMax), (yMin, yMax))
    }

    val semanticType = gm_aaBbGenFct :&: dimensionality_two_d_t
  }

  /*
  Transforms an mqtt cube (3D) to a polygon.
  Transforms cube to polygon, applies the affine 3D transformation matrix to the polygons vertices
  */
  @combinator object CubeToPolygon {
    def apply(): PpPolyhedronMesh => PpAaBb3D = { a =>
      val xCoords = a.vertices map (v => v.head)
      val yCoords = a.vertices map (v => v(1))
      val zCoords = a.vertices map (v => v(2))
      val (xMin, xMax) = (xCoords.min, xCoords.max)
      val (yMin, yMax) = (yCoords.min, yCoords.max)
      val (zMin, zMax) = (zCoords.min, zCoords.max)
      PpAaBb3D((xMin, xMax), (yMin, yMax), (zMin, zMax))
    }

    val semanticType = gm_aaBbGenFct :&: dimensionality_three_d_t
  }

  /*
  Transforms an mqtt cube (3D) to a polygon.
  Applies the affine 3D transformation matrix to the cube, Transforms resulting cube to polygon by stripping its z coords and
  */


  @combinator object AabbGen3D {
    def apply(): PpPolyhedronMesh => PpAaBb3D = { a =>
      val xCoords = a.vertices map (v => v.head)
      val yCoords = a.vertices map (v => v(1))
      val zCoords = a.vertices map (v => v(2))
      val (xMin, xMax) = (xCoords.min, xCoords.max)
      val (yMin, yMax) = (yCoords.min, yCoords.max)
      val (zMin, zMax) = (zCoords.min, zCoords.max)
      PpAaBb3D((xMin, xMax), (yMin, yMax), (zMin, zMax))
    }

    val semanticType = gm_aaBbGenFct :&: dimensionality_three_d_t
  }

  /*  Turns Cubes to Polygons,
   works for 2d, 3d */
  @combinator object CubeToPoly {
    def apply(vertices: List[List[Float]], transform: (List[List[Float]], MqttTransform) => List[List[Float]]):
    List[MqttCubeData] => List[PpVertexList] = { a =>
      (for {i <- a}
        yield transform(vertices, MqttTransform(i.tMatrix))).map(a => PpVertexList(a))
    }

    val semanticType = gm_rectangular :&: dimensionality_var =>:
      gm_AffTransformVertexListFunction :&: dimensionality_var =>:
      gm_CubeToPoly :&: dimensionality_var
  }


  /*
  Function to apply affine 3D transformation to 2d structure
  */
  @combinator object ApplyAffineTransform2DVertexList {
    def apply(): (List[List[Float]], MqttTransform) => List[List[Float]] = {
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

    val semanticType = gm_AffTransformVertexListFunction :&: dimensionality_two_d_t
  }

  /*
  Function to apply affine 3D transformation to 3d vertex
  */
  @combinator object ApplyAffineTransform3DVertexList {
    def apply(): (List[List[Float]], MqttTransform) => List[List[Float]] = {
      (p: List[List[Float]], tMatrix: MqttTransform) => {
        println("Before")
        println(p)
        val f = p.map(i => i :+ 1.0f)
        val tMatrixList = tMatrix.transformMatrixList
        println("result")
        println(multList(tMatrixList, f).map(i => i.take(3)))
        multList(tMatrixList, f).map(i => i.take(3))
      }
    }

    val semanticType = gm_AffTransformVertexListFunction :&: dimensionality_three_d_t
  }

  /*
Function to apply affine 2d transformation to 2d structure for a single vertex.
*/
  @combinator object ApplyAffineTransform2D {
    def apply(): (List[Float], MqttTransform) => List[Float] = {
      (p: List[Float], tMatrix: MqttTransform) => {
        val f = p :+ 1.0f
        val tMatrixList = tMatrix.transformMatrixList
        mult(tMatrixList, f).take(2)
      }
    }

    val semanticType = gm_AffTransformVertexFunction :&: dimensionality_two_d_t
  }



  /*
Function to apply affine 3d transformation to 3d structure for a single vertex.
*/
  @combinator object ApplyAffineTransform3D {
    def apply(): (List[Float], MqttTransform) => List[Float] = {
      (p: List[Float], tMatrix: MqttTransform) => {
        val f = p :+ 1.0f
        val tMatrixList = tMatrix.transformMatrixList
        mult(tMatrixList, f).take(3)
      }
    }

    val semanticType = gm_AffTransformVertexFunction :&: dimensionality_three_d_t
  }

  @combinator object CubeVertices3D {
    def apply: List[List[Float]] = List(
      List(-0.5f, -0.5f, -0.5f),
      List(0.5f, -0.5f, -0.5f),
      List(0.5f, 0.5f, -0.5f),
      List(-0.5f, 0.5f, -0.5f),
      List(-0.5f, -0.5f, 0.5f),
      List(0.5f, -0.5f, 0.5f),
      List(0.5f, 0.5f, 0.5f),
      List(-0.5f, 0.5f, 0.5f))

    val semanticType = gm_rectangular :&: dimensionality_three_d_t
  }

  @combinator object RectangleVertices2D {
    def apply: List[List[Float]] = List(
      List(-0.5f, -0.5f),
      List(0.5f, -0.5f),
      List(0.5f, 0.5f),
      List(-0.5f, 0.5f))

    val semanticType = gm_rectangular :&: dimensionality_two_d_t
  }

  //Todo check if math commons is occasionally faster: MatrixUtils.create
  def mult(a: List[List[Float]], b: List[Float]): List[Float] = for (row <- a) yield
    (row zip b).map { case ((matElem, coordinate)) => matElem * coordinate }.sum

  def multList(a: List[List[Float]], b: List[List[Float]]): List[List[Float]] =
    for (p <- b) yield mult(a, p)

/*
@combinator object SharedToDoubledVertices {
  // Recalculates a polygon that uses shared vertices
  // and returns array structure with doubled vertices
  // (https://docs.unity3d.com/Manual/AnatomyofaMesh.html)

  def apply = ???
  val semanticType = ???

}

  @combinator object DoubledToSharedVertexArray {
    // Recalculates a polygon that contains double vertices as used by Unity
    // (https://docs.unity3d.com/Manual/AnatomyofaMesh.html)
    // and returns a shared vertex array

    def apply = ???
    val semanticType = ???
  }*/
/*
  @combinator object PointDifferenceUnity {
    def apply: (vertexType, vertexType) => Float =
      (a: vertexType, b: vertexType) => {
        math.sqrt(
          (a zip b) map {
            case (x: Float, y: Float) => pow(x - y, 2)
          } reduce { (c, d) => abs(c) + abs(d) }).toFloat
      }
    val semanticType = ??? //geoPointDiffUnity
  }*/

//  @combinator object BoxNodes {
//    def apply (b: boxData) =
//      val semanticType =
//  }
/*  @combinator object UnityPolygon {
    def apply: unityNativeType  = (geo.intArray, geo.vertexArray)
    val semanticType = ??? // unityPolyType
  }

  @combinator object OpenGLToUnity {
    def apply(a: glNativeType): unityNativeType = (geo.intArray, geo.vertexArray)
    val semanticType = ??? //glPolyType =>: unityPolyType
  }*/

/*  @combinator object IntArray {
    def apply = ???
    val semanticType = ??? //solidPolygon :&: leftFaced :&: threeD
  }

  @combinator object VertexArray {
    def apply = ???
    val semanticType = ??? //solidPolygon :&: leftFaced :&: threeD
  }

  @combinator object StandardSolidPolygonCombinator3D {
    def apply = ???
    val semanticType = ??? //solidPolygon :&: leftFaced :&: threeD
  }

  @combinator object StandardSolidPolygonCombinator {
    def apply = ???
    val semanticType = ??? //solidPolygon :&: leftFaced :&: twoD
  }

  @combinator object StandardPolygonWithHoleCombinator {
    def apply = ???
    val semanticType = ??? //solidPolygon :&: leftFaced =>: solidPolygon
  }

  @combinator object CircleCombinator {
    def apply = ???
    val semanticType = ???
  }*/

  val kinding = Kinding.empty

  def reflected: ReflectedRepository[GeometricRepository] = {
    val repo = ReflectedRepository(
      this,
      classLoader = this.getClass.getClassLoader,
      substitutionSpace = this.kinding
    )
    repo
  }
}