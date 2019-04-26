package org.combinators.ctp.repositories.geometricrepresentation

import org.combinators.cls.interpreter._
import org.combinators.cls.types.{Kinding, Type}
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories.protocol.UnityMeshData
import org.eclipse.paho.client.mqttv3.MqttClient
import math._

class GeometricDataFromUnity(geo: UnityMeshData) {

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
  }

  @combinator object PointDifferenceUnity {
    def apply: (vertexType, vertexType) => Float =
      (a: vertexType, b: vertexType) => {
        math.sqrt(
          (a zip b) map {
            case (x: Float, y: Float) => pow(x - y, 2)
          } reduce { (c, d) => abs(c) + abs(d) }).toFloat
      }
    val semanticType = geoPointDiffUnity
  }

//  @combinator object BoxNodes {
//    def apply (b: boxData) =
//      val semanticType =
//  }
  @combinator object UnityPolygon {
    def apply: unityNativeType  = (geo.intArray, geo.vertexArray)
    val semanticType = unityPolyType
  }

  @combinator object OpenGLToUnity {
    def apply(a: glNativeType): unityNativeType = (geo.intArray, geo.vertexArray)
    val semanticType = glPolyType =>: unityPolyType
  }

  @combinator object IntArray {
    def apply = ???
    val semanticType = solidPolygon :&: leftFaced :&: threeD
  }

  @combinator object VertexArray {
    def apply = ???
    val semanticType = solidPolygon :&: leftFaced :&: threeD
  }

  @combinator object StandardSolidPolygonCombinator3D {
    def apply = ???
    val semanticType = solidPolygon :&: leftFaced :&: threeD
  }

  @combinator object StandardSolidPolygonCombinator {
    def apply = ???
    val semanticType = solidPolygon :&: leftFaced :&: twoD
  }

  @combinator object StandardPolygonWithHoleCombinator {
    def apply = ???
    val semanticType = solidPolygon :&: leftFaced =>: solidPolygon
  }

  @combinator object CircleCombinator {
    def apply = ???
    val semanticType = circle
  }

  val kinding = Kinding.empty

  def reflected: ReflectedRepository[GeometricDataFromUnity] = {
    val repo = ReflectedRepository(
      this,
      classLoader = this.getClass.getClassLoader,
      substitutionSpace = this.kinding
    )
    repo
  }
}