package org.combinators.ctp.repositories

import org.combinators.cls.types.Type
import org.combinators.cls.types.syntax._

package object geometricrepresentation {
  val solidPolygon: Type = 'solidPolygon
  val leftFaced: Type = 'leftFaced
  val circle: Type = 'circle
  val twoD:Type = 'twoD
  val threeD:Type = 'threeD
  val vertexIndexArray:Type = 'vertexIndexArary
  val vertexArray:Type = 'vertexArary
  val unityPolyType: Type = 'unityPolygon
  val glPolyType: Type = 'glPolyType
  val geoPointDiffUnity:Type = 'geoPointDiffUnity
  val unityBoxType: Type = 'unityBoxType

  type vertexType = List[Float]
  type glNativeType = Vector[Float]
  type unityNativeType = (Seq[Int],vertexArrayType)

  type sphereRadius = Float
  type sphereData = (vertexType,Float)
  type boxData = (vertexType,vertexType)

  type intListType = List[Int]
  type vertexArrayType = List[Float]

  type vertexPairType = (vertexType, vertexType)
}
