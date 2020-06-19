package org.combinators.ctp.repositories.toplevel

import io.circe.generic.JsonCodec
import org.combinators.ctp.repositories._

case class UnityMeshData(intArray: intListType, vertexArray: vertexArrayType)

case class MinBoxData(vertexArray: vertexArrayType)

case class BoundingBoxData(minVertex: vertexType, maxVertex: vertexType)

case class BoundingSphereData(center: vertexType, radius: Float)