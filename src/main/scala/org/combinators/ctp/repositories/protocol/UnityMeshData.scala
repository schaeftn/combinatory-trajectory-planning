package org.combinators.ctp.repositories.protocol

import io.circe.generic.JsonCodec
import org.combinators.ctp.repositories._

@JsonCodec
case class UnityMeshData(intArray: intListType, vertexArray: vertexArrayType)

//TODO: Rename?!
@JsonCodec
case class MinBoxData(vertexArray: vertexArrayType)

@JsonCodec
case class BoundingBoxData(minVertex: vertexType, maxVertex: vertexType)

@JsonCodec
case class BoundingSphereData(center: vertexType, radius: Float)