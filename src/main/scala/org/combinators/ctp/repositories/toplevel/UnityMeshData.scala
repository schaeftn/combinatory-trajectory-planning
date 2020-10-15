package org.combinators.ctp.repositories.toplevel

import io.circe.generic.JsonCodec
import org.combinators.ctp.repositories._

case class UnityMeshData(intArray: List[Int], vertexArray: List[Float])

case class MinBoxData(vertexArray: List[Float])

case class BoundingBoxData(minVertex: List[Float], maxVertex: List[Float])

case class BoundingSphereData(center: List[Float], radius: Float)