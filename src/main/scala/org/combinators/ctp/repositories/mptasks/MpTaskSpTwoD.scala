package org.combinators.ctp.repositories.mptasks

import org.combinators.ctp.repositories.geometricrepresentation.vertexType

case class MpTaskSpTwoD(startPoint: vertexType, endPoint: vertexType) {
  val description: String = "shortest path 2d"
}

case class MpTaskSpThreeD(startPoint: vertexType, endPoint: vertexType) {
  val description: String = "shortest path 3d"
}
