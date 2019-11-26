package org.combinators.ctp.repositories.mptasks

import io.circe.generic.JsonCodec
import org.combinators.ctp.repositories.geometricrepresentation.vertexType

@JsonCodec
case class MpTaskStartGoal(startPosition: List[Float], endPosition: List[Float]) {}

@JsonCodec
case class MpTaskStartGoalPose(startPoint: List[Float], startPose: List[Float], endPoint: List[Float], endPose: List[Float]) {}
