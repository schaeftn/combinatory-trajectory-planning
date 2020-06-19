package org.combinators.ctp.repositories.toplevel

import io.circe.{Decoder, Encoder, Json}
import org.combinators.ctp.repositories.dynrepository.{SbmpAlg, SbmpCosts, SbmpDimensionality, SbmpMotionValidators, SbmpOptObjective, SbmpPlanners, SbmpSamplers, SbmpSceneInput, SbmpSimplification, SbmpStateValidators}
import scalax.collection.Graph
import scalax.collection.edge.WUnDiEdge

trait DecodeImplicits {
  implicit val plannerDecoder: Decoder[SbmpPlanners.Value] = Decoder.decodeEnumeration(SbmpPlanners)
  implicit val samplerDecoder: Decoder[SbmpSamplers.Value] = Decoder.decodeEnumeration(SbmpSamplers)
  implicit val stateValidatorDecoder: Decoder[SbmpStateValidators.Value] = Decoder.decodeEnumeration(SbmpStateValidators)
  implicit val motionValidatorDecoder: Decoder[SbmpMotionValidators.Value] = Decoder.decodeEnumeration(SbmpMotionValidators)
  implicit val costDecoder: Decoder[SbmpCosts.Value] = Decoder.decodeEnumeration(SbmpCosts)
  implicit val optObjDecoder: Decoder[SbmpOptObjective.Value] = Decoder.decodeEnumeration(SbmpOptObjective)
  implicit val simplificationDecoder: Decoder[SbmpSimplification.Value] = Decoder.decodeEnumeration(SbmpSimplification)
  implicit val sceneInputDecoder: Decoder[SbmpSceneInput.Value] = Decoder.decodeEnumeration(SbmpSceneInput)
  implicit val dimensionalityDecoder: Decoder[SbmpDimensionality.Value] = Decoder.decodeEnumeration(SbmpDimensionality)
}


