package org.combinators.ctp.repositories.toplevel

import io.circe.{Decoder, Encoder, Json}
import org.combinators.ctp.repositories.dynrepository._
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
  implicit val sceneInputDecoder: Decoder[SceneInput.Value] = Decoder.decodeEnumeration(SceneInput)
  implicit val dimensionalityDecoder: Decoder[Dimensionality.Value] = Decoder.decodeEnumeration(Dimensionality)

  implicit val cmpCellGraphDecoder: Decoder[CmpCellGraph.Value] = Decoder.decodeEnumeration(CmpCellGraph)
  implicit val cmpCellSegmentationDecoder: Decoder[CmpCellSegmentation.Value] = Decoder.decodeEnumeration(CmpCellSegmentation)
  implicit val cmpCellTypeDecoder: Decoder[CmpCellType.Value] = Decoder.decodeEnumeration(CmpCellType)
  implicit val cmpCentroidFctDecoder: Decoder[CmpCentroidFct.Value] = Decoder.decodeEnumeration(CmpCentroidFct)
  implicit val cmpGraphAlgDecoder: Decoder[CmpGraphAlg.Value] = Decoder.decodeEnumeration(CmpGraphAlg)
  implicit val cmpSceneInputDecoder: Decoder[CmpSceneInput.Value] = Decoder.decodeEnumeration(CmpSceneInput)
  implicit val cmpStartGoalFctDecoder: Decoder[CmpStartGoalFct.Value] = Decoder.decodeEnumeration(CmpStartGoalFct)
}


