package org.combinators.ctp.repositories.boundingvolumes

import io.circe.Decoder
import org.combinators.cls.interpreter.ReflectedRepository
import org.combinators.cls.types.Type
import org.combinators.ctp.repositories.boundingvolumes.{BoundingVolumeRepository, bv_bounding_box, bv_bounding_sphere}
import org.combinators.ctp.repositories.geometricrepresentation.{sphereData, vertexPairType}
import org.combinators.ctp.repositories.protocol.ClsMqttBvAgent.{bbRequestTopic, bbResponseTopic, bsRequestTopic, bsResponseTopic}
import org.combinators.ctp.repositories.protocol.{BoundingBoxData, BoundingSphereData, CtpMessageListener, UnityMeshData}
import scala.reflect.runtime.universe._
import io.circe._
import io.circe.syntax._
import io.circe.parser.decode

trait CptBbValues {
  val Gamma = new BoundingVolumeRepository {}
  val reflectedRepository: ReflectedRepository[BoundingVolumeRepository] =
    ReflectedRepository(Gamma, Gamma.semanticTaxonomy, Gamma.kinding)
  val responseTopic: String = bbResponseTopic
  val requestTopic: String = bbRequestTopic
  val targetType: Type = bv_bounding_box
  val decoder = Decoder[UnityMeshData]
  implicit val typeTag: WeakTypeTag[UnityMeshData => vertexPairType]

  def resultToByteArray: vertexPairType => Array[Byte] = { outputTy =>
    BoundingBoxData(outputTy._1, outputTy._2).asJson.toString.getBytes
  }
}

object CtpBbListener {
  def apply(implicit t: WeakTypeTag[UnityMeshData => vertexPairType]) =
    new CtpMessageListener[UnityMeshData, vertexPairType, BoundingVolumeRepository] with CptBbValues {
      override val typeTag: WeakTypeTag[UnityMeshData => vertexPairType] = t
    }
}

trait CptBsValues {
  val Gamma = new BoundingVolumeRepository {}
  val reflectedRepository: ReflectedRepository[BoundingVolumeRepository] =
    ReflectedRepository(Gamma, Gamma.semanticTaxonomy, Gamma.kinding)
  val responseTopic: String = bsResponseTopic
  val requestTopic: String = bsRequestTopic
  val targetType: Type = bv_bounding_sphere
  val decoder = Decoder[UnityMeshData]
  implicit val typeTag: WeakTypeTag[UnityMeshData => sphereData]

  def resultToByteArray: sphereData => Array[Byte] = { outputTy =>
    BoundingSphereData(outputTy._1, outputTy._2).asJson.toString.getBytes
  }
}

object CtpBsListener {
  def apply(implicit t: WeakTypeTag[UnityMeshData => sphereData]) =
    new CtpMessageListener[UnityMeshData, sphereData, BoundingVolumeRepository] with CptBsValues {
      override val typeTag: WeakTypeTag[UnityMeshData => sphereData] = t
    }
}
