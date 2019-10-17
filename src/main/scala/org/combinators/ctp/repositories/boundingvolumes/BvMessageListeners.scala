package org.combinators.ctp.repositories.boundingvolumes

import com.typesafe.scalalogging.LazyLogging
import io.circe.Decoder
import org.combinators.cls.interpreter.ReflectedRepository
import org.combinators.cls.types.Type
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.protocol.ClsMqttBvAgent.{bbRequestTopic, bbResponseTopic, bsRequestTopic, bsResponseTopic}
import org.combinators.ctp.repositories.protocol.{BoundingBoxData, BoundingSphereData, CtpMessageListener, UnityMeshData}

import scala.reflect.runtime.universe._
import io.circe.syntax._
import org.eclipse.paho.client.mqttv3.MqttClient

object CtpBbListener extends LazyLogging {
  def apply(c: MqttClient)(implicit t: WeakTypeTag[UnityMeshData => vertexPairType]) = {
    val Gamma = new BoundingVolumeRepository {}
    val reflectedRepository: ReflectedRepository[BoundingVolumeRepository] =
      ReflectedRepository(Gamma, Gamma.semanticTaxonomy, Gamma.kinding)
    val responseTopic: String = bbResponseTopic
    val requestTopic: String = bbRequestTopic
    val targetType: Type = bv_bounding_box
    val decoder = Decoder[UnityMeshData]

    def resultToByteArray: vertexPairType => Array[Byte] = { outputTy =>
      BoundingBoxData(outputTy._1, outputTy._2).asJson.toString.getBytes
    }

    def run = { a: UnityMeshData =>
      logger.info(s"Starting Inhabitation")
      val result = reflectedRepository.inhabit[UnityMeshData => vertexPairType](targetType)
      if (result.isEmpty) logger.info("Empty inhabitation result") else logger.info("Valid inhabitation result")
      result.interpretedTerms.index(0)(a)
    }

    CtpMessageListener[UnityMeshData, vertexPairType](run, decoder, resultToByteArray, requestTopic, responseTopic, c)
  }
}

object CtpBsListener extends LazyLogging {
  def apply(c: MqttClient)(implicit t: WeakTypeTag[UnityMeshData => sphereData]) = {
    val Gamma = new BoundingVolumeRepository {}
    val reflectedRepository: ReflectedRepository[BoundingVolumeRepository] =
      ReflectedRepository(Gamma, Gamma.semanticTaxonomy, Gamma.kinding)
    val responseTopic: String = bsResponseTopic
    val requestTopic: String = bsRequestTopic
    val targetType: Type = bv_bounding_sphere
    val decoder = Decoder[UnityMeshData]

    def resultToByteArray: sphereData => Array[Byte] = { outputTy =>
      BoundingSphereData(outputTy._1, outputTy._2).asJson.toString.getBytes
    }

    def run: UnityMeshData => sphereData = { a: UnityMeshData =>
      logger.info(s"Starting Inhabitation")
      val result = reflectedRepository.inhabit[UnityMeshData => sphereData](targetType)
      if (result.isEmpty) logger.info("Empty inhabitation result") else logger.info("Valid inhabitation result")
      result.interpretedTerms.index(0)(a)
    }

    CtpMessageListener[UnityMeshData, sphereData](run, decoder, resultToByteArray, requestTopic, responseTopic, c)
  }
}
