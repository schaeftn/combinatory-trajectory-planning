package org.combinators.ctp.repositories.protocol

import org.combinators.ctp.repositories.geometricrepresentation.{sphereData, vertexPairType}
import org.combinators.ctp.repositories.boundingvolumes._
import org.combinators.ctp.repositories.protocol.ClsMqttBvAgent._
import org.combinators.cls.interpreter.ReflectedRepository
import org.combinators.cls.types.Type
import io.circe._
import io.circe.syntax._
import io.circe.parser.decode
import com.typesafe.scalalogging.LazyLogging
import org.eclipse.paho.client.mqttv3.{IMqttMessageListener, MqttMessage}

import scala.reflect.runtime.universe._

abstract class CtpMessageListener[R, S, T] extends LazyLogging {
  type fctType = R => S

  val targetType: Type
  val reflectedRepository: ReflectedRepository[T]
  val decoder: Decoder[R]
  val responseTopic: String
  val requestTopic: String

  def resultToByteArray: S => Array[Byte]
  implicit val typeTag: WeakTypeTag[fctType]

  def subscribe: Unit = client.subscribe(requestTopic, 2, this.onMessage)

  def publishResult: S => Unit = { interpretedTerm =>
    logger.info("Publishing result to " + responseTopic)
    client.publish(responseTopic, resultToByteArray(interpretedTerm), 2, true)
  }

  def onMessage: IMqttMessageListener = new IMqttMessageListener {
    override def messageArrived(topic: String, message: MqttMessage): Unit = {
      implicit val decodeEvent: Decoder[R] = decoder

     logger.info("Received Payload: " + new String(message.getPayload))
      val decoded = decode[R](new String(message.getPayload))
      logger.info("decoded payload")

      if (decoded.isLeft) {
        logger.info(s"Error: ${decoded.left.get}")
      } else {
        logger.info(s"Starting Inhabitation")
        val result = reflectedRepository.inhabit[fctType](targetType)
        if(result.isEmpty) logger.info("Empty inhabitation result") else logger.info("Valid inhabitation result")
        val r = result.interpretedTerms.index(0)(decoded.right.get)
        logger.info("computed result")
        publishResult(r)
        logger.info("Result published")
      }
    }
  }
}

