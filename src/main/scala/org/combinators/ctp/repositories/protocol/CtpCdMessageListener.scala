package org.combinators.ctp.repositories.protocol

import com.typesafe.scalalogging.LazyLogging
import io.circe._
import io.circe.parser.decode
import org.combinators.cls.interpreter.ReflectedRepository
import org.combinators.cls.types.Type
import org.eclipse.paho.client.mqttv3.{IMqttMessageListener, MqttClient, MqttMessage}

import scala.reflect.runtime.universe._

/**
 * Message listener for cell decomposition tasks.
 *
 * Main Actions in onMessage. Receives scene, returns scene and decomposed cells.
 * computeCd, resultToByteArray must be supplied in message listener implementation
 *
 * R: Native type of Scene
 * S: Native type of Scene
 */
trait CtpCdMessageListener[R, S] extends LazyLogging {
  val decoder: Decoder[R]
  val responseTopic: String
  val requestTopic: String
  val client: MqttClient

  def resultToByteArray: S => Array[Byte]
  def computeCd: R => S
  //TODO Implement general listeners: file writer and python calls

  def subscribe(): Unit = {
    println("Response Topic: " + requestTopic)
    client.subscribe(responseTopic, 2, this.onMessage)
  }

  def publishResult: S => Unit = { resultData =>
    logger.info("Publishing result to " + responseTopic)
    client.publish(requestTopic, resultToByteArray(resultData), 2, true)
  }


  def onMessage: IMqttMessageListener = (topic: String, message: MqttMessage) => {
    implicit val decodeEvent: Decoder[R] = decoder

    logger.info("Received Payload: " + new String(message.getPayload))
    val decoded = decode[R](new String(message.getPayload))
    logger.info("decoded payload")

    if (decoded.isLeft) {
      logger.info(s"Error: ${decoded.left.get}")
    } else {
      logger.info(s"Starting cell decomposition")
      val r = computeCd(decoded.right.get)
      logger.info("computed result")
      publishResult(r)
      logger.info("Result published")
    }
  }
}
