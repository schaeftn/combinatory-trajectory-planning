package org.combinators.ctp.repositories.protocol

import com.typesafe.scalalogging.LazyLogging
import io.circe._
import io.circe.parser.decode
import org.eclipse.paho.client.mqttv3.{IMqttMessageListener, MqttClient, MqttMessage}

/**
 * Listener Mqtt Messages.
 * Subscribes to topic, receives and decodes messages, performs a computation and publishes output via mqtt.
 *
 * Main Actions in onMessage.
 *
 * R: Native serializable type of Mqtt Input
 * S: Native serializable type of Mqtt Output
 */
trait CtpMessageListener[R, S] extends LazyLogging {

  val responseTopic: String
  val requestTopic: String
  val client: MqttClient

  val decoder: Decoder[R]
  def run: R=>S
  def encodeResult: S => Array[Byte]

  def subscribe(): Unit = {
    println("Response Topic: " + requestTopic)
    client.subscribe(responseTopic, 2, this.onMessage)
  }

  def publishResult: S => Unit = { resultData =>
    logger.info("Publishing result to " + responseTopic)
    client.publish(requestTopic, encodeResult(resultData), 2, true)
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
      val r = run(decoded.right.get)
      logger.info("computed result")
      publishResult(r)
      logger.info("Result published")
    }
  }
}

object CtpMessageListener {
  def apply[R, S](runFct: R => S, dec: Decoder[R], encodeRes: S => Array[Byte],
                  reqTopic: String, resTopic: String, pClient: MqttClient): CtpMessageListener[R, S] = {
    new CtpMessageListener[R, S] {
      override val responseTopic: String = resTopic
      override val requestTopic: String = reqTopic
      override val client: MqttClient = pClient
      override val decoder: Decoder[R] = dec

      override def run: R => S = runFct

      override def encodeResult: S => Array[Byte] = encodeRes
    }
  }
}



