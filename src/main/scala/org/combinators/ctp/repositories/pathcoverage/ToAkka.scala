package org.combinators.ctp.repositories.pathcoverage

import akka.Done
import akka.stream.alpakka.mqtt.{MqttConnectionSettings, MqttMessage, MqttQoS}
import akka.stream.alpakka.mqtt.scaladsl.MqttSink
import akka.stream.scaladsl.{Sink, Source}
import akka.util.ByteString
import org.combinators.ctp.repositories.pathcoverage.ToAkka.mqttProperties
import io.circe.syntax._
import org.combinators.ctp.repositories.toplevel.{AkkaImplicits, PropertyFiles}
import org.eclipse.paho.client.mqttv3.persist.MemoryPersistence
import org.combinators.ctp.repositories.scene.SceneUtils
import org.combinators.ctp.repositories.toplevel.{AkkaImplicits, PropertyFiles}
import akka.stream.alpakka.mqtt.scaladsl.{MqttSink, MqttSource}
import akka.stream.alpakka.mqtt.{MqttConnectionSettings, MqttMessage, MqttQoS, MqttSubscriptions}
import akka.stream.scaladsl.{Sink, Source}
import io.circe.syntax._
import akka.stream.alpakka.mqtt.MqttMessage
import akka.util.ByteString
import akka.{Done, NotUsed}
import com.typesafe.scalalogging.LazyLogging
import org.eclipse.paho.client.mqttv3.persist.MemoryPersistence

import scala.concurrent.{ExecutionContext, Future}
import com.typesafe.scalalogging.LazyLogging
import org.combinators.ctp.repositories.pathcoverage.RunTrochoidalPath.asd
import org.combinators.ctp.repositories.scene.SceneUtils


object ToAkka extends PropertyFiles with AkkaImplicits {
  implicit val ev = ExecutionContext.global
  def apply[R](input: R): Unit = {
    val source = Source.single[R](input)
    val broker = mqttProperties.getProperty("org.combinators.ctp.broker")

    val connectionSettings = MqttConnectionSettings(broker, "cls/SimplePath",
      new MemoryPersistence).withAutomaticReconnect(true)

    val pathSink: Sink[MqttMessage, Future[Done]] = MqttSink(connectionSettings, MqttQoS.AtLeastOnce)

    def toMqttMsg[R](input: R): MqttMessage = {
      val topic = mqttProperties.getProperty("org.combinators.ctp.ctpPathfromScala")
      val jsonByteStr = input match {
        case i: List[List[Float]] => ByteString(i.asInstanceOf[List[List[Float]]].asJson.toString())
        case i: List[List[Double]] => ByteString(i.asInstanceOf[List[List[Double]]].asJson.toString())
      }
      MqttMessage(topic, jsonByteStr)
    }

    source.map(toMqttMsg).runWith(pathSink)
  }
}
