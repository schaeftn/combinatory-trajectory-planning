package org.combinators.ctp.repositories.benchmarks

import akka.{Done, NotUsed}
import akka.stream.alpakka.mqtt.{MqttConnectionSettings, MqttMessage, MqttQoS, MqttSubscriptions}
import akka.stream.alpakka.mqtt.scaladsl.{MqttSink, MqttSource}
import akka.stream.scaladsl.{RunnableGraph, Sink, Source}
import com.typesafe.scalalogging.LazyLogging
import org.combinators.ctp.repositories.geometry.GeometryUtils
import org.combinators.ctp.repositories.toplevel.{AkkaImplicits, AkkaMqttComponents, PropertyFiles}
import org.eclipse.paho.client.mqttv3.persist.MemoryPersistence
import io.circe.generic.auto._
import io.circe.parser.decode
import io.circe.syntax._

import scala.concurrent.{ExecutionContext, Future}


trait MpAkkaUtils extends LazyLogging with AkkaImplicits
  with AkkaMqttComponents with PropertyFiles with GeometryUtils {
  implicit val ec: ExecutionContext = ExecutionContext.global
  val uuidString: String
  lazy val bmSceneSrtInputTopic: String =
    mqttProperties.getProperty("org.combinators.ctp.bmSceneSrtInput") + "." + uuidString
  lazy val bmGenericInputTopic: String =
    mqttProperties.getProperty("org.combinators.ctp.bmGenericInput") + "." + uuidString
  lazy val bmGenericInputResponseTopic: String =
    mqttProperties.getProperty("org.combinators.ctp.bmGenericInputResponse") + "." + uuidString
  lazy val bmSceneMpTaskInputTopic: String =
    mqttProperties.getProperty("org.combinators.ctp.bmSceneMpTaskInput") + "." + uuidString
  lazy val bmProblemFileInputTopic: String =
    mqttProperties.getProperty("org.combinators.ctp.bmProblemFileInput") + "." + uuidString
  lazy val bmStartRequestTopic: String =
    mqttProperties.getProperty("org.combinators.ctp.bmStartRequest") + "." + uuidString
  lazy val bmStartResponseTopic: String =
    mqttProperties.getProperty("org.combinators.ctp.bmStartResponse") + "." + uuidString
  lazy val bmResultTopic: String = mqttProperties.getProperty("org.combinators.ctp.bmResult") + "." + uuidString
  lazy val bmStatusTopic: String = mqttProperties.getProperty("org.combinators.ctp.bmStatus") + "." + uuidString
  lazy val bmCloseTopic: String = mqttProperties.getProperty("org.combinators.ctp.bmClose") + "." + uuidString
  lazy val algEndpoint: String =
    mqttProperties.getProperty("org.combinators.ctp.fileBasedSbmpRequest") + "." + uuidString


  val connectionSettings = MqttConnectionSettings(broker, "cls/BenchmarkClient",
    new MemoryPersistence).withAutomaticReconnect(true).withCleanSession(true)

  def buildListenerSource[F](clientId: String, subscribeTopic: String, logString: String,
                             decodeFct: String => Option[F]): Source[F, Future[Done]] =
    MqttSource.atLeastOnce(
      connectionSettings.withClientId(clientId),
      MqttSubscriptions(subscribeTopic, MqttQoS.ExactlyOnce),
      bufferSize = 8).map { i =>
      logger.info(s"Received $logString: $i")
      decodeFct(i.message.payload.utf8String)
    }.filter {
      case Some(_) => true
      case None => false
    }.map(_.get)

  lazy val sourceBmStartRequest: Source[BenchmarkRequest, Future[Done]] =
    buildListenerSource("cls/bmStartRequestListener" + "." + uuidString, bmStartRequestTopic, "BenchmarkStartRequest",
      { s => decode[BenchmarkRequest](s).toOption })
  lazy val bmResultSink: Sink[MqttMessage, Future[Done]] = MqttSink(
    connectionSettings.withClientId("cls/BenchmarkResponse" + "." + uuidString), MqttQoS.ExactlyOnce)
  lazy val bmGenericInputResponseSink: Sink[MqttMessage, Future[Done]] = MqttSink(
    connectionSettings.withClientId("cls/bmGenericInputResponse" + "." + uuidString), MqttQoS.ExactlyOnce)
  lazy val bmStartResponse: Sink[MqttMessage, Future[Done]] = MqttSink(
    connectionSettings.withClientId("cls/bmStartResponse" + "." + uuidString), MqttQoS.ExactlyOnce)

  def bmRunGraph: RunnableGraph[NotUsed]

  def runGraph(): Unit = {
    bmRunGraph.run()
    logger.info(s"Benchmark runGraph called. Listening for args and benchmark request.")
  }
}