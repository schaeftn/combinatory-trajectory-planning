package org.combinators.ctp.repositories.benchmarks

import akka.stream.ClosedShape
import akka.Done
import akka.stream.alpakka.mqtt.scaladsl.{MqttSink, MqttSource}
import akka.stream.alpakka.mqtt.{MqttConnectionSettings, MqttMessage, MqttQoS, MqttSubscriptions}
import akka.stream.scaladsl.{Broadcast, GraphDSL, RunnableGraph, Sink, Source, Zip}
import akka.util.ByteString
import com.typesafe.scalalogging.LazyLogging
import org.combinators.ctp.repositories.toplevel._
import org.eclipse.paho.client.mqttv3.persist.MemoryPersistence
import io.circe.generic.auto._
import io.circe.parser.decode
import io.circe.syntax._
import org.combinators.ctp.repositories.dynrepository.{SbmpAlg}
import GraphDSL.Implicits._
import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps

object RunBmClient extends App with LazyLogging with AkkaImplicits with PropertyFiles with AkkaMqttComponents {
  val bmInitRequestTopic = mqttProperties.getProperty("org.combinators.ctp.bmInitRequest")
  val bmInitResponseTopic = mqttProperties.getProperty("org.combinators.ctp.bmInitResponse")
  val bmInitWithConfigRequestTopic = mqttProperties.getProperty("org.combinators.ctp.bmInitRequest")
  val bmInitWithConfigResponseTopic = mqttProperties.getProperty("org.combinators.ctp.bmInitResponse")
  implicit val ec: ExecutionContext = ExecutionContext.global

  def buildListenerSource[T](clientId: String, subscribeTopic: String, logString: String,
                             decodeFct: String => Option[T]): Source[T, Future[Done]] =
    MqttSource.atLeastOnce(
      connectionSettings.withClientId(clientId),
      MqttSubscriptions(subscribeTopic, MqttQoS.AtLeastOnce),
      bufferSize = 8).map { i =>
      logger.info(s"Received $logString: $i")
      decodeFct(i.message.payload.utf8String)
    }.filter {
      case Some(_) => true
      case None => false
    }.map(_.get)

  val connectionSettings = MqttConnectionSettings(broker, "cls/BenchmarkClientConfig",
    new MemoryPersistence).withAutomaticReconnect(true)

  val bmInitSink: Sink[MqttMessage, Future[Done]] = MqttSink(
    connectionSettings.withClientId("cls/bmInitResponse"), MqttQoS.AtMostOnce)

  val bmInitSource: Source[SbmpAlg, Future[Done]] = MqttSource.atLeastOnce(
    connectionSettings.withCleanSession(true),
    MqttSubscriptions(bmInitRequestTopic, MqttQoS.AtLeastOnce),
    bufferSize = 8).map { i =>
    logger.info(s"Received SbmpAlg: $i")
    decode[SbmpAlg](i.message.payload.utf8String).toOption
  }.filter {
    case Some(_) => true
    case None => print("Failed to decode SbmpAlg, ignoring message")
      false
  }.map(_.get)

  val g = RunnableGraph.fromGraph(GraphDSL.create() { implicit b =>
    val fooo = bmInitSource.map(i =>
      i.configurableAlg match {
        case true =>
          MpInstance[SbmpAlg, (ProblemDefinitionFiles, Map[String, String]), List[List[Float]]](i) match {
            case Some(a) =>
              val f = BenchmarkClientSbmp[(ProblemDefinitionFiles, Map[String, String]), List[List[Float]]](i, a)
              logger.debug("BenchmarkClient instantiated, running inputListener.")
              f.runGraph()
              logger.info(s"after map and run Benchmarks")
              MqttMessage(bmInitResponseTopic + "." + f.sbmpAlg.id.toString, ByteString("Success"))
            case None => logger.info("No valid inhabitant found, ignoring bm/Init request")
              MqttMessage(bmInitResponseTopic + "." + i.id.toString, ByteString("Failure: No valid inhabitant found, ignoring bm/Init request"))
          }
        case false =>
          MpInstance[SbmpAlg, ProblemDefinitionFiles, List[List[Float]]](i) match {
            case Some(a) =>
              val f = BenchmarkClientSbmp[ProblemDefinitionFiles, List[List[Float]]](i, a)
              logger.info("Benchmark client instantiated, starting input listeners.")
              f.runGraph()
              logger.info(s"Benchmark graph run() called.")
              logger.info(s"Sending success msg: ${MqttMessage(bmInitResponseTopic + "." + f.sbmpAlg.id.toString, ByteString("Success"))}")
              MqttMessage(bmInitResponseTopic + "." + f.sbmpAlg.id.toString, ByteString("Success"))
            case None => logger.info("No valid inhabitant found, ignoring bm/Init request")
              MqttMessage(bmInitResponseTopic + "." + i.id.toString, ByteString("Failure: No valid inhabitant found, ignoring bm/Init request"))
          }
      }
    )
    fooo ~> bmInitSink
    ClosedShape
  })
  g.run()
  logger.info(s"Mqtt bm init client starting.")

  scala.io.StdIn.readLine()
  logger.info(s"Done. Mqtt inhabitation agent disconnecting.")
}

