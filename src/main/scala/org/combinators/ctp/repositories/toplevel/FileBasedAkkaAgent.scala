package org.combinators.ctp.repositories.toplevel

import java.io.File
import java.util.UUID

import akka.stream.ClosedShape
import akka.stream.alpakka.mqtt.MqttMessage
import akka.stream.scaladsl.GraphDSL.Implicits._
import akka.stream.scaladsl.{GraphDSL, RunnableGraph, Sink, Source}
import akka.util.ByteString
import akka.{Done, NotUsed}
import com.typesafe.scalalogging.LazyLogging
import io.circe.syntax._
import org.combinators.cls.interpreter.combinator
import org.combinators.cls.types.Constructor
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories._
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.concurrent.{Await, ExecutionContext, Future}

import scala.concurrent.{ExecutionContext, Future}
import scala.io.StdIn._

trait FileBasedAkkaAgent extends LazyLogging with AkkaImplicits with PropertyFiles {
  implicit val ec: ExecutionContext=  ExecutionContext.global
  /*Akka endpoint that lists available problem instances*/
  @combinator object ProblemFileListAkkaAgent {
    def apply(sourceMqtt: Source[MqttMessage, Future[Done]],
              sceneSink: Sink[MqttMessage, Future[Done]]): RunnableGraph[NotUsed] = {
      logger.info(s"Akka file problem instances")

      def toMqttMsg(s: List[String]) = {
        val topic = mqttProperties.getProperty("org.combinators.ctp.problemFileList")
        MqttMessage(topic, ByteString(s.asJson.toString()))
      }

      val probDir = problemsProperties.getProperty("org.combinators.ctp.problemFolder")

      val d = new File(probDir)
      val cfgFiles: IndexedSeq[String] = if (d.exists && d.isDirectory) {
        d.listFiles.filter(_.isFile).map(_.getName).filter(_.endsWith("cfg")).toIndexedSeq
      } else {
        IndexedSeq.empty[String]
      }

      val streamGraph: RunnableGraph[NotUsed] = RunnableGraph.fromGraph(GraphDSL.create() { implicit b =>
        sourceMqtt.map(
          _ => toMqttMsg(cfgFiles.toList)
        ) ~> sceneSink
        ClosedShape
      })

      streamGraph
    }

    val semanticType =
      p_mqttAkkaSource_type :&: util_file_list_type =>:
        p_mqttAkkaSink_type :&: util_file_list_type =>:
        p_mqttAkkaComposition_type :&: util_file_list_type
  }

  /*Akka endpoint that starts a mp program for loaded Probdef Files.
  Combinator will be used by toplevel combinator that runs two graphs for 1) listing 2) execution */
  @combinator object FileBasedTopLevelSbmpAkka {
    def apply(sourceMqtt: UUID => Source[Option[ProblemDefinitionFiles], Future[Done]],
              sceneSink: Sink[MqttMessage, Future[Done]],
              composedFunction: (ProblemDefinitionFiles => List[List[Float]])): UUID => Unit = {
      uuid =>
        logger.info(s"FileBasedTopLevelSbmpAkka starting.")
        val topic = mqttProperties.getProperty("org.combinators.ctp.fileBasedSbmpResponse") + "." + uuid.toString

        def streamGraph: RunnableGraph[NotUsed] = {
          RunnableGraph.fromGraph(GraphDSL.create() { implicit b =>
            def toMqttMsg(s: List[List[Float]]) =
              MqttMessage(topic , ByteString(s.asJson.toString()))

            def errorMessage(s: String): MqttMessage = MqttMessage(topic, ByteString(s"Failed to load config file $s"))

            sourceMqtt(uuid).mapAsync(10) {
              case Some(i) => {
                val f = Future {
                  composedFunction(i)
                }
                val out = f.map(toMqttMsg)
                out.onComplete(i => logger.info(s"Sending result via Mqtt: $i"))
                out
              }
              case None => Future{errorMessage("Error occured in FileBasedTopLevelSbmpAkka")}
            } ~> sceneSink
            ClosedShape
          })
        }

        streamGraph.run()

        println("Akka agent running. Press any key to quit.")
        val a = readLine()
    }

    val semanticType =
      p_mqttAkkaSource_type :&: util_file_reader_type =>:
        p_mqttAkkaSink_type :&: cmp_path_only =>:
        sbmp_planning_algorithm =>:
        p_mqttAkkaComposition_type :&: util_file_reader_type
  }
}
