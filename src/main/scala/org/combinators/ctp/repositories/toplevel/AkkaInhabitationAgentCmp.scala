package org.combinators.ctp.repositories.toplevel

import java.nio.file.Paths
import java.util.UUID

import akka.stream.ClosedShape
import akka.{Done, NotUsed}
import akka.stream.alpakka.mqtt.scaladsl.{MqttSink, MqttSource}
import akka.stream.alpakka.mqtt.{MqttConnectionSettings, MqttMessage, MqttQoS, MqttSubscriptions}
import akka.stream.scaladsl.{FileIO, Framing, GraphDSL, Keep, RunnableGraph, Sink, Source, Zip}
import com.typesafe.scalalogging.LazyLogging
import io.circe.parser.decode
import io.circe.generic.auto._
import io.circe.syntax._
import org.combinators.cls.types.Constructor
import org.combinators.ctp.repositories.dynrepository.{CmpAlg, SbmpAlg, SceneInput}
import org.eclipse.paho.client.mqttv3.persist.MemoryPersistence
import akka.stream.ClosedShape
import akka.stream.alpakka.mqtt.MqttMessage
import akka.stream.scaladsl.GraphDSL.Implicits._
import akka.util.ByteString
import akka.{Done, NotUsed}
import org.combinators.cls.interpreter.InhabitationResult
import org.combinators.ctp.repositories.python_interop.{PlannerScheme, SubstitutionScheme}
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.runinhabitation.RunAkkaTopLevelCmp.{Gamma, ihBatch}
import org.combinators.ctp.repositories.taxkinding.{CtpSemanticTypes, SbmpSemanticTypes}
import org.combinators.ctp.repositories.{any_dimensionality_type, any_sbmp_optimization_objective_type, sbmp_planning_algorithm, util_file_reader_type}
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories.benchmarks.{BenchmarkClientSbmp, MpAkkaUtils}
import org.combinators.ctp.repositories.toplevel.AkkaInhabitationAgent.logger

import scala.concurrent.Future
import scala.concurrent.Future

object AkkaInhabitationAgentCmp extends App with AkkaImplicits with LazyLogging with EncodeImplicits with PropertyFiles with SbmpSemanticTypes {
  val broker = mqttProperties.getProperty("org.combinators.ctp.broker")
  val ihRequestTopic = mqttProperties.getProperty("org.combinators.ctp.inhabitationRequest")
  val ihResponseTopic = mqttProperties.getProperty("org.combinators.ctp.inhabitationResponse")
  val strPattern: UUID => String = id => s"IhAgent${id.toString}"
  val connectionSettings = MqttConnectionSettings(broker, "cls/InhabitationRequestListener",
    new MemoryPersistence).withAutomaticReconnect(true)

  val inhabRequestSource: Source[Option[CmpAlg], Future[Done]] = MqttSource.atLeastOnce(
    connectionSettings,
    MqttSubscriptions(ihRequestTopic, MqttQoS.AtLeastOnce),
    bufferSize = 8).map { i =>
    logger.info(s"Received inhabitationRequest: $i")

    decode[CmpAlg](i.message.payload.utf8String) match {
      case Right(value) => Some(value)
      case Left(_) => logger.error(s"Failed to decode. String: \r\n ${i.message.payload.utf8String}")
        None
    }
  }

  val inhabResponseSink: Sink[MqttMessage, Future[Done]] = MqttSink(
    connectionSettings.withClientId("cls/InhabitationResponseClient"), MqttQoS.AtLeastOnce)

  def toMqttMsg(id: String, s: String) = {
    val topic = ihResponseTopic + "." + id
    MqttMessage(topic, ByteString(s))
  }

  val streamGraph: RunnableGraph[NotUsed] = RunnableGraph.fromGraph(GraphDSL.create() {
    implicit b =>
      val mappedSource = inhabRequestSource.map {
        case Some(alg) => {
          logger.info("starting...")
          val endpoint = MpEndpointCmp(alg)
          endpoint match {
            case None => toMqttMsg(alg.id.toString, "Inhabitation Empty")
            case Some(ep) =>
              logger.info("endpoint set up...")
              ep.bmRunGraph.run()
              toMqttMsg(alg.id.toString, alg.id.toString)
          }
        }
        case None => {
          toMqttMsg("-1", "-1, decoding Error for string")
        }
      }
      mappedSource ~> inhabResponseSink
      ClosedShape
  })
  streamGraph.run()

  //  val alg = decode[SbmpAlg](testString).right.get
  //  InhabitationAgent(alg).run()
  println(UUID.randomUUID().toString)
  //alg.buildRepository.inhabit[Unit](Constructor("anyPlanner"))
  logger.info(s"Mqtt inhabitation agent running.")

  scala.io.StdIn.readLine()
  logger.info(s"Mqtt inhabitation agent disconnecting.")
  println("dnot")
}

case class MpEndpointCmp[S, T](algModel: CmpAlg, algInterpreted: S => T)
  extends LazyLogging with MpAkkaUtils {
  override val uuidString: String = algModel.id.toString()

  val sourceGenericInput: Source[Any, Future[Done]] =
    algModel.sceneInput match {
      case SceneInput.scene_input_data_file =>
        buildListenerSource[ProblemDefinitionFiles]("cls/bmGenericInputListener" + "." + uuidString, bmGenericInputTopic, "GenericInput",
          { s => ProblemDefinitionFiles(s) })
      case SceneInput.scene_input_mqtt =>
        buildListenerSource[(Scene, MpTaskStartGoal)]("cls/bmGenericInputListener" + "." + uuidString, bmGenericInputTopic, "GenericInput",
          {
            str =>
              logger.info(s"decoding: $str")
              val decStr = decode[(Scene, MpTaskStartGoal)](str).toOption
              logger.info(s"decode result: $decStr")

              decStr
          })
    }


  override def bmRunGraph: RunnableGraph[NotUsed] = RunnableGraph.fromGraph(GraphDSL.create() { implicit b =>
    algModel.sceneInput match {
      case SceneInput.scene_input_data_file =>
        def toMqttMsg(resultPath: List[List[Float]]): MqttMessage = {
          logger.debug(s"Trying to encode result path $resultPath")

          val foo = MqttMessage(bmResultTopic, ByteString(resultPath.asJson.toString()))
          logger.info(s"Publishing to topic: $bmResultTopic, message: $foo")
          foo
        }

        val mapped = sourceGenericInput.map(_.asInstanceOf[ProblemDefinitionFiles]).
          map(algInterpreted.asInstanceOf[ProblemDefinitionFiles => List[List[Float]]](_)).map(toMqttMsg)
        mapped ~> bmResultSink
        ClosedShape
      case SceneInput.scene_input_mqtt =>
        def toMqttMsg(resultGraphPath: PolySceneSegmentationRoadmapPath): MqttMessage = {
          logger.info(s"Trying to encode result graph path $resultGraphPath")
          logger.info(s"JSON: ${resultGraphPath.asJson.toString()}")

          val rMsg = MqttMessage(bmResultTopic, ByteString(resultGraphPath.asJson.toString()))
          logger.info(s"Publishing to topic: $bmResultTopic, message: $rMsg")
          rMsg
        }

        val mapped = sourceGenericInput.
          map(_.asInstanceOf[(Scene, MpTaskStartGoal)]).
          map(algInterpreted.asInstanceOf[((Scene, MpTaskStartGoal))=>PolySceneSegmentationRoadmapPath]).
          map(toMqttMsg)

        mapped ~> bmResultSink
        ClosedShape
    }
  })
}


object MpEndpointCmp extends LazyLogging with CtpSemanticTypes {
  def apply(cmpAlg: CmpAlg): Option[MpEndpointCmp[_, _]] = {
    val repository = cmpAlg.buildRepository
    logger.debug("Repository built. Starting inhabitation")

    val ihResult = cmpAlg.getIhResult(repository)



    def getResultList(b: repository.InhabitationBatchJob) = {
      @scala.annotation.tailrec
      def getElements(l: List[InhabitationResult[Any]], bnew: b.ResultType): List[InhabitationResult[Any]] =
        bnew match {
          case (newJob: b.ResultType, result: InhabitationResult[Any]) => getElements(result +: l, newJob)
          case a: InhabitationResult[Any] => a +: l
        }

      getElements(List.empty, b.run())
    }

    val l = getResultList(ihResult.asInstanceOf[repository.InhabitationBatchJob])
    l.foreach(i => println((if (i.isEmpty) "inhabitant not found" else "inhabitant found") + "," + i.target.toString()))

    if (l.isEmpty || l.last.isEmpty) {
      logger.info("Inhabitation empty")
      None
    } else {
      logger.info("Running inhabitant.")

      def algTupleInput(t: (Scene, MpTaskStartGoal) => PolySceneSegmentationRoadmapPath):
      ((Scene, MpTaskStartGoal)) => PolySceneSegmentationRoadmapPath = {
        case ((a, b)) =>
          t(a, b)
      }

      val asd = cmpAlg.sceneInput match {
        case SceneInput.scene_input_mqtt =>
          new MpEndpointCmp[(Scene, MpTaskStartGoal), PolySceneSegmentationRoadmapPath](cmpAlg,
            algTupleInput(l.last.interpretedTerms.index(0).
              asInstanceOf[(Scene, MpTaskStartGoal) => PolySceneSegmentationRoadmapPath]))
        case SceneInput.scene_input_data_file =>
          new MpEndpointCmp[ProblemDefinitionFiles, PolySceneSegmentationRoadmapPath](cmpAlg,
            l.last.interpretedTerms.index(0).
              asInstanceOf[ProblemDefinitionFiles => PolySceneSegmentationRoadmapPath])
      }
      logger.debug("repository built, inhab running")
      Some(asd)
    }
  }
}

