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
import org.combinators.ctp.repositories.dynrepository.SbmpAlg
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
import org.combinators.ctp.repositories.taxkinding.SbmpSemanticTypes
import org.combinators.ctp.repositories.{any_dimensionality_type, any_sbmp_optimization_objective_type, sbmp_planning_algorithm, util_file_reader_type}
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories.benchmarks.BenchmarkClient

import scala.concurrent.Future
import scala.concurrent.Future

object AkkaInhabitationAgent extends App with AkkaImplicits with LazyLogging with EncodeImplicits with PropertyFiles with SbmpSemanticTypes {
  val broker = mqttProperties.getProperty("org.combinators.ctp.broker")
  val ihRequestTopic = mqttProperties.getProperty("org.combinators.ctp.inhabitationRequest")
  val ihResponseTopic = mqttProperties.getProperty("org.combinators.ctp.inhabitationResponse")
  val strPattern: UUID => String = id => s"IhAgent${id.toString}"
  val connectionSettings = MqttConnectionSettings(broker, "cls/InhabitationRequestListener",
    new MemoryPersistence).withAutomaticReconnect(true)

  val testString =
    """{
      |  "planner" : "sbmp_planner_BITstar",
      |  "sampler" : "sbmp_gaussian_valid_state_sampler",
      |  "stateValidator" : "sbmp_fcl_validator",
      |  "motionValidator" : "sbmp_discrete_motion_validator",
      |  "costs" : "not_specified",
      |  "optObjective" : "not_specified",
      |  "simplification" : "sbmp_use_simplification",
      |  "sceneInput" : "sbmp_from_data_file",
      |  "dimensionality" : "dimensionality_three_d_t",
      |  "id":"0350"
      |}""".stripMargin

  val source: Source[Option[SbmpAlg], Future[Done]] = MqttSource.atLeastOnce(
    connectionSettings,
    MqttSubscriptions(ihRequestTopic, MqttQoS.AtLeastOnce),
    bufferSize = 8).map { i =>
    logger.info(s"Received inhabitationRequest: $i")

    decode[SbmpAlg](i.message.payload.utf8String) match {
      case Right(value) => Some(value)
      case Left(value) => logger.error(s"Failed to decode. String: \r\n ${i.message.payload.utf8String}")
        None
    }
  }

  val sceneSink: Sink[MqttMessage, Future[Done]] = MqttSink(
    connectionSettings.withClientId("cls/InhabitationResponseClient"), MqttQoS.AtLeastOnce)

  def toMqttMsg(id: String, s: String) = {
    val topic = ihResponseTopic + "." + id
    MqttMessage(topic, ByteString(s))
  }

  val streamGraph: RunnableGraph[NotUsed] = RunnableGraph.fromGraph(GraphDSL.create() {
    implicit b =>
      val mappedSource = source.map {
        case Some(alg) => {
          logger.info("starting...")
          val endpoint = MpEndpoint(alg)
          endpoint match {
            case None => toMqttMsg(alg.id.toString, "Inhabitation Empty")
            case Some(ep) =>
              logger.info("endpoint set up...")
              ep.run()
              toMqttMsg(alg.id.toString, alg.id.toString)
          }
        }
        case None => {
          toMqttMsg("-1", "-1, decoding Error for string")
        }
      }
      mappedSource ~> sceneSink
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

case class MpEndpoint(requestId: UUID, algModel: SbmpAlg, algInterpreted: UUID => Unit)
  extends Runnable with LazyLogging {
  def runAlgorithm(): Unit = {
  }

  def startInhabitation() = {}

  override def run(): Unit = {
    println("MpEndpoint: Starting")
    algInterpreted(requestId)
    println("MpEndpoint: Done")
  }
}


object MpEndpoint extends LazyLogging with SbmpSemanticTypes {
  def apply(sbmpAlg: SbmpAlg): Option[MpEndpoint] = {
    val repository = sbmpAlg.buildRepository
    logger.debug("Repository built. Starting inhabitation")

    val ihResult = repository.InhabitationBatchJob[PlannerScheme[List[List[Float]]]](any_sbmp_planner_type)
      .addJob[Any](any_sbmp_sampler_type)
      .addJob[Any](any_sbmp_state_validator_type)
      .addJob[Any](any_sbmp_motion_validator_type)
      .addJob[Any](any_sbmp_cost_type)
      .addJob[Any](any_sbmp_optimization_objective_type)
      .addJob[((SceneSRT, MpTaskStartGoal)) => SubstitutionScheme](sbmp_input_data)
      .addJob[(SceneSRT, MpTaskStartGoal) => SubstitutionScheme](sbmp_input_data)
      .addJob[Any](any_sbmp_simplification_type)
      .addJob[Any](any_dimensionality_type)
      .addJob[((SceneSRT, MpTaskStartGoal)) => List[List[Float]]](sbmp_planning_algorithm)
      .addJob[(SceneSRT, MpTaskStartGoal) => List[List[Float]]](sbmp_planning_algorithm)
      .addJob[UUID => Unit](p_mqttAkkaComposition_type)

    def getResultList(b: repository.InhabitationBatchJob) = {
      @scala.annotation.tailrec
      def getElements(l: List[InhabitationResult[Any]], bnew: b.ResultType): List[InhabitationResult[Any]] =
        bnew match {
          case (newJob: b.ResultType, result: InhabitationResult[Any]) => getElements(result +: l, newJob)
          case a: InhabitationResult[Any] => a +: l
        }

      getElements(List.empty, b.run())
    }

    val l = getResultList(ihResult)
    l.foreach(i => println((if (i.isEmpty) "inhabitant not found" else "inhabitant found") + "," + i.target.toString()))

    if (l.isEmpty || l.last.isEmpty) {
      logger.info("Inhabitation empty")
      None
    } else {
      logger.info("Running inhabitant.")
      val asd = new MpEndpoint(sbmpAlg.id, sbmpAlg,
        l.last.interpretedTerms.index(0).asInstanceOf[UUID=> Unit])
      logger.debug("repository built, inhab running")
      Some(asd)
    }
  }
}

