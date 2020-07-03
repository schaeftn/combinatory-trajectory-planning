package org.combinators.ctp.repositories.benchmarks

import java.util.UUID

import akka.stream.ClosedShape
import akka.{Done, NotUsed}
import akka.stream.alpakka.mqtt.scaladsl.{MqttSink, MqttSource}
import akka.stream.alpakka.mqtt.{MqttConnectionSettings, MqttMessage, MqttQoS, MqttSubscriptions}
import akka.stream.javadsl.ZipWithN
import akka.stream.scaladsl.{Flow, GraphDSL, Merge, RunnableGraph, Sink, Source, Zip, ZipN}
import akka.util.ByteString
import com.typesafe.scalalogging.LazyLogging
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.toplevel._
import org.eclipse.paho.client.mqttv3.persist.MemoryPersistence
import io.circe.generic.auto._
import io.circe.parser.decode
import io.circe.syntax._
import org.combinators.ctp.repositories.dynrepository.{SbmpAlg, SbmpSceneInput}
import GraphDSL.Implicits._
import akka.stream.alpakka.mqtt.javadsl.MqttMessageWithAck
import org.combinators.cls.interpreter.InhabitationResult
import org.combinators.ctp.repositories.benchmarks.testClient.{connectionSettings, logger}
import org.combinators.ctp.repositories.geometry.GeometryUtils
import org.combinators.ctp.repositories.python_interop.{PlannerScheme, SubstitutionScheme}
import org.combinators.ctp.repositories.toplevel.AkkaInhabitationAgent.{logger, source, toMqttMsg}
import scala.concurrent.duration._

import scala.concurrent.{Await, ExecutionContext, Future}

case class BenchmarkRequest(iterations: Int, async: Boolean = false, maxTime: Int = 10, maxMem: Int = 1024) {}

case class BenchmarkClient[S, T](sbmpAlg: SbmpAlg, algInterpreted: S => T) extends LazyLogging with AkkaImplicits with AkkaMqttComponents
  with PropertyFiles with GeometryUtils {
  val uuid = sbmpAlg.id
  implicit val ec: ExecutionContext = ExecutionContext.global
  val bmSceneSrtInputTopic = mqttProperties.getProperty("org.combinators.ctp.bmSceneSrtInput") + "." + uuid.toString
  val bmGenericInputTopic = mqttProperties.getProperty("org.combinators.ctp.bmGenericInput") + "." + uuid.toString
  val bmSceneMpTaskInputTopic =
    mqttProperties.getProperty("org.combinators.ctp.bmSceneMpTaskInput") + "." + uuid.toString
  val bmProblemFileInputTopic =
    mqttProperties.getProperty("org.combinators.ctp.bmProblemFileInput") + "." + uuid.toString
  val bmStartRequestTopic = mqttProperties.getProperty("org.combinators.ctp.bmStartRequest") + "." + uuid.toString
  val bmResultTopic = mqttProperties.getProperty("org.combinators.ctp.bmResult") + "." + uuid.toString
  val bmStatusTopic = mqttProperties.getProperty("org.combinators.ctp.bmStatus") + "." + uuid.toString
  val bmCloseTopic = mqttProperties.getProperty("org.combinators.ctp.bmClose") + "." + uuid.toString

  val algEndpoint = mqttProperties.getProperty("org.combinators.ctp.fileBasedSbmpRequest") + "." + uuid.toString

  val connectionSettings = MqttConnectionSettings(broker, "cls/BenchmarkClient",
    new MemoryPersistence).withAutomaticReconnect(true).withCleanSession(true)


  def buildListenerSource[F](clientId: String, subscribeTopic: String, logString: String,
                             decodeFct: String => Option[F]): Source[F, Future[Done]] =
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

  val sourceBmRequest: Source[BenchmarkRequest, Future[Done]] =
    buildListenerSource("bm/bmRequestListener" + "." + uuid.toString, bmStartRequestTopic, "BenchmarkStartRequest",
      { s => decode[BenchmarkRequest](s).toOption })

  val sourceMpTaskStartGoal: Source[MpTaskStartGoal, Future[Done]] =
    buildListenerSource("bm/MpTaskStartGoalListener" + "." + uuid.toString, bmSceneMpTaskInputTopic, "MpStartGoal",
      { s => decode[MpTaskStartGoal](s).toOption })

  val sourceSceneSrt: Source[SceneSRT, Future[Done]] =
    buildListenerSource("bm/SceneSrtListener" + "." + uuid.toString, bmSceneSrtInputTopic, "SceneSrt", { s => decode[SceneSRT](s).toOption })

  val sourceGenericInput: Source[Any, Future[Done]] =
    sbmpAlg.sceneInput match {
      case SbmpSceneInput.sbmp_from_data_file =>
        buildListenerSource("bm/GenericInputListener" + "." + uuid.toString, bmGenericInputTopic, "GenericInput",
          { s => ProblemDefinitionFiles(s) })
      case SbmpSceneInput.sbmp_from_unity =>
        buildListenerSource("bm/GenericInputListener" + "." + uuid.toString, bmGenericInputTopic, "GenericInput",
          { s => decode[(SceneSRT, MpTaskStartGoal)](s).toOption })
      case _ => buildListenerSource("bm/GenericInputListener" + "." + uuid.toString, bmGenericInputTopic, "GenericInput",
        { s => decode[(SceneSRT, MpTaskStartGoal)](s).toOption })
    }


  val sourceProblemDefinitionFiles: Source[ProblemDefinitionFiles, Future[Done]] =
    buildListenerSource("bm/ProblemDefinitionFileListener" + "." + uuid.toString, bmProblemFileInputTopic, "config file string",
      { s => ProblemDefinitionFiles(s) })

  val sourceInhabResponse: Source[ProblemDefinitionFiles, Future[Done]] =
    buildListenerSource("bm/ProblemDefinitionFileListener" + "." + uuid.toString, bmProblemFileInputTopic, "config file string",
      { s => ProblemDefinitionFiles(s) })

  val bmResultSink: Sink[MqttMessage, Future[Done]] = MqttSink(
    connectionSettings.withClientId("cls/BenchmarkResponse" + "." + uuid.toString), MqttQoS.ExactlyOnce)

  def bmRunGraph: RunnableGraph[NotUsed] = RunnableGraph.fromGraph(GraphDSL.create() { implicit b =>
    sbmpAlg.sceneInput match {
      case SbmpSceneInput.sbmp_from_data_file =>
        logger.debug(s"Building zip...")
        val zip = b.add(Zip[S, BenchmarkRequest])

        sourceGenericInput.map(i => i.asInstanceOf[S]) ~> zip.in0
        sourceBmRequest ~> zip.in1

        def toMqttMsg2(pathlength: Float, computationTime: Float, failures: Int): MqttMessage = {
          val fTriple = (
            if (pathlength.isNaN) 500000 else pathlength
            , if (computationTime.isNaN) 500000 else computationTime, failures.toFloat)
          logger.info(s"Trying to encode tuple $fTriple")

          val foo = MqttMessage(bmResultTopic, ByteString(fTriple.asJson.toString()))
          logger.info(s"Publishing to topic: $bmResultTopic, message: $foo")
          foo
        }

        val zipMapped = zip.out.map {
          case (a, b) => {
            def singleRun(arg: S): (T, Float) = {
              val startTime = System.currentTimeMillis
              logger.debug(s"singleRun. Start time: ${startTime}")
              val result = algInterpreted(arg)
              val stopTime = System.currentTimeMillis
              logger.debug(s"singleRun. Stop time: ${stopTime}")
              (result, stopTime - startTime)
            }

            val runList = (1 to b.iterations).map(_ => {
              logger.debug(s"inside map")
              singleRun(a)
            })

            logger.debug(s"runList: $runList")
            val results: IndexedSeq[(Float,Float)] =
              runList.map {
                case (path, time) =>
                  if (path.asInstanceOf[List[Any]].isEmpty)
                    (0, time)
                  else
                    (path_distance(path.asInstanceOf[List[List[Float]]]), time)
              }

            //            val foo = Future.sequence(results)
            //            val fooList2 = Await.result(foo, b.maxTime*1.5 seconds)
            //            logger.info(s"Results: $fooList2")
            val fooList:IndexedSeq[(Float, Float)] = results.filter { case (a, b) => a != 0 }

            logger.info(s"Results filtered: $fooList")
            val asd = fooList.map(_._1)
            val asd2 = fooList.map(_._2)

            val msg = toMqttMsg2(
              asd.sum / asd.length,
              asd2.sum / asd2.length, b.iterations - fooList.length) //add paths to mqttmsg
            logger.info(s"msg: $msg")
            msg
          }
        }


        zipMapped ~> bmResultSink
        logger.debug("bm runGraph built")
        ClosedShape

      case _ => logger.info("unhandled case: sceneInput != ProblemDefinitionFile")
        sourceSceneSrt ~> Sink.ignore
        sourceMpTaskStartGoal ~> Sink.ignore
        ClosedShape
    }
  })

  def runGraph(): Unit = {
    bmRunGraph.run()
    logger.info(s"run bm called. Graph run...")

  }
}

object BenchmarkClient extends LazyLogging {
  def apply[S, T](alg: SbmpAlg, algInterpreted: S => T): BenchmarkClient[S, T] = {
    logger.debug(s"Building bmc instance")
    new BenchmarkClient(alg, algInterpreted)
  }
}

object testClient extends App with LazyLogging with AkkaImplicits with PropertyFiles with AkkaMqttComponents {
  val bmInitRequestTopic = mqttProperties.getProperty("org.combinators.ctp.bmInitRequest")
  val bmInitResponseTopic = mqttProperties.getProperty("org.combinators.ctp.bmInitResponse")
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


  val connectionSettings = MqttConnectionSettings(broker, "cls/BenchmarkClient",
    new MemoryPersistence).withAutomaticReconnect(true)

  val bmInitSink: Sink[MqttMessage, Future[Done]] = MqttSink(
    connectionSettings.withClientId("cls/bmInitResponse"), MqttQoS.AtMostOnce)

  val bmInitSource: Source[SbmpAlg, Future[Done]] = MqttSource.atLeastOnce(
    connectionSettings,
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
      MpInstance[ProblemDefinitionFiles, List[List[Float]]](i) match {
        case Some(a) =>
          val f = BenchmarkClient[ProblemDefinitionFiles, List[List[Float]]](i, a)
          logger.debug("BenchmarkClient instantiated, running inputListener.")
          f.runGraph()
          logger.info(s"after map and run Benchmarks")
          MqttMessage(bmInitResponseTopic + "." + f.sbmpAlg.id.toString, ByteString("Success"))
        case None => logger.info("No valid inhabitant found, ignoring bm/Init request")
          MqttMessage(bmInitResponseTopic + "." + i.id.toString, ByteString("Failure: No valid inhabitant found, ignoring bm/Init request"))
      })
    fooo ~> bmInitSink
    ClosedShape
  })
  g.run()
  logger.info(s"Mqtt bm init client starting.")

  scala.io.StdIn.readLine()
  logger.info(s"Done. Mqtt inhabitation agent disconnecting.")
}

object MpInstance extends LazyLogging {
  def apply[T, S](sbmpAlg: SbmpAlg): Option[T => S] = {
    val repository = sbmpAlg.buildRepository
    logger.debug("Repository built. Starting inhabitation")

    val ihResult = repository.InhabitationBatchJob[PlannerScheme[Any, Any]](any_sbmp_planner_type)
      .addJob[PlannerScheme[ProblemDefinitionFiles, List[List[Float]]]](any_sbmp_planner_type)
      .addJob[Any](any_sbmp_sampler_type)
      .addJob[Any](any_sbmp_state_validator_type)
      .addJob[Any](any_sbmp_motion_validator_type)
      .addJob[Any](any_sbmp_cost_type)
      .addJob[Any](any_sbmp_optimization_objective_type)
      .addJob[ProblemDefinitionFiles => SubstitutionScheme](sbmp_input_data)
      .addJob[Any](any_sbmp_simplification_type)
      .addJob[Any](any_dimensionality_type)
      .addJob[ProblemDefinitionFiles => List[List[Float]]](sbmp_planning_algorithm)

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
    l.foreach(i => logger.debug((if (i.isEmpty) "inhabitant not found" else "inhabitant found") + "," + i.target.toString()))

    if (l.isEmpty || l.last.isEmpty) {
      logger.info("Inhabitation empty")
      None
    } else {
      logger.info("Running inhabitant.")
      // l.last.interpretedTerms.index(0).asInstanceOf[T => S]
      val r = l.last.interpretedTerms.index(0).asInstanceOf[T => S]
      logger.debug("repository built, inhab running")
      Some(r)
    }
  }


}