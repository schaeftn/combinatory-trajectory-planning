package org.combinators.ctp.repositories.benchmarks


import akka.stream.ClosedShape
import akka.{Done, NotUsed}
import akka.stream.alpakka.mqtt.scaladsl.{MqttSink, MqttSource}
import akka.stream.alpakka.mqtt.{MqttConnectionSettings, MqttMessage, MqttQoS, MqttSubscriptions}
import akka.stream.scaladsl.{Broadcast, GraphDSL, RunnableGraph, Sink, Source, Zip}
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
import org.combinators.cls.interpreter.{InhabitationResult}
import org.combinators.ctp.repositories.geometry.GeometryUtils

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success, Try}
import scala.language.postfixOps

case class BenchmarkRequest(iterations: Int, async: Boolean = false, maxTime: Int = 10, maxMem: Int = 1024) {}

case class BenchmarkClient[S, T](sbmpAlg: SbmpAlg, algInterpreted: S => T) extends LazyLogging with AkkaImplicits with AkkaMqttComponents
  with PropertyFiles with GeometryUtils {
  val uuid = sbmpAlg.id
  implicit val ec: ExecutionContext = ExecutionContext.global
  val bmSceneSrtInputTopic = mqttProperties.getProperty("org.combinators.ctp.bmSceneSrtInput") + "." + uuid.toString
  val bmGenericInputTopic = mqttProperties.getProperty("org.combinators.ctp.bmGenericInput") + "." + uuid.toString
  val bmGenericInputResponseTopic =
    mqttProperties.getProperty("org.combinators.ctp.bmGenericInputResponse") + "." + uuid.toString
  val bmSceneMpTaskInputTopic =
    mqttProperties.getProperty("org.combinators.ctp.bmSceneMpTaskInput") + "." + uuid.toString
  val bmProblemFileInputTopic =
    mqttProperties.getProperty("org.combinators.ctp.bmProblemFileInput") + "." + uuid.toString
  val bmStartRequestTopic = mqttProperties.getProperty("org.combinators.ctp.bmStartRequest") + "." + uuid.toString
  val bmStartResponseTopic = mqttProperties.getProperty("org.combinators.ctp.bmStartResponse") + "." + uuid.toString
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
      MqttSubscriptions(subscribeTopic, MqttQoS.ExactlyOnce),
      bufferSize = 8).map { i =>
      logger.info(s"Received $logString: $i")
      decodeFct(i.message.payload.utf8String)
    }.filter {
      case Some(_) => true
      case None => false
    }.map(_.get)


  val sourceBmStartRequest: Source[BenchmarkRequest, Future[Done]] =
    buildListenerSource("cls/bmStartRequestListener" + "." + uuid.toString, bmStartRequestTopic, "BenchmarkStartRequest",
      { s => decode[BenchmarkRequest](s).toOption })

  val sourceGenericInput: Source[Any, Future[Done]] =
    sbmpAlg.sceneInput match {
      case SbmpSceneInput.sbmp_from_data_file if !sbmpAlg.configurableAlg =>
        buildListenerSource("cls/bmGenericInputListener" + "." + uuid.toString, bmGenericInputTopic, "GenericInput",
          { s => ProblemDefinitionFiles(s) })
      case SbmpSceneInput.sbmp_from_data_file if sbmpAlg.configurableAlg =>
        buildListenerSource("cls/bmGenericInputListener" + "." + uuid.toString, bmGenericInputTopic, "GenericInput",
          {
            str =>
              logger.info(s"decoding: $str")
              val decStr = decode[Option[(String, String)]](str).toOption
              logger.info(s"decode result: $decStr")

              decStr.map {
                case Some(tuple) =>
                  ProblemDefinitionFiles(tuple._1)
                  match {
                    case Some(a) => (a, tuple._2)
                    case None =>
                      logger.info("Error while loading problem definition file")
                      None
                  }
                case None =>
                  logger.info("Error while decoding")
                  None //Error decoding
              }
          })
    }

  val bmResultSink: Sink[MqttMessage, Future[Done]] = MqttSink(
    connectionSettings.withClientId("cls/BenchmarkResponse" + "." + uuid.toString), MqttQoS.ExactlyOnce)
  val bmGenericInputResponseSink: Sink[MqttMessage, Future[Done]] = MqttSink(
    connectionSettings.withClientId("cls/bmGenericInputResponse" + "." + uuid.toString), MqttQoS.ExactlyOnce)
  val bmStartResponse: Sink[MqttMessage, Future[Done]] = MqttSink(
    connectionSettings.withClientId("cls/bmStartResponse" + "." + uuid.toString), MqttQoS.ExactlyOnce)

  def bmRunGraph: RunnableGraph[NotUsed] = RunnableGraph.fromGraph(GraphDSL.create() { implicit b =>
    sbmpAlg.sceneInput match {
      case SbmpSceneInput.sbmp_from_data_file =>

        val bcast1 = b.add(Broadcast[S](2))
        val bcast2 = b.add(Broadcast[BenchmarkRequest](2))

        val gSource1 = b.add(sourceGenericInput.map(_.asInstanceOf[S]).take(1))
        val gSource2 = b.add(sourceBmStartRequest.take(1))
        val zip = b.add(Zip[S, BenchmarkRequest])
        logger.debug(s"Building zip...")

        gSource1 ~> bcast1
        gSource2 ~> bcast2

        bcast1.take(1).map { _ =>
          logger.info("Mapping bcast1 to bmGenericInputResponseSink")
          MqttMessage(bmGenericInputResponseTopic, ByteString("Success"))
        } ~> bmGenericInputResponseSink
        bcast2.take(1).map { _ =>
          logger.info("Mapping bcast2 to bmStartResponse")
          MqttMessage(bmStartResponseTopic, ByteString("Success"))
        } ~> bmStartResponse

        bcast1.take(1) ~> zip.in0
        bcast2.take(1) ~> zip.in1

        def toMqttMsg2(paths: List[List[List[Float]]], pathlengths: List[Float], computationTimes: List[Float], failures: Int): MqttMessage = {
          val rQuatruple = (paths, pathlengths, computationTimes, failures.toFloat)
          logger.info(s"Trying to encode result triple $rQuatruple")

          val foo = MqttMessage(bmResultTopic, ByteString(rQuatruple.asJson.toString()))
          logger.info(s"Publishing to topic: $bmResultTopic, message: $foo")
          foo
        }

        //        def toMqttMsg2(paths: List[List[List[Float]]], pathlengths: List[Float], computationTimes: List[Float], failures: Int): MqttMessage = {
        //          val rTriple = (
        //            paths,
        //            if (pathlengths.isNaN) 500000 else pathlengths
        //            , if (computationTimes.isNaN) 500000 else computationTimes, failures.toFloat)
        //          logger.info(s"Trying to encode result triple $rTriple")
        //
        //          val foo = MqttMessage(bmResultTopic, ByteString(rTriple.asJson.toString()))
        //          logger.info(s"Publishing to topic: $bmResultTopic, message: $foo")
        //          foo
        //        }

        def toMqttMsgStates(statesPaths: List[(List[List[Float]], List[List[Float]])], pathLengths: List[Float],
                            computationTimes: List[Float], failures: Int): MqttMessage = {
          val rTriple = (
            statesPaths,
            pathLengths, computationTimes, failures.toFloat)
          logger.info(s"Trying to encode result triple $rTriple")

          val foo = MqttMessage(bmResultTopic, ByteString(rTriple.asJson.toString()))
          logger.info(s"Publishing to topic: $bmResultTopic, message: $foo")
          foo
        }

        val zipMapped = zip.out.map {
          case (a, b) =>


            def singleRun(arg: S): (T, Float) = {
              val startTime = System.currentTimeMillis
              logger.debug(s"singleRun. Start time: $startTime")
              logger.debug(s"inside single run: running alg")
              val result = algInterpreted(arg)
              val stopTime = System.currentTimeMillis
              logger.debug(s"singleRun. Stop time: $stopTime")
              (result, stopTime - startTime)
            }

            def buildSingleResult(path: T, time: Float) = {
              if (sbmpAlg.withStates) {
                if (path.asInstanceOf[(List[List[Float]], List[List[Float]])]._2.isEmpty)
                  ((List.empty[List[Float]], List.empty[List[Float]]), 0.0f, time)
                else
                  (path.asInstanceOf[(List[List[Float]], List[List[Float]])], path_distance(path.asInstanceOf[(List[List[Float]], List[List[Float]])]._2), time)
              } else {
                if (path.asInstanceOf[List[Any]].isEmpty) {
                  (List.empty[List[Float]], 0.0f, time)
                } else {
                  (path.asInstanceOf[List[List[Float]]], path_distance(path.asInstanceOf[List[List[Float]]]), time)
                }
              }
            }
            def computeResults(): Seq [(Equals with Serializable, Float, Float)] = {
              if (b.async) {
                logger.debug(s"b.Iterations: ${b.iterations}")
                lazy val runList =
                  (1 to b.iterations).map(_ => Future {
                    {
                      logger.debug(s"inside future map")
                      singleRun(a)
                    }
                  })

                logger.debug(s"temp info runList: $runList")
                val resultsFutures =
                  runList.toList.map { f =>
                    f.map {
                      case (path, time) =>
                       buildSingleResult(path, time)
                    }
                  }
                logger.debug(s"Results length: ${resultsFutures.size}")

                val futureSequence = Future.sequence(resultsFutures)
                logger.debug(s"Duration: ${b.maxTime * 3 seconds}")
                val awaitedResults = Try(Await.result(futureSequence, b.maxTime * 3 seconds)) match {
                  case Success(res) => res // we can deal with the result directly
                  case Failure(e) => logger.info(s"Failed to await Future: $e")
                    IndexedSeq() // but we might have to figure out if a timeout happened
                }

                val filteredResults = awaitedResults.filter { case (_, a: Float, _) => a != 0.0 }
                filteredResults
              } else {
                lazy val runList =
                  (1 to b.iterations).map(_ => {
                    logger.debug(s"inside future map")
                    singleRun(a)
                  })

                logger.debug(s"temp info runList: $runList")
                val resultsFutures =
                  runList.toList.map { f =>
                    val path = f._1
                    val time = f._2
                    buildSingleResult(path, time)
                  }

                logger.debug(s"Results length: ${resultsFutures.size}")
                logger.debug(s"Duration: ${b.maxTime * 3 seconds}")

                val filteredResults = resultsFutures.filter { case (_, a:Float, _) => a != 0.0 }
                filteredResults
              }
            }

            val filteredResults: Seq[(Equals with Serializable, Float, Float)] = computeResults()

            logger.debug(s"Results filtered: $filteredResults")

            val pathLengths: List[Float] = if (filteredResults.isEmpty) List.empty[Float] else filteredResults.map(_._2).asInstanceOf[List[Float]]
            val cTimes: List[Float] = if (filteredResults.isEmpty) List.empty[Float] else filteredResults.map(_._3).asInstanceOf[List[Float]]
            logger.debug(s"pathLength: $pathLengths")
            logger.debug(s"cTimes: $cTimes")

            val msg = if (sbmpAlg.withStates) {
              val fResultsParam: List[(List[List[Float]], List[List[Float]])] = if (filteredResults.isEmpty)
                List((List.empty[List[Float]], List.empty[List[Float]]))
              else {
                val ff = filteredResults.asInstanceOf[List[((List[List[Float]], List[List[Float]]), Float, Float)]]
                val asd = ff.map(_._1)
                asd
              }
              toMqttMsgStates(fResultsParam,
                pathLengths,
                cTimes, b.iterations - filteredResults.length)
            } else {
              val fResultsParam = if (filteredResults.isEmpty)
                List.empty[List[List[Float]]]
              else
                filteredResults.asInstanceOf[List[(List[List[Float]], Float, Float)]].map(_._1)

              logger.debug(f"fResultsParam: $fResultsParam")
              toMqttMsg2(fResultsParam,
                pathLengths,
                cTimes, b.iterations - filteredResults.length) //add paths to mqttmsg}
            }
            logger.info(s"MqttMessage: $msg")
            msg
        }

        zipMapped ~> bmResultSink

        logger.info("Benchmark runGraph built.")
        ClosedShape

      case _ => logger.info("unhandled case: sceneInput != ProblemDefinitionFile")
        //        sourceSceneSrt ~> Sink.ignore
        //        sourceMpTaskStartGoal ~> Sink.ignore
        ClosedShape
    }
  })

  def runGraph(): Unit = {
    bmRunGraph.run()
    logger.info(s"Benchmark runGraph called. Listening for args and benchmark request.")
  }
}

object BenchmarkClient extends LazyLogging {
  def apply[S, T](alg: SbmpAlg, algInterpreted: S => T): BenchmarkClient[S, T] = {
    logger.debug(s"Building BenchmarkClient instance")
    new BenchmarkClient(alg, algInterpreted)
  }
}





