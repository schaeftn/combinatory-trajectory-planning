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
import org.combinators.ctp.repositories.dynrepository.{CmpAlg, SbmpAlg, SceneInput}
import GraphDSL.Implicits._
import org.combinators.cls.interpreter.InhabitationResult
import org.combinators.ctp.repositories.geometry.GeometryUtils
import org.combinators.ctp.repositories.python_interop.{PythonTemplateUtils, PythonWrapper, SimplePythonWrapper, SubstitutionScheme}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success, Try}
import scala.language.postfixOps

case class BenchmarkRequest(iterations: Int, async: Boolean = false, maxTime: Int = 10, maxMem: Int = 1024) {}

case class StatesPathBmResults(states: List[List[List[Float]]], paths: List[List[List[Float]]],
                               pathLengths: List[Float], computationTimes: List[Float], failures: Int)

case class PathBmResults(paths: List[List[List[Float]]], pathLengths: List[Float],
                         computationTimes: List[Float], failures: Int)

case class BenchmarkClientSbmp[S, T](sbmpAlg: SbmpAlg, algInterpreted: S => T) extends MpAkkaUtils with PythonTemplateUtils {
  val uuidString = sbmpAlg.id.toString
  val sourceGenericInput: Source[Any, Future[Done]] =
    sbmpAlg.sceneInput match {
      case SceneInput.scene_input_data_file if !sbmpAlg.configurableAlg =>
        buildListenerSource("cls/bmGenericInputListener" + "." + uuidString, bmGenericInputTopic, "GenericInput",
          { s => ProblemDefinitionFiles(s) })
      case SceneInput.scene_input_data_file if sbmpAlg.configurableAlg =>
        buildListenerSource("cls/bmGenericInputListener" + "." + uuidString, bmGenericInputTopic, "GenericInput",
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

  def validatePath(s: List[List[Float]]): Boolean = {
    val a = SubstitutionScheme.apply(Map(pathDataTemplate -> pathDataFile),
      Map("$path_data.path$" -> writePyPathData(s))) //File in, File out
    val fct: String => Boolean = a => a.contains("Path is valid")
    PythonWrapper(a, samplingFolder + "path_validator.py", fct).computeResult
  }

  def bmRunGraph: RunnableGraph[NotUsed] = RunnableGraph.fromGraph(GraphDSL.create() { implicit b =>
    sbmpAlg.sceneInput match {
      case SceneInput.scene_input_data_file =>
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
          val result = PathBmResults(paths, pathlengths, computationTimes, failures)
          logger.info(s"Trying to encode result $result")

          val mqttMsg = MqttMessage(bmResultTopic, ByteString(result.asJson.toString()))
          logger.info(s"Publishing to topic: $bmResultTopic, message: $mqttMsg")
          mqttMsg
        }

        def toMqttMsgStates(statesPaths: List[(List[List[Float]], List[List[Float]])], pathLengths: List[Float],
                            computationTimes: List[Float], failures: Int): MqttMessage = {
          val result = StatesPathBmResults(statesPaths.map(_._1), statesPaths.map(_._2),
            pathLengths, computationTimes, failures)
          logger.info(s"Trying to encode result triple $result")

          val mqttMsg = MqttMessage(bmResultTopic, ByteString(result.asJson.toString()))
          logger.info(s"Publishing to topic: $bmResultTopic, message: $mqttMsg")
          mqttMsg
        }

        val zipMapped = zip.out.map {
          case (a, b) =>
            def singleRun(arg: S): (T, Float) = {
              val startTime = System.currentTimeMillis
              logger.debug(s"singleRun. Start time: $startTime")
              logger.debug(s"inside single run: running alg")
              val result:T = algInterpreted(arg)
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

            def computeResults(): Seq[(Equals with Serializable, Float, Float)] = {
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

                val filteredResults = resultsFutures.filter { case (_: T, a: Float, _) => a != 0.0 }
                val validatedResults = filteredResults.map {
                  case (path: List[List[Float]], b, c) =>
                    if (validatePath(path.asInstanceOf[List[List[Float]]])) {
                      (path, b, c)
                    } else {
                      val pythonString = writePyPathData(path.asInstanceOf[List[List[Float]]])
                      logger.warn(s"Path validation found invalid path: $pythonString")
                      (List.empty[List[Float]], 0.0f, c)
                    }
                  case (a, b: Float, c: Float) => (a, b, c)
                }
                validatedResults
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
        ClosedShape
    }
  })
}

object BenchmarkClientSbmp extends LazyLogging {
  def apply[S, T](alg: SbmpAlg, algInterpreted: S => T): BenchmarkClientSbmp[S, T] = {
    logger.debug(s"Building BenchmarkClient instance")
    new BenchmarkClientSbmp(alg, algInterpreted)
  }
}





