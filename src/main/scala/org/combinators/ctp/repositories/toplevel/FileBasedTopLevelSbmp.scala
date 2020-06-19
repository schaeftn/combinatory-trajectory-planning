package org.combinators.ctp.repositories.toplevel

import java.util.{Properties, UUID}

import akka.stream.ClosedShape
import akka.stream.alpakka.mqtt.MqttMessage
import akka.stream.scaladsl.GraphDSL.Implicits._
import akka.stream.scaladsl.{GraphDSL, RunnableGraph, Sink, Source, Zip}
import akka.util.ByteString
import akka.{Done, NotUsed}
import com.typesafe.scalalogging.LazyLogging
import io.circe.syntax._
import org.combinators.cls.interpreter.combinator
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories._

import scala.io.StdIn._
import scala.concurrent.Future
import java.io.File

trait FileBasedTopLevelSbmp extends LazyLogging with AkkaImplicits with AkkaMqttComponents {

  @combinator object FileBasedTopLevelSbmp {
    def apply(composedFunction: ProblemDefinitionFiles => List[List[Float]],
              sceneSink: Sink[MqttMessage, Future[Done]]): Unit = {
      logger.info(s"FileBasedTopLevelSbmp: SampleBasedAkka Start")

      def toMqttMsg(s: List[List[Float]]) = {
        val topic = mqttProperties.getProperty("org.combinators.ctp.ctpPathfromScala")
        MqttMessage(topic, ByteString(s.asJson.toString()))
      }

      val streamGraph: Option[ProblemDefinitionFiles] => RunnableGraph[NotUsed] = pDef =>
        RunnableGraph.fromGraph(GraphDSL.create() { implicit b =>
          Source.single(pDef).filter {
            case Some(_) => true
            case _ => false
          }.map(i => i.get).map { pDefinition => {
            logger.info("FileBasedTopLevelSbmp: Running composed function")
            val path = composedFunction(pDefinition)
            println(s"FileBasedTopLevelSbmp: Found path: $path")
            toMqttMsg(path)
          }
          } ~> sceneSink
          ClosedShape
        })

      val probDir = problemsProperties.getProperty("org.combinators.ctp.problemFolder")

      val d = new File(probDir)
      val cfgFiles: IndexedSeq[String] = if (d.exists && d.isDirectory) {
        d.listFiles.filter(_.isFile).map(_.getName).filter(_.endsWith("cfg")).toIndexedSeq
      } else {
        IndexedSeq.empty[String]
      }

      logger.info(s"Loading Files...")

      println(cfgFiles.zipWithIndex)

      @scala.annotation.tailrec
      def getUserInput: Option[String] = {
        println("Enter your choice (q to quit)")
        val a = readLine()
        if (a.map(_.isDigit).reduce(_ && _) && cfgFiles.indices.contains(a.toInt)) {
          Some(cfgFiles(a.toInt))
        } else {
          if (a == "q")
            None
          else
            getUserInput
        }
      }

      @scala.annotation.tailrec
      def runInput: Unit =
        getUserInput match {
          case Some(s) => streamGraph(ProblemDefinitionFiles(s)).run()
            runInput
          case None => ()
        }

      runInput
    }

    val semanticType =
      sbmp_planning_algorithm :&: sbmp_planner_var :&: sbmp_sampler_var :&:
        sbmp_state_validator_var :&: sbmp_motion_validator_var :&: sbmp_optimization_objective_var :&:
        sbmp_cost_var =>:
        p_mqttAkkaSink_type :&: cmp_path_only :&: dimensionality_var =>:
        p_fileToAkka_type :&: dimensionality_var :&: cmp_path_only :&:
          sbmp_planner_var :&: sbmp_sampler_var :&: sbmp_state_validator_var :&: sbmp_motion_validator_var :&:
          sbmp_optimization_objective_var :&: sbmp_cost_var
  }













}
