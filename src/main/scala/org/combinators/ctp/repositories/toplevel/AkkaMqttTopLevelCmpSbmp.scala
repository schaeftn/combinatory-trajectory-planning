package org.combinators.ctp.repositories.toplevel

import java.io.File
import java.util.Properties

import akka.stream.ClosedShape
import akka.stream.alpakka.mqtt.MqttMessage
import akka.stream.scaladsl.GraphDSL.Implicits._
import akka.stream.scaladsl.{GraphDSL, RunnableGraph, Sink, Source, Zip}
import akka.util.ByteString
import akka.{Done, NotUsed}
import com.typesafe.scalalogging.LazyLogging
import io.circe.generic.auto._
import io.circe.syntax._
import org.combinators.cls.interpreter.combinator
import org.combinators.cls.types.Constructor
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.toplevel._
import org.combinators.ctp.repositories.scene._

import scala.concurrent.Future
import scala.io.StdIn.readLine


trait AkkaMqttTopLevelCmpSbmp extends LazyLogging with AkkaImplicits with AkkaMqttComponents {
  @combinator object SampleBasedMpAkkaRefinement {
    def apply(
               cmpFileBased: ProblemDefinitionFiles => List[List[Float]],
               composedFunction: ((ProblemDefinitionFiles, List[List[Float]])) => List[List[Float]],
               sceneSink: Sink[MqttMessage, Future[Done]]): Unit = {
      logger.info(s"Refinement start")

      def toMqttMsg(s: List[List[Float]]) = {
        val topic = mqttProperties.getProperty("org.combinators.ctp.ctpPathfromScala")
        MqttMessage(topic, ByteString(s.asJson.toString()))
      }

      val streamGraph: ProblemDefinitionFiles => RunnableGraph[NotUsed] = pDef =>
        RunnableGraph.fromGraph(GraphDSL.create() { implicit b =>
          logger.info("running composedfct")
          val cmpPath = cmpFileBased(pDef)
          val path = composedFunction((pDef, cmpPath))
          println(s"found path: $path")
          Source.single(toMqttMsg(path)) ~> sceneSink
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
      cmp_algorithm_type :&: cmp_graph_algorithm_var :&: rmc_connectorNodes_var :&: rmc_centroidFct_var :&:
        rmc_cellGraph_var :&: sd_cell_type_var :&: sd_poly_scene_cell_segmentation_var :&: dimensionality_var :&:
        rmc_cellNodeAddFct_var :&: rmc_startGoalFct_var :&: rmc_usingCentroids_var =>:
        sbmp_planning_algorithm :&: sbmp_planner_var :&: sbmp_sampler_var :&:
          sbmp_state_validator_var :&: sbmp_motion_validator_var :&: sbmp_optimization_objective_var :&:
          sbmp_cost_var =>:
        p_mqttAkkaSink_type :&: cmp_path_only :&: dimensionality_var =>:
        p_fileToAkka_type :&: dimensionality_var :&: cmp_path_only :&:
          sbmp_planner_var :&: sbmp_sampler_var :&: sbmp_state_validator_var :&: sbmp_motion_validator_var :&:
          sbmp_optimization_objective_var :&: sbmp_cost_var :&: cmp_graph_algorithm_var :&: rmc_connectorNodes_var :&:
          rmc_centroidFct_var :&: rmc_cellGraph_var :&: sd_cell_type_var :&: sd_poly_scene_cell_segmentation_var  :&:
          rmc_cellNodeAddFct_var :&: rmc_startGoalFct_var :&: rmc_usingCentroids_var
  }

@combinator object SampleBasedMpAkkaRefinementStates {
    def apply(
               cmpFileBased: ProblemDefinitionFiles => List[List[Float]],
               composedFunction: ((ProblemDefinitionFiles, List[List[Float]])) => (List[List[Float]],List[List[Float]]),
               sceneSink: Sink[MqttMessage, Future[Done]]): Unit = {
      logger.info(s"Refinement start")

      def toMqttMsg(path: List[List[Float]], states: List[List[Float]]):List[MqttMessage] = {
        val pathTopic = mqttProperties.getProperty("org.combinators.ctp.ctpPathfromScala")
        val stateTopic = mqttProperties.getProperty("org.combinators.ctp.ctpStatesfromScala")

        List(MqttMessage(pathTopic, ByteString(path.asJson.toString())), MqttMessage(stateTopic, ByteString(states.asJson.toString())))
      }

      val streamGraph: ProblemDefinitionFiles => RunnableGraph[NotUsed] = pDef =>
        RunnableGraph.fromGraph(GraphDSL.create() { implicit b =>
          logger.info("running composedfct")
          val cmpPath = cmpFileBased(pDef)
          val path = composedFunction((pDef, cmpPath))
          println(s"found path: $path")
          Source(toMqttMsg(path._1, path._2)) ~> sceneSink
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
      cmp_algorithm_type :&: cmp_graph_algorithm_var :&: rmc_connectorNodes_var :&: rmc_centroidFct_var :&:
        rmc_cellGraph_var :&: sd_cell_type_var :&: sd_poly_scene_cell_segmentation_var :&: dimensionality_var :&:
        rmc_cellNodeAddFct_var :&: rmc_startGoalFct_var :&: rmc_usingCentroids_var =>:
        sbmp_planning_algorithm :&: sbmp_planner_var :&: sbmp_sampler_var :&:
          sbmp_state_validator_var :&: sbmp_motion_validator_var :&: sbmp_optimization_objective_var :&:
          sbmp_cost_var =>:
        p_mqttAkkaSink_type :&: cmp_path_only :&: dimensionality_var =>:
        p_fileToAkka_type :&: dimensionality_var :&: cmp_path_only :&:
          sbmp_planner_var :&: sbmp_sampler_var :&: sbmp_state_validator_var :&: sbmp_motion_validator_var :&:
          sbmp_optimization_objective_var :&: sbmp_cost_var :&: cmp_graph_algorithm_var :&: rmc_connectorNodes_var :&:
          rmc_centroidFct_var :&: rmc_cellGraph_var :&: sd_cell_type_var :&: sd_poly_scene_cell_segmentation_var  :&:
          rmc_cellNodeAddFct_var :&: rmc_startGoalFct_var :&: rmc_usingCentroids_var :&: Constructor("ShowStates")
  }

}
