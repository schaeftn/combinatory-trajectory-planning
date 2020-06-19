package org.combinators.ctp.repositories.toplevel

import java.io.File
import java.util.Properties

import akka.stream.alpakka.mqtt.MqttMessage
import akka.util.ByteString
import io.circe.parser.decode
import io.circe.generic.auto._
import io.circe.syntax._
import org.combinators.ctp.repositories.scene._
import akka.stream.scaladsl.GraphDSL.Implicits._
import akka.{Done, NotUsed}
import akka.stream.ClosedShape
import akka.stream.scaladsl.{GraphDSL, RunnableGraph, Sink, Source, Zip}
import com.typesafe.scalalogging.LazyLogging
import org.combinators.cls.interpreter.combinator
import org.combinators.cls.types.Constructor

import scala.concurrent.Future
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.toplevel._
import org.combinators.cls.types.syntax._
import scalax.collection.Graph
import scalax.collection.edge.WUnDiEdge

import scala.io.StdIn.readLine


trait AkkaMqttTopLevelCmp extends LazyLogging with AkkaImplicits with AkkaMqttComponents with SceneUtils {
/* @combinator object AkkaFlowSceneSeg2D {
    def apply(p:Properties, sceneSource: Source[Scene, Future[Done]],
              composedFunction: Scene => PolySceneSegmentationGraph,
              sceneSink: Sink[MqttMessage, Future[Done]]): Unit = {
      def toMqttMsg(s: PolySceneSegmentationGraph) = {
        val topic = p.getProperty("org.combinators.ctp.ctpSceneGraphFromScala2d")
        MqttMessage(topic, ByteString(s.asJson.toString()))
      }

      sceneSource.map(i => toMqttMsg(composedFunction(i))).runWith(sceneSink)
      logger.info(s"Akka Listener: AkkaFlowSceneSeg2D")
      scala.io.StdIn.readLine()
      logger.info(s"Disconnecting from MqttClient")
    }

    val semanticType = p_unityConnectionProperties_type =>:
      p_mqttAkkaSource_type :&: sd_unity_scene_type :&: dimensionality_two_d_t =>:
      (sd_unity_scene_type :&: dimensionality_two_d_t =>: cmp_scene_graph :&: dimensionality_two_d_t) =>:
      p_mqttAkkaSink_type :&: cmp_scene_graph :&: dimensionality_two_d_t =>:
      p_unitySceneAgent_type :&: cmp_vertical_cell_decomposition_type :&:
        dimensionality_two_d_t :&: p_unityResult_type :&: cmp_scene_graph
  }*/


  @combinator object AkkaGraphSceneSeg2D {
    def apply(p:Properties,
              sceneSource: Source[Option[Scene], Future[Done]],
              taskSource: Source[Option[MpTaskStartGoal], Future[Done]],
              composedFunction: (Scene, MpTaskStartGoal) => PolySceneSegmentationRoadmapPath,
              sceneSink: Sink[MqttMessage, Future[Done]]): Unit = {
      def toMqttMsg(s: PolySceneSegmentationRoadmapPath) = {
        val topic = p.getProperty("org.combinators.ctp.ctpSceneGraphPathFromScala")
        MqttMessage(topic, ByteString(s.asJson.toString()))
      }

      val streamGraph: RunnableGraph[NotUsed] = RunnableGraph.fromGraph(GraphDSL.create() { implicit b =>
        val zip = b.add(Zip[Option[Scene], Option[MpTaskStartGoal]])
        sceneSource.filter {
          case Some(_) => true
          case _ => logger.info("Error while decoding Scene. Ignoring message.")
            false
        } ~> zip.in0
        taskSource.filter {
          case Some(_) => true
          case _ => logger.info("Error while decoding task. Ignoring message.")
            false
        } ~> zip.in1
        zip.out.map {
          case (a, b) =>
            logger.info("running composedfct")
            val result = composedFunction(a.get, b.get)
            logger.info("post composedfct")
            println(toMqttMsg(result))
            toMqttMsg(result)
        } ~> sceneSink
        ClosedShape
      })
      streamGraph.run()

      logger.info(s"Shortest path motion planning via vertical cell decomposition.")
      logger.info(s"Listening.")
      scala.io.StdIn.readLine()
      logger.info(s"Disconnecting from MqttClient")
    }

    val semanticType = p_unityConnectionProperties_type =>:
      p_mqttAkkaSource_type :&: sd_unity_scene_type :&: dimensionality_var =>:
      p_mqttAkkaSource_type :&: mpt_start_goal_position_type :&: dimensionality_var =>:
      cmp_algorithm_type :&: cmp_graph_algorithm_var :&: rmc_connectorNodes_var :&: rmc_centroidFct_var :&:
        rmc_cellGraph_var :&: sd_cell_type_var :&: sd_poly_scene_cell_segmentation_var :&: dimensionality_var :&:
        rmc_cellNodeAddFct_var :&: rmc_startGoalFct_var :&: rmc_usingCentroids_var =>:
      p_mqttAkkaSink_type :&: cmp_scene_graph_path :&: dimensionality_var =>:
      p_mqttAkkaComposition_type :&: cmp_scene_graph_path :&: cmp_graph_algorithm_var :&: rmc_connectorNodes_var :&:
        rmc_centroidFct_var :&: rmc_cellGraph_var :&: sd_cell_type_var :&: sd_poly_scene_cell_segmentation_var :&:
        dimensionality_var :&: rmc_cellNodeAddFct_var :&: rmc_startGoalFct_var :&: rmc_usingCentroids_var
  }

  @combinator object AkkaCmpProbFiles {
    def apply(
               problemToRm: ProblemDefinitionFiles => Graph[List[Float], WUnDiEdge],
               findPath: (Graph[List[Float], WUnDiEdge], MpTaskStartGoal) => Seq[List[Float]],
               sceneSink: Sink[MqttMessage, Future[Done]]): Unit = {
      def toMqttMsg(s: PolySceneSegmentationRoadmapPath) = {
        val topic = mqttProperties.getProperty("org.combinators.ctp.ctpSceneGraphPathFromScala")
        MqttMessage(topic, ByteString(s.asJson.toString()))
      }

      logger.info(s"Refinement start")


      val streamGraph: Option[ProblemDefinitionFiles] => RunnableGraph[NotUsed] = pDef =>
        RunnableGraph.fromGraph(GraphDSL.create() { implicit b =>
          Source.single(pDef).filter {
            case Some(_) => true
            case _ => false
          }.map(i => i.get).map { pDefinition => {
            logger.info("running composedfct")
            val cmpPath = problemToRm(pDefinition)
            val path = findPath(cmpPath, readMpStartGoalFromProperties(pDefinition.problemProperties))
            println(s"found path: $path")
            val rmData = PolySceneSegmentationRoadmapPath(List.empty[List[Float]],
              List.empty[List[Int]],
              List.empty[Float],
              List.empty[List[Int]],
              cmpPath,
              path)
            toMqttMsg(rmData)
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
    cmp_sceneSegFct_type :&: cmp_cell_graph_fct :&: sd_poly_scene_cell_segmentation_var :&: dimensionality_var :&:
      rmc_cellNodeAddFct_var :&: rmc_startGoalFct_var :&: rmc_usingCentroids_var :&:
      rmc_centroidFct_var :&: sd_cell_type_var :&: rmc_cellGraph_var :&: rmc_connectorNodes_var =>:
      cmp_graph_algorithm_var =>:
      p_mqttAkkaSink_type :&: cmp_scene_graph_path :&: dimensionality_var =>:
      p_fileToAkka_type :&: cmp_scene_graph_path :&: cmp_graph_algorithm_var :&: rmc_connectorNodes_var :&:
        rmc_centroidFct_var :&: rmc_cellGraph_var :&: sd_cell_type_var :&: sd_poly_scene_cell_segmentation_var :&:
        dimensionality_var :&: rmc_cellNodeAddFct_var :&: rmc_startGoalFct_var :&: rmc_usingCentroids_var
  }
}
