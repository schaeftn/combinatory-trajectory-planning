package org.combinators.ctp.repositories.toplevel

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
import akka.stream.scaladsl.{ GraphDSL, RunnableGraph, Sink, Source, Zip}
import com.typesafe.scalalogging.LazyLogging
import org.combinators.cls.interpreter.combinator
import org.combinators.cls.types.Constructor
import scala.concurrent.Future
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.toplevel._
import org.combinators.cls.types.syntax._


trait AkkaMqttTopLevelCmp extends LazyLogging with AkkaImplicits with AkkaMqttComponents {
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
              sceneSource: Source[Scene, Future[Done]],
              taskSource: Source[MpTaskStartGoal, Future[Done]],
              composedFunction: (Scene, MpTaskStartGoal) => PolySceneSegmentationRoadmapPath,
              sceneSink: Sink[MqttMessage, Future[Done]]): Unit = {
      def toMqttMsg(s: PolySceneSegmentationRoadmapPath) = {
        val topic = p.getProperty("org.combinators.ctp.ctpSceneGraphPathFromScala")
        MqttMessage(topic, ByteString(s.asJson.toString()))
      }

      val streamGraph: RunnableGraph[NotUsed] = RunnableGraph.fromGraph(GraphDSL.create() { implicit b =>
        val zip = b.add(Zip[Scene, MpTaskStartGoal])
        sceneSource ~> zip.in0
        taskSource ~> zip.in1
        zip.out.map {
          case (a, b) =>
            logger.info("running composedfct")
            val result = composedFunction(a, b)
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
        rmc_cellGraph_var :&: sd_cell_type_var :&: sd_poly_scene_cell_segmentation_var :&: dimensionality_var =>:
      p_mqttAkkaSink_type :&: cmp_scene_graph_path :&: dimensionality_var =>:
      p_mqttAkkaComposition_type :&: cmp_scene_graph_path :&: cmp_graph_algorithm_var :&: rmc_connectorNodes_var :&: rmc_centroidFct_var :&:
      rmc_cellGraph_var :&: sd_cell_type_var :&: sd_poly_scene_cell_segmentation_var :&: dimensionality_var
  }







}
