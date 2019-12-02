package org.combinators.ctp.repositories.toplevel

import java.time.Duration
import java.util.Properties

import akka.stream.alpakka.mqtt.MqttMessage
import akka.util.ByteString
import io.circe.parser.decode
import io.circe.generic.auto._
import io.circe.syntax._
import org.combinators.ctp.repositories.mptasks.MpTaskStartGoal
import org.combinators.ctp.repositories.scene.{PolySceneSegmentationGraph, _}
import akka.stream.scaladsl.GraphDSL.Implicits._
import akka.{Done, NotUsed}
import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, ClosedShape, Materializer}
import akka.stream.alpakka.mqtt.{MqttConnectionSettings, MqttQoS, MqttSubscriptions}
import akka.stream.alpakka.mqtt.scaladsl.{MqttSink, MqttSource}
import akka.stream.scaladsl.{Flow, GraphDSL, RunnableGraph, Sink, Source, Zip}
import com.typesafe.scalalogging.LazyLogging
import org.combinators.cls.interpreter.combinator
import org.combinators.cls.types.Constructor
import org.eclipse.paho.client.mqttv3.persist.MemoryPersistence

import scala.concurrent.Future
import scalax.collection.io.json.descriptor.GenEdgeDescriptor

import scala.reflect.io.Path
import org.combinators.ctp.repositories._
import org.combinators.cls.types.syntax._
import scalax.collection.edge.WUnDiEdge


trait AkkaMqttTopLevel extends LazyLogging with AkkaImplicits{
 @combinator object AkkaFlowSceneSeg2D {
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
  }


  @combinator object AkkaGraphSceneSeg2D {
    def apply(p:Properties,
              sceneSource: Source[Scene, Future[Done]],
              taskSource: Source[MpTaskStartGoal, Future[Done]],
              composedFunction: (Scene, MpTaskStartGoal) => PolySceneSegmentationGraphPath,
              sceneSink: Sink[MqttMessage, Future[Done]]): Unit = {
      def toMqttMsg(s: PolySceneSegmentationGraphPath) = {
        val topic = p.getProperty("org.combinators.ctp.ctpSceneGraphPathFromScala2d")
        MqttMessage(topic, ByteString(s.asJson.toString()))
      }

      val streamGraph: RunnableGraph[NotUsed] = RunnableGraph.fromGraph(GraphDSL.create() { implicit b =>
        val zip = b.add(Zip[Scene, MpTaskStartGoal])
        sceneSource ~> zip.in0
        taskSource ~> zip.in1
        zip.out.map {
          case ((a, b)) =>
            logger.info("running composedfct")
            val result = composedFunction(a, b)
            logger.info("post composedfct")
            println(toMqttMsg(result))
            toMqttMsg(result)
        } ~> sceneSink
        ClosedShape
      })
      streamGraph.run()

      logger.info(s"Listening: AkkaGraphSceneSeg2D asd")
      scala.io.StdIn.readLine()
      logger.info(s"Disconnecting from MqttClient")
    }

    val semanticType = p_unityConnectionProperties_type =>:
      p_mqttAkkaSource_type :&: sd_unity_scene_type :&: dimensionality_two_d_t =>:
      p_mqttAkkaSource_type :&: mpt_start_goal_position_type :&: dimensionality_two_d_t =>:
      (sd_unity_scene_type :&: mpt_start_goal_position_type :&: dimensionality_two_d_t =>:
        cmp_scene_graph_path :&: dimensionality_two_d_t) =>:
      p_mqttAkkaSink_type :&: cmp_scene_graph_path :&: dimensionality_two_d_t =>:
      p_unitySceneAgent_type :&: cmp_vertical_cell_decomposition_type :&:
        dimensionality_two_d_t :&: p_unityResult_type :&: cmp_scene_graph_path
  }

  @combinator object AkkaGraphSceneSegTri2D {
    def apply(p:Properties,
              sceneSource: Source[Scene, Future[Done]],
              taskSource: Source[MpTaskStartGoal, Future[Done]],
              composedFunction: (Scene, MpTaskStartGoal) => TriangleSegPath,
              sceneSink: Sink[MqttMessage, Future[Done]]): Unit = {
      def toMqttMsg(s: TriangleSegPath) = {
        val topic = p.getProperty("org.combinators.ctp.ctpSceneGraphPathFromScala2d")
        MqttMessage(topic, ByteString(s.asJson.toString()))
      }

      val streamGraph: RunnableGraph[NotUsed] = RunnableGraph.fromGraph(GraphDSL.create() { implicit b =>
        val zip = b.add(Zip[Scene, MpTaskStartGoal])
        sceneSource ~> zip.in0
        taskSource ~> zip.in1
        zip.out.map {
          case ((a, b)) =>
            logger.info("running composedfct")
            val result = composedFunction(a, b)
            logger.info("post composedfct")
            println(toMqttMsg(result))
            toMqttMsg(result)
        } ~> sceneSink
        ClosedShape
      })
      streamGraph.run()

      logger.info(s"Listening: AkkaGraphSceneSeg2D asd")
      scala.io.StdIn.readLine()
      logger.info(s"Disconnecting from MqttClient")
    }

    val semanticType = p_unityConnectionProperties_type =>:
      p_mqttAkkaSource_type :&: sd_unity_scene_type :&: dimensionality_two_d_t =>:
      p_mqttAkkaSource_type :&: mpt_start_goal_position_type :&: dimensionality_two_d_t =>:
      Constructor("tgp") =>:
      p_mqttAkkaSink_type :&: cmp_scene_graph_path :&: dimensionality_two_d_t =>:
      p_unitySceneAgent_type :&: cmp_vertical_cell_decomposition_type :&:
        dimensionality_two_d_t :&: p_unityResult_type :&: cmp_scene_graph_path :&: sd_seg_triangles
  }

// @combinator object AkkaFlowSceneSeg2DPath2 {
//    def apply(p: Properties,
//              mc: MqttConnectionSettings,
//              composedFunction: Scene => PolySceneSegmentationGraph,
//              graphTraversal: (PolySceneSegmentationGraph, MpTaskStartGoal) =>
//                (Seq[List[Float]], Seq[WUnDiEdge[List[Float]]], Float)
//              ): Unit = {
//      def toMqttMsg(s: PolySceneSegmentationGraphPath) = {
//        val topic = p.getProperty("org.combinators.ctp.ctpSceneGraphPathFromScala2d")
//        MqttMessage(topic, ByteString(s.asJson.toString()))
//        //
//      }
//
//      val topic = "ctpSceneFromUnity2D"
//      val topic2 = "ctpTaskSpFromUnity2D"
//
//      val sceneSink = MqttFlow.atMostOnce(mc,MqttSubscriptions(topic, MqttQoS.AtLeastOnce).
//        addSubscription(topic2,MqttQoS.atLeastOnce), bufferSize = 8, MqttQoS.atMostOnce).map{
//        _ => "foo"
//      }
//
//      logger.info(s"Akka Listener: AkkaFlowSceneSeg2DPath")
//      scala.io.StdIn.readLine()
//      logger.info(s"Disconnecting from MqttClient")
//    }
//
//    val semanticType = p_unityConnectionProperties_type =>:
//      p_mqttAkkaConnxSettings_type =>:
//      (sd_unity_scene_type :&: dimensionality_two_d_t =>: cmp_scene_graph :&: dimensionality_two_d_t) =>:
//      Constructor("graphTraversalFct") =>:
//      p_unitySceneAgent_type :&: cmp_vertical_cell_decomposition_type :&:
//        dimensionality_two_d_t :&: p_unityResult_type :&: cmp_scene_graph_pathf
//  }



/*  @combinator object UnityMqttAkkaSourceTask2DPose{
    def apply(p: Properties): Source[MpTaskStartGoalPose, Future[Done]] = {
      val broker = p.getProperty("org.combinators.ctp.broker")
      val topic = p.getProperty("org.combinators.ctp.ctpTaskSpPoseFromUnity2D")

      val connectionSettings = MqttConnectionSettings(broker, "cls/TaskPose2DListener",
        new MemoryPersistence).withAutomaticReconnect(true)

      MqttSource.atMostOnce(
        connectionSettings,
        MqttSubscriptions(topic, MqttQoS.AtLeastOnce),
        bufferSize = 8).map(i => decode[MpTaskStartGoalPose](i.payload.utf8String).right.get)
    }

    val semanticType = p_unityConnectionProperties_type =>:
      p_mqttAkkaSource_type :&: mpt_start_goal_pose_type :&: dimensionality_two_d_t
  }
  */

  /*@combinator object AkkaFlowSceneSeg2DPathOld {
    def apply(p: Properties,
              sceneSource: Source[Scene, Future[Done]],
              taskSource: Source[MpTaskStartGoal, Future[Done]],
              composedFunction: Scene => PolySceneSegmentationGraph,
              graphTraversal: (PolySceneSegmentationGraph, MpTaskStartGoal) =>
                (Seq[List[Float]], Seq[WUnDiEdge[List[Float]]], Float),
              sceneSink: Sink[MqttAkkaMessage, Future[Done]]): Unit = {
      def toMqttMsg(s: PolySceneSegmentationGraphTraversal) = {
        val topic = p.getProperty("org.combinators.ctp.ctpSceneGraphPathFromScala2d")
        MqttAkkaMessage(topic, ByteString(s.asJson.toString()))
        //
      }

      (sceneSource zip taskSource).map {
        case ((s, t)) =>
          val seg = composedFunction(s)
          val traversal = graphTraversal(seg, t)

          val segGTrav = PolySceneSegmentationGraphTraversal(seg.vertices, seg.obstacles,
            seg.boundaries, seg.freeCells, seg.graph, traversal)
          toMqttMsg(segGTrav)
      }.runWith(sceneSink)
      logger.info(s"Akka Listener: AkkaFlowSceneSeg2DPath")
      scala.io.StdIn.readLine()
      logger.info(s"Disconnecting from MqttClient")
    }

    val semanticType = p_unityConnectionProperties_type =>:
      p_mqttAkkaSource_type :&: sd_unity_scene_type :&: dimensionality_two_d_t =>:
      p_mqttAkkaSource_type :&: mpt_start_goal_position_type :&: dimensionality_two_d_t =>:
      (sd_unity_scene_type :&: dimensionality_two_d_t =>: cmp_scene_graph :&: dimensionality_two_d_t) =>:
      Constructor("graphTraversalFct") =>:
      p_mqttAkkaSink_type :&: cmp_scene_graph :&: dimensionality_two_d_t =>:
      p_unitySceneAgent_type :&: cmp_vertical_cell_decomposition_type :&:
        dimensionality_two_d_t :&: p_unityResult_type :&: cmp_scene_graph_path
  }*/



}
