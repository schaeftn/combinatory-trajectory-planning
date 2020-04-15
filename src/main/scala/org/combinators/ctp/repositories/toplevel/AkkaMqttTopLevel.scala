package org.combinators.ctp.repositories.toplevel

import java.util.Properties
import akka.stream.alpakka.mqtt.MqttMessage
import akka.util.ByteString
import io.circe.parser.decode
import io.circe.generic.auto._
import io.circe.syntax._
import org.combinators.ctp.repositories.mptasks.MpTaskStartGoal
import org.combinators.ctp.repositories.scene._
import akka.stream.scaladsl.GraphDSL.Implicits._
import akka.{Done, NotUsed}
import akka.stream.{ ClosedShape}
import akka.stream.scaladsl.{ GraphDSL, RunnableGraph, Sink, Source, Zip}
import com.typesafe.scalalogging.LazyLogging
import org.combinators.cls.interpreter.combinator
import org.combinators.cls.types.Constructor
import scala.concurrent.Future
import org.combinators.ctp.repositories._
import org.combinators.cls.types.syntax._


trait AkkaMqttTopLevel extends LazyLogging with AkkaImplicits with AkkaMqttComponents {
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
/*
  @combinator object AkkaGraphSceneSegTri2D {
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

      logger.info(s"Scene segmentation via triangulation, shortest path")
      scala.io.StdIn.readLine()
      logger.info(s"Disconnecting from MqttClient")
    }

    val semanticType = p_unityConnectionProperties_type =>:
      p_mqttAkkaSource_type :&: sd_unity_scene_type :&: dimensionality_two_d_t =>:
      p_mqttAkkaSource_type :&: mpt_start_goal_position_type :&: dimensionality_two_d_t =>:
      triangulation_path_prog_type :&:cmp_scene_triangulation_parameters_var :&: mpt_start_goal_position_type :&: cmp_graph_algorithm_var=>:
      p_mqttAkkaSink_type :&: cmp_scene_graph_path :&: dimensionality_two_d_t =>:
      p_mqttAkkaComposition_type :&: dimensionality_two_d_t :&: cmp_scene_graph_path :&: cmp_scene_triangulation_parameters_var :&: mpt_start_goal_position_type :&: cmp_graph_algorithm_var
  }

  @combinator object AkkaGraphSceneSegTri2DTsp {
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

      logger.info(s"Scene segmentation via triangulation, tsp")
      scala.io.StdIn.readLine()
      logger.info(s"Disconnecting from MqttClient")
    }

    val semanticType = p_unityConnectionProperties_type =>:
      p_mqttAkkaSource_type :&: sd_unity_scene_type :&: dimensionality_two_d_t =>:
      p_mqttAkkaSource_type :&: mpt_start_goal_position_type :&: dimensionality_two_d_t =>:
      triangulation_path_prog_type :&: cmp_scene_triangulation_parameters_var :&: Constructor("graphTsp") =>:
      p_mqttAkkaSink_type :&: cmp_scene_graph_path :&: dimensionality_two_d_t =>:
      p_mqttAkkaComposition_type :&: dimensionality_two_d_t :&: cmp_scene_graph_path :&: cmp_scene_triangulation_parameters_var :&: Constructor("graphTsp")
  }*/
/*
  @combinator object AkkaGraphSceneSegTri2DMst {
    def apply(p:Properties,
              sceneSource: Source[Scene, Future[Done]],
              composedFunction: Scene => PolySceneSegmentationRoadmapPath,
              sceneSink: Sink[MqttMessage, Future[Done]]): Unit = {
      def toMqttMsg(s: PolySceneSegmentationRoadmapPath) = {
        val topic = p.getProperty("org.combinators.ctp.ctpSceneGraphPathFromScala")
        MqttMessage(topic, ByteString(s.asJson.toString()))
      }

      sceneSource.map(i => toMqttMsg(composedFunction(i))).runWith(sceneSink)

      logger.info(s"Listening: AkkaGraphSceneSeg2D Mst")
      scala.io.StdIn.readLine()
      logger.info(s"Disconnecting from MqttClient")
    }

    val semanticType = p_unityConnectionProperties_type =>:
      p_mqttAkkaSource_type :&: sd_unity_scene_type :&: dimensionality_two_d_t =>:
      triangulation_path_prog_type :&: cmp_scene_triangulation_parameters_var :&: cmp_graph_mst_type =>:
      p_mqttAkkaSink_type :&: cmp_scene_graph_path :&: dimensionality_two_d_t =>:
      p_mqttAkkaComposition_type :&: dimensionality_two_d_t :&:
        cmp_scene_graph_path :&: cmp_scene_triangulation_parameters_var :&: cmp_graph_mst_type
  }*/

/* TODO uncomment
  @combinator object AkkaGraphSceneSegTet {
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

      logger.info(s"Listening: Tet")
      scala.io.StdIn.readLine()
      logger.info(s"Disconnecting from MqttClient")
    }

    val semanticType = p_unityConnectionProperties_type =>:
      p_mqttAkkaSource_type :&: sd_unity_scene_type :&: dimensionality_three_d_t =>:
      p_mqttAkkaSource_type :&: mpt_start_goal_position_type :&: dimensionality_three_d_t =>:
      cmp_sd_tetrahedra_type :&: cmp_graph_algorithm_var=>:
      p_mqttAkkaSink_type :&: cmp_scene_graph_path :&: dimensionality_three_d_t =>:
      p_mqttAkkaComposition_type :&: dimensionality_three_d_t :&: cmp_scene_graph_path :&: cmp_graph_algorithm_var
  }

  //TODO Type params
  @combinator object SampleBasedMpAkka {
    def apply(p:Properties,
              sceneSource: Source[SceneSRT, Future[Done]],
              taskSource: Source[MpTaskStartGoal, Future[Done]],
              composedFunction: (SceneSRT, MpTaskStartGoal) => List[List[Float]],
              sceneSink: Sink[MqttMessage, Future[Done]]): Unit = {
      logger.info(s"SampleBasedAkka Start")

      def toMqttMsg(s: List[List[Float]]) = {
        val topic = p.getProperty("org.combinators.ctp.ctpPathfromScala")
        MqttMessage(topic, ByteString(s.asJson.toString()))
      }


      val streamGraph: RunnableGraph[NotUsed] = RunnableGraph.fromGraph(GraphDSL.create() { implicit b =>
        val zip = b.add(Zip[SceneSRT, MpTaskStartGoal])
        sceneSource ~> zip.in0
        taskSource ~> zip.in1
        zip.out.map {
          case (a, b) =>
            logger.info("running composedfct")
            val result = composedFunction(a, b)
            logger.info("post composedfct")
            logger.info(s"Result: $result")
            toMqttMsg(result)
        } ~> sceneSink
        ClosedShape
      })
      streamGraph.run()

      logger.info(s"Sample-based planning. Listening: SceneSRT and TaskStartGoal")
      scala.io.StdIn.readLine()
      logger.info(s"Disconnecting from MqttClient")
    }

    val semanticType = p_unityConnectionProperties_type =>:
      p_mqttAkkaSource_type :&: sd_unity_scene_srt_type :&: dimensionality_var =>:
      p_mqttAkkaSource_type :&: mpt_start_goal_position_type :&: dimensionality_var =>:
      sbmp_planning_algorithm :&: sbmp_planner_var :&: sbmp_sampler_var :&:
        sbmp_state_validator_var :&: sbmp_motion_validator_var :&: sbmp_optimization_objective_var :&:
        sbmp_cost_var =>:
      p_mqttAkkaSink_type :&: cmp_path_only :&: dimensionality_var =>:
      p_mqttAkkaComposition_type :&: dimensionality_var :&: cmp_path_only :&:
        sbmp_planner_var :&: sbmp_sampler_var :&: sbmp_state_validator_var :&: sbmp_motion_validator_var :&:
        sbmp_optimization_objective_var :&: sbmp_cost_var
  }

  //TODO adapt to new cmp
  @combinator object SampleBasedMpAkkaRefinement {
    def apply(p:Properties,
              sceneSource: Source[Scene, Future[Done]],
              taskSource: Source[MpTaskStartGoal, Future[Done]],
              composedFunction: (Scene, MpTaskStartGoal) => PolySceneSegmentationRoadmapPath,
              pathSmoothingFct: (PolySceneSegmentationRoadmapPath, MpTaskStartGoal) => PolySceneSegmentationRoadmapPath,
              sceneSink: Sink[MqttMessage, Future[Done]]): Unit = {
      logger.info(s"SampleBasedAkka Start")

      def toMqttMsg(s: PolySceneSegmentationRoadmapPath) = {
        val topic = p.getProperty("org.combinators.ctp.ctpPathfromScala")
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
            val smoothed = pathSmoothingFct(result, b)
            logger.info("post composedfct")
            logger.info(s"Result: $result")
            toMqttMsg(smoothed)
        } ~> sceneSink
        ClosedShape
      })
      streamGraph.run()

      logger.info(s"Sample-based planning. Listening: SceneSRT and TaskStartGoal")
      scala.io.StdIn.readLine()
      logger.info(s"Disconnecting from MqttClient")
    }

    val semanticType = p_unityConnectionProperties_type =>:
      p_mqttAkkaSource_type :&: sd_unity_scene_type :&: dimensionality_var =>:
      p_mqttAkkaSource_type :&: mpt_start_goal_position_type :&: dimensionality_var =>:
      triangulation_path_prog_type  :&: mpt_start_goal_position_type :&:
        cmp_graph_algorithm_var =>:
      sbmp_planning_algorithm :&: sbmp_planner_var :&: sbmp_sampler_var :&:
        sbmp_state_validator_var :&: sbmp_motion_validator_var :&: sbmp_optimization_objective_var :&:
        sbmp_cost_var =>: //TODO dimensionality
      p_mqttAkkaSink_type :&: cmp_path_only :&: dimensionality_var =>:
      p_mqttAkkaComposition_type :&: dimensionality_var :&: cmp_path_only :&: Constructor("sampleAkka") :&:
        sbmp_planner_var :&: sbmp_sampler_var :&: sbmp_state_validator_var :&: sbmp_motion_validator_var :&:
        sbmp_optimization_objective_var :&: sbmp_cost_var :&: Constructor("pathsmoothing")
  }*/


/*
  @combinator object SampleBasedMpAkkaRefinementTriangles {
    def apply(p:Properties,
              sceneSource: Source[Scene, Future[Done]],
              taskSource: Source[MpTaskStartGoal, Future[Done]],
              composedFunction: (Scene, MpTaskStartGoal) => PolySceneSegmentationRoadmapPath,
              pathSmoothingFct: (PolySceneSegmentationRoadmapPath, MpTaskStartGoal) => PolySceneSegmentationRoadmapPath,
              sceneSink: Sink[MqttMessage, Future[Done]]): Unit = {
      logger.info(s"SampleBasedAkka Start")

      def toMqttMsg(s: PolySceneSegmentationRoadmapPath) = {
        val topic = p.getProperty("org.combinators.ctp.ctpPathfromScala3d")
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
            val smoothed = pathSmoothingFct(result, b)
            logger.info("post composedfct")
            logger.info(s"Result: $result")
            toMqttMsg(smoothed)
        } ~> sceneSink
        ClosedShape
      })
      streamGraph.run()

      logger.info(s"Sample-based planning. Listening: SceneSRT and TaskStartGoal")
      scala.io.StdIn.readLine()
      logger.info(s"Disconnecting from MqttClient")
    }

    val semanticType = p_unityConnectionProperties_type =>:
      p_mqttAkkaSource_type :&: sd_unity_scene_type :&: dimensionality_var =>:
      p_mqttAkkaSource_type :&: mpt_start_goal_position_type :&: dimensionality_var =>:
      triangulation_path_prog_type :&: cmp_scene_triangulation_parameters_var :&: mpt_start_goal_position_type :&:
        cmp_graph_algorithm_var =>:
      sbmp_planning_algorithm :&: sbmp_planner_var :&: sbmp_sampler_var :&:
        sbmp_state_validator_var :&: sbmp_motion_validator_var :&: sbmp_optimization_objective_var :&:
        sbmp_cost_var =>: //TODO dimensionality
      p_mqttAkkaSink_type :&: cmp_path_only :&: dimensionality_var =>:
      p_mqttAkkaComposition_type :&: dimensionality_var :&: cmp_path_only :&: Constructor("sampleAkka") :&:
        sbmp_planner_var :&: sbmp_sampler_var :&: sbmp_state_validator_var :&: sbmp_motion_validator_var :&:
        sbmp_optimization_objective_var :&: sbmp_cost_var :&: Constructor("pathsmoothing")
  }*/
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
//      cmp_graph_dijkstra_type =>:
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
      cmp_graph_dijkstra_type =>:
      p_mqttAkkaSink_type :&: cmp_scene_graph :&: dimensionality_two_d_t =>:
      p_unitySceneAgent_type :&: cmp_vertical_cell_decomposition_type :&:
        dimensionality_two_d_t :&: p_unityResult_type :&: cmp_scene_graph_path
  }*/



}
