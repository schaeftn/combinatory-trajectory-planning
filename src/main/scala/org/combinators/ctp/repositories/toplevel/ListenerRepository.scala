package org.combinators.ctp.repositories.toplevel

import java.util.Properties

import com.typesafe.scalalogging.LazyLogging
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.parser.decode
import org.combinators.ctp.repositories._
import org.combinators.cls.interpreter.combinator
import org.combinators.cls.types.Constructor
import org.combinators.cls.types.syntax._
//import org.combinators.ctp.repositories.toplevel.ListenerTupleTest.getClass
import org.combinators.ctp.repositories.scene.{CtpSceneConnectionValues, CtpSceneConnectionValues2d, CtpSceneConnectionValues3d, CtpSceneConnectionValuesVcd, CtpSceneListener, CtpSceneListenerVcd, CtpSceneListenerVcd2, CtpSceneUtils2D, PolySceneCellSegmentation, PolySceneCellSegmentationCentroids, PolySceneLineSegmentation, PolygonScene, PythonTemplateUtils, Scene, SegmentationLines2d}
import org.eclipse.paho.client.mqttv3.persist.MemoryPersistence
import org.eclipse.paho.client.mqttv3.{IMqttMessageListener, MqttClient, MqttConnectOptions, MqttMessage => MqttPahoMessage}
import scalax.collection.Graph
import scalax.collection.edge.WUnDiEdge

import scala.concurrent.Future

trait ListenerRepository extends LazyLogging with PythonTemplateUtils{

/*  @combinator object SceneAgentMqtt {
    def apply(c: MqttClient, v: CtpSceneConnectionValues3d): Unit = {
      CtpSceneListener(c, v).subscribe
      println("Combinator SceneAgentMqtt")
      println("Press <Enter> to exit.")
      scala.io.StdIn.readLine()
      println(s"Disconnecting from MqttClient")
      c.disconnect()
      c.close()
    }

    val semanticType = p_mqttClient_type =>: p_unitySceneConnectionValues_three_d_type =>:
      p_unitySceneAgent_type :&: cmp_tetrahedralization_cd_type :&: dimensionality_three_d_t :&: p_unityResult_type
  }*/

  @combinator object SceneAgentMqttVcdToCells {
    def apply(c: MqttClient, v: CtpSceneConnectionValues2d,
              transformToPoly: Scene => PolygonScene, run: PolygonScene => PolySceneLineSegmentation,
              translateResult: PolySceneLineSegmentation => PolySceneCellSegmentation): Unit = {
      def publishResult: String => Unit = { resultData =>
        logger.info("Publishing result to " + v.ctpScenePublishTopic)
        c.publish(v.ctpScenePublishTopic, resultToByteArray(resultData), 2, true)
      }

      def onMessage: IMqttMessageListener = (topic: String, message: MqttPahoMessage) => {
        logger.info("Received Payload: " + new String(message.getPayload))
        val decoded = decode[scene_type_2d_n](new String(message.getPayload))
        logger.info("decoded payload")

        if (decoded.isLeft) {
          logger.info(s"Error: ${decoded.left.get}")
        } else {
          logger.info(s"Starting cell decomposition")
          val r = run(transformToPoly(decoded.right.get))
          logger.info("computed result")
          publishResult(translateResult(r).asJson.toString())
          logger.info("Result published")
        }
      }
      logger.info("Combinator SceneAgentMqttVcd")

      logger.info("Publishing to Topic: " + v.ctpScenePublishTopic)
      logger.info("Subscribung to topic: " + v.ctpSceneSubscribeTopic)
      c.subscribe(v.ctpSceneSubscribeTopic, 2, onMessage)
      logger.info("SceneAgentMqttVcd Cells: Press <Enter> to exit.")
      scala.io.StdIn.readLine()
      logger.info(s"Disconnecting from MqttClient")
      c.disconnect()
      c.close()
    }

    val semanticType = p_mqttCirceClient_type =>:
      p_unitySceneConnectionValues_two_d_type :&: cmp_cd_cells =>:
      (sd_unity_scene_type =>: sd_polygon_scene_type) =>:
      (sd_polygon_scene_type =>: sd_polygon_scene_type :&: sd_scene_segmentation) =>:
      (sd_seg_lines :&: sd_poly_scene_segmentation =>: sd_seg_cells :&: sd_poly_scene_segmentation) =>:
      p_unitySceneAgent_type :&: cmp_vertical_cell_decomposition_type :&: dimensionality_two_d_t :&: p_unityResult_type :&: cmp_cd_cells
  }


  @combinator object SceneAgentMqttVcdToGraph {
    def apply(c: MqttClient, v: CtpSceneConnectionValues2d,
              transformToPoly: Scene => PolygonScene, run: PolygonScene => PolySceneLineSegmentation,
              toCellSegmentation: PolySceneLineSegmentation => PolySceneCellSegmentation,
              toCentroids: PolySceneCellSegmentation => PolySceneCellSegmentationCentroids,
              toGraph: PolySceneCellSegmentationCentroids => Graph[List[Float], WUnDiEdge]): Unit = {
      def publishResult: String => Unit = { resultData =>
        logger.info("Publishing result to " + v.ctpScenePublishTopic)
        c.publish(v.ctpScenePublishTopic, resultToByteArray(resultData), 2, true)
      }

      def onMessage: IMqttMessageListener = (topic: String, message: MqttPahoMessage) => {
        logger.info("Received Payload: " + new String(message.getPayload))
        val decoded = decode[scene_type_2d_n](new String(message.getPayload))
        logger.info("decoded payload")

        if (decoded.isLeft) {
          logger.info(s"Error: ${decoded.left.get}")
        } else {
          logger.info(s"Starting cell decomposition")
          val r = run(transformToPoly(decoded.right.get))
          logger.info("computed result")
          logger.info(s"String ${toGraph(toCentroids(toCellSegmentation(r))).toString()}")
          publishResult(toCellSegmentation(r).asJson.toString())
          logger.info("Result published")
        }
      }
      logger.info("Combinator SceneAgentMqttVcd")

      logger.info("Publishing to Topic: " + v.ctpScenePublishTopic)
      logger.info("Subscribung to topic: " + v.ctpSceneSubscribeTopic)
      c.subscribe(v.ctpSceneSubscribeTopic, 2, onMessage)
      logger.info("SceneAgentMqttVcd Graph: Press <Enter> to exit.")
      scala.io.StdIn.readLine()
      logger.info(s"Disconnecting from MqttClient")
      c.disconnect()
      c.close()
    }

    val semanticType = p_mqttCirceClient_type =>:
      p_unitySceneConnectionValues_two_d_type :&: cmp_cd_cells =>:
      (sd_unity_scene_type =>: sd_polygon_scene_type) =>:
      (sd_polygon_scene_type =>: sd_polygon_scene_type :&: sd_scene_segmentation) =>:
      (sd_seg_lines :&: sd_poly_scene_segmentation =>: sd_seg_cells :&: sd_poly_scene_segmentation) =>:
      (sd_poly_scene_segmentation :&: sd_seg_cells =>: sd_poly_scene_segmentation :&: sd_seg_centroid_cells) =>:
      (sd_poly_scene_segmentation :&: sd_seg_centroid_cells =>: cmp_cell_graph) =>:
      p_unitySceneAgent_type :&: cmp_vertical_cell_decomposition_type :&: dimensionality_two_d_t :&:
        p_unityResult_type :&: cmp_cell_graph
  }

  @combinator object SceneAgentMqttVcd {
    def apply(c: MqttClient, v: CtpSceneConnectionValues2d,
              transformToPoly: Scene => PolygonScene, run: PolygonScene => PolySceneLineSegmentation, translateResult: PolySceneLineSegmentation => SegmentationLines2d): Unit = {
      def publishResult: String => Unit = { resultData =>
        logger.info("Publishing result to " + v.ctpScenePublishTopic)
        c.publish(v.ctpScenePublishTopic, resultToByteArray(resultData), 2, true)
      }

      def onMessage: IMqttMessageListener = (topic: String, message: MqttPahoMessage) => {
        logger.info("Received Payload: " + new String(message.getPayload))
        val decoded = decode[scene_type_2d_n](new String(message.getPayload))
        logger.info("decoded payload")

        if (decoded.isLeft) {
          logger.info(s"Error: ${decoded.left.get}")
        } else {
          logger.info(s"Starting cell decomposition")
          val r = run(transformToPoly(decoded.right.get))
          logger.info("computed result")
          publishResult(translateResult(r).asJson.toString())
          logger.info("Result published")
        }
      }
      logger.info("Combinator SceneAgentMqttVcd")

      logger.info("Response Topic: " + v.ctpSceneSubscribeTopic)
      c.subscribe(v.ctpSceneSubscribeTopic, 2, onMessage)
      logger.info("SceneAgentMqttVcd Lines: Press <Enter> to exit.")
      scala.io.StdIn.readLine()
      logger.info(s"Disconnecting from MqttClient")
      c.disconnect()
      c.close()
    }

    val semanticType = p_mqttCirceClient_type  =>:
      p_unitySceneConnectionValues_two_d_type :&:  cmp_cd_lines =>:
      (sd_unity_scene_type =>: sd_polygon_scene_type) =>:
      (sd_polygon_scene_type =>: sd_polygon_scene_type :&: sd_scene_segmentation) =>:
      Constructor("foo") =>:
      p_unitySceneAgent_type :&: cmp_vertical_cell_decomposition_type :&: dimensionality_two_d_t :&: p_unityResult_type :&: cmp_cd_lines
  }


  /*
    @combinator object SceneAgentMqttVcd2 {
      def apply(c: MqttClient, v: CtpSceneConnectionValues2d): Unit = {
        CtpSceneListenerVcd2(c, v).subscribe

        println("SceneAgentMqttVcd2: Press <Enter> to exit.")
        scala.io.StdIn.readLine()
        println(s"Disconnecting from MqttClient")
        c.disconnect()
        c.close()
      }

      val semanticType = p_mqttClient_type =>: p_unitySceneConnectionValues_two_d_type :&: gm_AABB =>: //optimized vcd2d uses aabbs
        p_unitySceneAgent_type :&: cmp_vertical_cell_decomposition_type :&: dimensionality_two_d_t :&: p_unityResult_type :&: cmp_cfree_polygons
    }*/



/*  @combinator object TopLevel {
    def apply(scene: scene_type_3d_n): Unit = {
      ???
    }

    val semanticType =  cmp_ac_scene_type :&: cmp_scene_source_var :&: dimensionality_var =>: //(can be unity, file description, native scala, parsable string)
      p_unitySceneAgent_type :&: cmp_decomposition_var :&: dimensionality_var :&: p_unityResult_type :&: cmp_ac_decomposition_type :&: cmp_cell_graph =>:
      cmp_graph_shortest_path_var :&: cmp_ac_spgs_type =>:
      cmp_ac_path_opt_type :&: cmp_path_opt_var =>:
      cmp_ac_result_type :&: cmp_result_var =>:
      cmp_combinatorial_type :&: cmp_scene_source_var :&: dimensionality_var :&: cmp_graph_shortest_path_var :&: cmp_path_opt_var :&: cmp_result_var
  }*/

  /*
  tasks:
    scene_description :&: unity ... // oder obj, mesh (file location), scala (function argument)
    scene decomposition :&: alpha :&: beta =>: //alpha scene decomp, beta dimension
    graph_search :&: gs_var =>:
    path_opt :&: po_var :&: beta =>:
    visualisation :&:
    combinatorial :&: mp :&: scene
  */


  @combinator object SceneConnectionValues {
    def apply(p: Properties): CtpSceneConnectionValues3d = {
      val ctpSceneRequest = p.getProperty("org.combinators.ctp.ctpSceneRequest")
      val ctpSceneResponse = p.getProperty("org.combinators.ctp.ctpSceneResponse")
      CtpSceneConnectionValues(ctpSceneRequest, ctpSceneResponse)
    }

    val semanticType = p_unityConnectionProperties_type =>: p_unitySceneConnectionValues_three_d_type
  }

  @combinator object SceneConnectionValues2d {
    def apply(p: Properties): CtpSceneConnectionValues2d = {
      val ctpSceneRequest = p.getProperty("org.combinators.ctp.ctpSceneRequest2D")
      val ctpSceneResponse = p.getProperty("org.combinators.ctp.ctpSceneFromUnity2D")
      CtpSceneConnectionValuesVcd(ctpSceneRequest, ctpSceneResponse)
    }

    val semanticType = p_unityConnectionProperties_type =>:  p_unitySceneConnectionValues_two_d_type :&: cmp_cd_lines
  }

  @combinator object SceneConnectionValues2dCells {
    def apply(p: Properties): CtpSceneConnectionValues2d = {
      val ctpScenePublishFromScala = p.getProperty("org.combinators.ctp.ctpSceneRequest2DCells")
      val ctpScenePushFromUnity = p.getProperty("org.combinators.ctp.ctpSceneFromUnity2D")
      CtpSceneConnectionValuesVcd(ctpScenePublishFromScala, ctpScenePushFromUnity)
    }

    val semanticType = p_unityConnectionProperties_type =>:  p_unitySceneConnectionValues_two_d_type :&: cmp_cd_cells
  }

  @combinator object SceneConnectionValues2daabb {
    def apply(p: Properties): CtpSceneConnectionValues2d = {
      val ctpSceneRequest = p.getProperty("org.combinators.ctp.ctpSceneRequest2D")
      val ctpSceneResponse = p.getProperty("org.combinators.ctp.ctpSceneFromUnity2D")
      CtpSceneConnectionValuesVcd(ctpSceneRequest, ctpSceneResponse)
    }

    val semanticType = p_unityConnectionProperties_type =>:  p_unitySceneConnectionValues_two_d_type :&: gm_AABB
  }

  @combinator object UnityMqttClient{
    def apply(p: Properties): MqttClient = {
      val broker = p.getProperty("org.combinators.ctp.broker")
      val clientId = p.getProperty("org.combinators.ctp.clientId")
      println(s"""Connecting to broker "$broker" as "$clientId"""")
      val client: MqttClient = new MqttClient(broker, clientId)
      val options: MqttConnectOptions = new MqttConnectOptions()
      options.setAutomaticReconnect(true)
      options.setCleanSession(true)
      options.setConnectionTimeout(10)
      options.setMaxInflight(1000)
      client.connect(options)
      client
    }

    val semanticType = p_unityConnectionProperties_type =>: p_mqttCirceClient_type
  }

  @combinator object UnityConnectionProperties {
    def apply:Properties = {
      val p = new Properties()
      p.load(getClass.getClassLoader.getResourceAsStream("mqtt.properties"))
      p
    }

    val semanticType = p_unityConnectionProperties_type
  }

// TODO combinator für utils
  //Combinator für scene connector objects
/*
  @combinator object
*/
}
