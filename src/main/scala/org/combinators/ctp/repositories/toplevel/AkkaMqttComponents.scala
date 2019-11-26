package org.combinators.ctp.repositories.toplevel

import java.time.Duration
import java.util.Properties

import akka.Done
import akka.stream.alpakka.mqtt.scaladsl.{MqttFlow, MqttSink, MqttSource}
import akka.stream.alpakka.mqtt.{MqttConnectionSettings, MqttMessage, MqttQoS, MqttSubscriptions}
import akka.stream.scaladsl.{Sink, Source}
import com.typesafe.scalalogging.LazyLogging
import io.circe.syntax._
import io.circe.generic.auto._
import io.circe.parser.decode
import io.circe.{Decoder, Encoder, HCursor, Json}
import org.combinators.cls.interpreter.combinator
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.scene._
import org.eclipse.paho.client.mqttv3.persist.MemoryPersistence
import scalax.collection.Graph
import scalax.collection.edge.WUnDiEdge
import scalax.collection.io.json._
import org.combinators.ctp.repositories.mptasks.{MpTaskStartGoal, MpTaskStartGoalPose}
import org.combinators.ctp.repositories.toplevel.ListenerTupleTest.getClass

import scala.concurrent.Future

trait AkkaMqttComponents extends LazyLogging {


  @combinator object UnityMqttAkkaSourceScene{
    def apply(p: Properties): Source[Scene, Future[Done]] = {
      val broker = p.getProperty("org.combinators.ctp.broker")
      val topic = p.getProperty("org.combinators.ctp.ctpSceneFromUnity2D")

      val connectionSettings = MqttConnectionSettings(broker, "cls/Scene2DListener",
        new MemoryPersistence).withAutomaticReconnect(true)

      MqttSource.atLeastOnce(
        connectionSettings,
        MqttSubscriptions(topic, MqttQoS.AtLeastOnce),
        bufferSize = 8).map{i =>
        println(s"Received scene: $i")
        logger.info(s"Received scene: $i")
        decode[Scene](i.message.payload.utf8String).right.get}
    }

    val semanticType = p_unityConnectionProperties_type =>:
      p_mqttAkkaSource_type :&: sd_unity_scene_type :&: dimensionality_two_d_t
  }

  @combinator object UnityMqttAkkaSourceTask2D{
    def apply(p: Properties): Source[MpTaskStartGoal, Future[Done]] = {
      val broker = p.getProperty("org.combinators.ctp.broker")
      val topic = p.getProperty("org.combinators.ctp.ctpTaskSpFromUnity2D")

      val connectionSettings = MqttConnectionSettings(broker, "cls/Task2DListener",
        new MemoryPersistence).withAutomaticReconnect(true)

      MqttSource.atLeastOnce(
        connectionSettings,
        MqttSubscriptions(topic, MqttQoS.AtLeastOnce),
        bufferSize = 8).map { i =>
        println(s"Received task: $i")
        logger.info(s"Received task: $i")
        decode[MpTaskStartGoal](i.message.payload.utf8String).right.get
      }
    }

    val semanticType = p_unityConnectionProperties_type =>:
      p_mqttAkkaSource_type :&: mpt_start_goal_position_type :&: dimensionality_two_d_t
  }



  @combinator object UnityMqttAkkaSinkSceneSegGraphPath {
    def apply(p: Properties): Sink[MqttMessage, Future[Done]] = {
      val broker = p.getProperty("org.combinators.ctp.broker")
      val connectionSettings = MqttConnectionSettings(broker, "cls/GraphSink",
        new MemoryPersistence).withAutomaticReconnect(true)

      MqttSink(connectionSettings, MqttQoS.AtLeastOnce)
    }

    val semanticType = p_unityConnectionProperties_type =>:
      p_mqttAkkaSink_type :&: cmp_scene_graph_path :&: dimensionality_two_d_t
  }

  @combinator object UnityMqttAkkaConnectSettings {
    def apply(p: Properties): MqttConnectionSettings = {
      val broker = p.getProperty("org.combinators.ctp.broker")
      MqttConnectionSettings(broker, "",
        new MemoryPersistence).withAutomaticReconnect(true)
    }

    val semanticType = p_unityConnectionProperties_type =>: p_mqttAkkaConnxSettings_type
  }
}
