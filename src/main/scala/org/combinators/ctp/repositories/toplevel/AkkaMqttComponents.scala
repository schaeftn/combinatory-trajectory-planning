package org.combinators.ctp.repositories.toplevel

import java.util.UUID

import akka.Done
import akka.stream.alpakka.mqtt.scaladsl.{MqttSink, MqttSource}
import akka.stream.alpakka.mqtt.{MqttConnectionSettings, MqttMessage, MqttQoS, MqttSubscriptions}
import akka.stream.scaladsl.{Sink, Source}
import com.typesafe.scalalogging.LazyLogging
import io.circe.generic.auto._
import io.circe.parser.decode
import org.combinators.cls.interpreter.combinator
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories._
import org.eclipse.paho.client.mqttv3.persist.MemoryPersistence

import scala.concurrent.Future

trait AkkaMqttComponents extends LazyLogging with EncodeImplicits with PropertyFiles {
  val broker = mqttProperties.getProperty("org.combinators.ctp.broker")
  val mqttAkkaConnxSettings = MqttConnectionSettings(broker, "", new MemoryPersistence).
    withAutomaticReconnect(true)

  def filterOption: Option[Any] => Boolean = {
    case Some(_) => true
    case _ => logger.info("MqttSource: Error while decoding. Message will be ignored.")
      false
  }

  @combinator object UnityMqttAkkaSourceScene{
    def apply: Source[Option[Scene], Future[Done]] = {
      val broker = mqttProperties.getProperty("org.combinators.ctp.broker")
      val topic = mqttProperties.getProperty("org.combinators.ctp.ctpSceneFromUnity2D")

      val connectionSettings = MqttConnectionSettings(broker, "cls/Scene2DListener",
        new MemoryPersistence).withAutomaticReconnect(true)

      MqttSource.atLeastOnce(
        connectionSettings,
        MqttSubscriptions(topic, MqttQoS.AtLeastOnce),
        bufferSize = 8).map{i =>
        logger.info(s"Received scene: $i")
        decode[Scene](i.message.payload.utf8String).toOption}
    }

    val semanticType = p_mqttAkkaSource_type :&: sd_unity_scene_type :&: dimensionality_two_d_t
  }

  @combinator object UnityMqttAkkaSourceScene3D{
    def apply: Source[Option[Scene], Future[Done]] = {
      val broker = mqttProperties.getProperty("org.combinators.ctp.broker")
      val topic = mqttProperties.getProperty("org.combinators.ctp.ctpSceneFromUnity3D")

      val connectionSettings = MqttConnectionSettings(broker, "cls/Scene3DListener",
        new MemoryPersistence).withAutomaticReconnect(true)

      MqttSource.atLeastOnce(
        connectionSettings,
        MqttSubscriptions(topic, MqttQoS.AtLeastOnce),
        bufferSize = 8).map{i =>
        logger.info(s"Received scene: $i")
        decode[Scene](i.message.payload.utf8String).toOption}
    }

    val semanticType = p_mqttAkkaSource_type :&: sd_unity_scene_type :&: dimensionality_three_d_t
  }

  @combinator object UnityMqttAkkaSourceSceneSRT3D {
    def apply: Source[Option[SceneSRT], Future[Done]] = {
      val broker = mqttProperties.getProperty("org.combinators.ctp.broker")
      val topic = mqttProperties.getProperty("org.combinators.ctp.ctpSceneSRTFromUnity3D")

      val connectionSettings = MqttConnectionSettings(broker, "cls/Scene3DSRTListener",
        new MemoryPersistence).withAutomaticReconnect(true)

      MqttSource.atLeastOnce(
        connectionSettings,
        MqttSubscriptions(topic, MqttQoS.AtLeastOnce),
        bufferSize = 8).map { i =>
        logger.info(s"Received 3D SRT scene: $i")
        decode[SceneSRT](i.message.payload.utf8String).toOption
      }
    }

    val semanticType = p_mqttAkkaSource_type :&: sd_unity_scene_srt_type :&: dimensionality_three_d_t
  }


  @combinator object UnityMqttAkkaSourceSceneSRT3DTopicUuid {
    def apply: UUID => Source[Option[SceneSRT], Future[Done]] = { uuid =>
      val broker = mqttProperties.getProperty("org.combinators.ctp.broker")
      val topic = mqttProperties.getProperty("org.combinators.ctp.ctpSceneSRTFromUnity3D") + "." + uuid

      val connectionSettings = MqttConnectionSettings(broker, s"cls/Scene3DSRTListener.$uuid",
        new MemoryPersistence).withAutomaticReconnect(true)

      MqttSource.atLeastOnce(
        connectionSettings,
        MqttSubscriptions(topic, MqttQoS.AtLeastOnce),
        bufferSize = 8).map { i =>
        logger.info(s"Received 3D SRT scene: $i")
        decode[SceneSRT](i.message.payload.utf8String).toOption
      }
    }

    val semanticType = p_mqttAkkaSource_type :&: sd_unity_scene_srt_type :&: dimensionality_three_d_t
  }

  @combinator object UnityMqttAkkaSourceSceneSRT2D {
    def apply: Source[Option[SceneSRT], Future[Done]] = {
      val broker = mqttProperties.getProperty("org.combinators.ctp.broker")
      val topic = mqttProperties.getProperty("org.combinators.ctp.ctpSceneSRTFromUnity2D")

      val connectionSettings = MqttConnectionSettings(broker, "cls/Scene2DSRTListener",
        new MemoryPersistence).withAutomaticReconnect(true)

      MqttSource.atLeastOnce(
        connectionSettings,
        MqttSubscriptions(topic, MqttQoS.AtLeastOnce),
        bufferSize = 8).map { i =>
        logger.info(s"Received 2D SRT scene: $i")
        decode[SceneSRT](i.message.payload.utf8String).toOption
      }
    }

    val semanticType = p_mqttAkkaSource_type :&: sd_unity_scene_srt_type :&: dimensionality_two_d_t
  }

  @combinator object UnityMqttAkkaSourceTask2D {
    def apply: Source[Option[MpTaskStartGoal], Future[Done]] = {
      val broker = mqttProperties.getProperty("org.combinators.ctp.broker")
      val topic = mqttProperties.getProperty("org.combinators.ctp.ctpTaskSpFromUnity2D")

      val connectionSettings = MqttConnectionSettings(broker, "cls/Task2DListener",
        new MemoryPersistence).withAutomaticReconnect(true)

      MqttSource.atLeastOnce(
        connectionSettings,
        MqttSubscriptions(topic, MqttQoS.AtLeastOnce),
        bufferSize = 8).map { i =>
        logger.info(s"Received task: $i")
        decode[MpTaskStartGoal](i.message.payload.utf8String).toOption
      }
    }

    val semanticType = p_mqttAkkaSource_type :&: mpt_start_goal_position_type :&: dimensionality_two_d_t
  }

  @combinator object UnityMqttAkkaSourceTask3D{
    def apply: Source[Option[MpTaskStartGoal], Future[Done]] = {
      val broker = mqttProperties.getProperty("org.combinators.ctp.broker")
      val topic = mqttProperties.getProperty("org.combinators.ctp.ctpTaskSpFromUnity3D")

      val connectionSettings = MqttConnectionSettings(broker, "cls/Task3DListener",
        new MemoryPersistence).withAutomaticReconnect(true)

      MqttSource.atLeastOnce(
        connectionSettings,
        MqttSubscriptions(topic, MqttQoS.AtLeastOnce),
        bufferSize = 8).map { i =>
        logger.info(s"Received task: $i")
        decode[MpTaskStartGoal](i.message.payload.utf8String).toOption
      }
    }

    val semanticType = p_mqttAkkaSource_type :&: mpt_start_goal_position_type :&: dimensionality_three_d_t
  }



  @combinator object UnityMqttAkkaSourceTask3DUuid{
    def apply: UUID => Source[Option[MpTaskStartGoal], Future[Done]] = { uuid =>
      val broker = mqttProperties.getProperty("org.combinators.ctp.broker")
      val topic = mqttProperties.getProperty("org.combinators.ctp.ctpTaskSpFromUnity3D") + "." + uuid.toString

      val connectionSettings = MqttConnectionSettings(broker, "cls/Task3DListener",
        new MemoryPersistence).withAutomaticReconnect(true)

      MqttSource.atLeastOnce(
        connectionSettings,
        MqttSubscriptions(topic, MqttQoS.AtLeastOnce),
        bufferSize = 8).map { i =>
        logger.info(s"Received task: $i")
        decode[MpTaskStartGoal](i.message.payload.utf8String).toOption
      }
    }

    val semanticType = p_mqttAkkaSource_type :&: mpt_start_goal_position_type :&: dimensionality_three_d_t
  }


  @combinator object MqttAkkaProblemFileLoader{
    def apply: UUID => Source[Option[ProblemDefinitionFiles], Future[Done]] = { uuid =>
      val broker = mqttProperties.getProperty("org.combinators.ctp.broker")
      val topic = mqttProperties.getProperty("org.combinators.ctp.fileBasedSbmpRequest")

      val connectionSettings = MqttConnectionSettings(broker, "cls/ProblemFileLoader",
        new MemoryPersistence).withAutomaticReconnect(true)

      MqttSource.atLeastOnce(
        connectionSettings,
        MqttSubscriptions(topic + "."+uuid, MqttQoS.AtLeastOnce),
        bufferSize = 8).map { i =>
        logger.info(s"Received config file string: ${i.message.payload.utf8String}")
        ProblemDefinitionFiles(i.message.payload.utf8String)
      }
    }

    val semanticType = p_mqttAkkaSource_type :&: util_file_reader_type
  }

  @combinator object UnityMqttAkkaSinkSceneSegGraphPath {
    def apply: Sink[MqttMessage, Future[Done]] = {
      val broker = mqttProperties.getProperty("org.combinators.ctp.broker")
      val connectionSettings = MqttConnectionSettings(broker, "cls/GraphSink",
        new MemoryPersistence).withAutomaticReconnect(true)

      MqttSink(connectionSettings, MqttQoS.AtLeastOnce)
    }

    val semanticType = p_mqttAkkaSink_type :&: cmp_scene_graph_path :&: dimensionality_var
  }

  //Test for Akka file sbmp, replace with UnityMqttAkkaSinkPath3D?
  @combinator object UnityMqttAkkaSinkPath3DNew {
    def apply: Sink[MqttMessage, Future[Done]] = {
      val broker = mqttProperties.getProperty("org.combinators.ctp.broker")
      val connectionSettings = MqttConnectionSettings(broker, "cls/FileBasedSink",
        new MemoryPersistence).withAutomaticReconnect(true)

      MqttSink(connectionSettings, MqttQoS.AtLeastOnce)
    }

    val semanticType = p_mqttAkkaSink_type :&: cmp_path_only :&: dimensionality_three_d_t
  }


  @combinator object UnityMqttAkkaSinkPath3D {
    def apply: Sink[MqttMessage, Future[Done]] = {
      val broker = mqttProperties.getProperty("org.combinators.ctp.broker")
      val connectionSettings = MqttConnectionSettings(broker, "cls/Path3D",
        new MemoryPersistence).withAutomaticReconnect(true)

      MqttSink(connectionSettings, MqttQoS.AtLeastOnce)
    }

    val semanticType = p_mqttAkkaSink_type :&: cmp_path_only :&: dimensionality_three_d_t
  }

  @combinator object UnityMqttAkkaSinkPath2D {
    def apply: Sink[MqttMessage, Future[Done]] = {
      val broker = mqttProperties.getProperty("org.combinators.ctp.broker")
      val connectionSettings = MqttConnectionSettings(broker, "cls/Path2D",
        new MemoryPersistence).withAutomaticReconnect(true)

      MqttSink(connectionSettings, MqttQoS.AtLeastOnce)
    }

    val semanticType = p_mqttAkkaSink_type :&: cmp_path_only :&: dimensionality_two_d_t
  }
}
