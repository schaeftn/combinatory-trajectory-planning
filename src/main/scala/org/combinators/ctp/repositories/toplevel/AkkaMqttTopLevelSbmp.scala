package org.combinators.ctp.repositories.toplevel

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
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.toplevel._
import org.combinators.ctp.repositories.scene._

import scala.concurrent.Future


trait AkkaMqttTopLevelSbmp extends LazyLogging with AkkaImplicits with AkkaMqttComponents {
  @combinator object SampleBasedMpAkka {
    def apply(p: Properties,
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
}
