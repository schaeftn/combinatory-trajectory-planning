package org.combinators.ctp.repositories.pathcoverage

import org.combinators.ctp.repositories.scene.SceneUtils
import org.combinators.ctp.repositories.toplevel.{AkkaImplicits, PropertyFiles}
import akka.stream.alpakka.mqtt.scaladsl.{MqttSink, MqttSource}
import akka.stream.alpakka.mqtt.{MqttConnectionSettings, MqttMessage, MqttQoS, MqttSubscriptions}
import akka.stream.scaladsl.{Sink, Source}
import io.circe.syntax._
import akka.stream.alpakka.mqtt.MqttMessage
import akka.util.ByteString
import akka.{Done, NotUsed}
import com.typesafe.scalalogging.LazyLogging
import org.eclipse.paho.client.mqttv3.persist.MemoryPersistence

import scala.concurrent.Future

trait CircleUtils extends tempGeoUtils{
  val twoPi = Math.PI * 2
}

trait tempGeoUtils {
  def translatePointBy(p: List[Float], t: List[Float]): List[Float] = (p zip t).map { case ((a, b)) => a + b }
}

trait LocalMotionPrimitive extends SceneUtils with CircleUtils {
  def getLocalOffset: Float => List[Float]
}

trait CircularMotion extends LocalMotionPrimitive {
  val radius: Float
  def getLocalOffset: Float => List[Float] =
    t => List(radius * Math.cos(t * 2 * Math.PI).toFloat, radius * Math.sin(t * 2 * Math.PI).toFloat, 0.0f) //local
}

trait CircularMotion2 extends LocalMotionPrimitive {
  val radius: Float
  val d: Float = 5f

  def tLocalQuarterCircle: Float = (Math.PI.toFloat * radius / (4 * (Math.PI.toFloat * radius + d)))

  def tLocalD: Float = d / (2 * Math.PI.toFloat * radius + 2 * d)

  def getLocalOffset: Float => List[Float] = {
    case t if (t <= tLocalQuarterCircle) =>
      translatePointBy(
        List(radius * Math.cos(t / (4 * tLocalQuarterCircle) * 2 * Math.PI).toFloat, radius * Math.sin(t / (4 * tLocalQuarterCircle) * 2 * Math.PI).toFloat, 0.0f),
        List(d / 2, 0.0f, 0.0f))
    case t if (t > tLocalQuarterCircle && t <= tLocalQuarterCircle + tLocalD) =>
      val tNew = t - tLocalQuarterCircle
      List(
        (d / 2 - (tNew / tLocalD) * d).toFloat, radius, 0.0f)
    case t if (t > tLocalQuarterCircle + tLocalD && t <= 3 * tLocalQuarterCircle + tLocalD) => {
      val tNew = t - (tLocalQuarterCircle + tLocalD)
      translatePointBy(
        List(radius * Math.cos(tNew / (4 * tLocalQuarterCircle) * Math.PI * 2 + Math.PI / 2).toFloat, radius * Math.sin(tNew / (4 * tLocalQuarterCircle) * Math.PI * 2 + Math.PI / 2).toFloat, 0.0f),
        List(-d / 2, 0.0f, 0.0f))
    }
    case t if (t > 3 * tLocalQuarterCircle + tLocalD && t <= 3 * tLocalQuarterCircle + 2 * tLocalD) => {
      val tNew = t - 3 * tLocalQuarterCircle - tLocalD
      List(
        (-d / 2 + (tNew / tLocalD) * d).toFloat, -radius, 0.0f)
    }
    case t if (t > 3 * tLocalQuarterCircle + 2 * tLocalD) => {
      val tNew = t - 3 * tLocalQuarterCircle - 2 * tLocalD
      translatePointBy(
        List(radius * Math.cos(tNew / (4 * tLocalQuarterCircle) * Math.PI * 2 + 1.5 * Math.PI).toFloat,
          radius * Math.sin(tNew / (4 * tLocalQuarterCircle) * Math.PI * 2 + 1.5 * Math.PI).toFloat,
          0.0f),
        List(d / 2, 0.0f, 0.0f))
    }
    case a =>
      println(s"emptyList for value: ${a}")
      List.empty[Float]
  }
}

trait LinearStepSize {
  val maxPathParam = 10.0f // Path is a function which maps t from 0.0 to 100.0f to a point in R3
  val maxY: Float = 10.0f
  val stepSize: Float

  def getStepForT(t: Float): Int = math.floor(t / maxPathParam * numberOfSteps).toInt
  def numberOfSteps: Int = math.ceil(maxY / stepSize).toInt
  def getTPerStep(t: Float): Float = maxPathParam / numberOfSteps
  def linearGlobalToLocal(t: Float): Float = (t % getTPerStep(t)) / getTPerStep(t) //local t from 0.0 1.0
}


//Frage Konstanter faktor vt? vgl. Trochoidal milling paper 2018
trait Trochoidal extends SceneUtils with LinearStepSize {
  val minY: Float = 1.0f
  val zVal: Float = 0.0f
  val maxPathParamLocal = 1.0f // Path is a function which maps t from 0.0 to 100.0f to a point in R3
  val xVal: Float = 0.0f
  val tResolution = 10000
  val localOffsetFct: LocalMotionPrimitive
  def getSimpleTrochoidalPath(): List[List[Float]] = (0 until tResolution).map(i => getPathFunction(maxPathParam / tResolution * i)).toList

  def getPathFunction: Float => List[Float] = { // Absolute Float
    t: Float =>
      val basePointFct: List[Float] = getBasePointForTLinear(t)
      val localOffset: List[Float] = localOffsetFct.getLocalOffset(linearGlobalToLocal(t)) //TODO Parametrisieren?
      val asd: List[Float] = (basePointFct zip localOffset).map { case ((a, b)) => a + b }
      asd
  }

  def getBasePointForTLinear: Float => List[Float] = t => List(xVal, getStepForT(t) * stepSize, 0.0f)
}

object RunTrochoidalPath extends App {
  val asd = new Trochoidal {
    override val stepSize: Float = 1.0f
    override val localOffsetFct: LocalMotionPrimitive = new CircularMotion {
      override val radius: Float = 5f
    }
  }
  println(asd.getSimpleTrochoidalPath())
  ToAkka(asd.getSimpleTrochoidalPath())
}


