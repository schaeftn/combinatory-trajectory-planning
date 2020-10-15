package org.combinators.ctp.repositories.pathcoverage

import com.typesafe.scalalogging.LazyLogging
import org.combinators.ctp.repositories.pathcoverage.RunTrochoidalPath.asd
import org.combinators.ctp.repositories.scene.SceneUtils


trait NonLinearStepSize extends LazyLogging {
  val maxPathParam = 1.0f // Path is a function which maps t in (0.0, value) to a point in R3
  lazy val maxY: Float = 1.0f
  lazy val minY: Float = 0.1f
  lazy val stepSize: Float = 1.0f

  def getStepForT(t: Float): Int = math.floor(t / maxPathParam * numberOfSteps).toInt

  lazy val numberOfSteps: Int = {
    logger.info(s"maxY: $maxY, minY: $minY, stepSize: $stepSize, calc value: ${(maxY - minY) / stepSize}," +
      s"math.ceil: ${math.ceil((maxY - minY) / stepSize)}" +
      s"numberOfSteps: ${math.ceil((maxY - minY) / stepSize).toInt}")
    math.ceil(((maxY - minY) / stepSize).abs).toInt
  }

  def getPathLengthForStep(t: Int): Float

  def getLocalPathForStep(t: Int): Float => List[Float]

  def getGlobalToLocalT(t: Float): (Int, Float) // StepId, LocalT
}

//Unter 60° Tauchbohrer oder Konventionell? Frage Relevanz? Abbildung Repository?
//Frage: Formel Umschlingungswinkel?
//Frage Besäumen: Auch konstanter faktor vt? vgl. Trochoidal milling paper 2018 Antwort: Könnte man machen
//Frage: Macht in bestimmten Fällen Sinn, hier nicht die Winkelhalbierende zu verwenden? Vermutung: Wahrscheinlich

trait InnerEdgeTop extends SceneUtils with NonLinearStepSize with LazyLogging{
  val innerEdgeAlpha: Float
  val resolution: Int

  lazy val getPathLengths = {
    logger.debug(s"numberOfSteps: $numberOfSteps")
    (0 until numberOfSteps).map(i => getPathLengthForStep(i))
  }

  lazy val getPl2 = {
    logger.debug(s"getPathLengths: $getPathLengths")

    getPathLengths.map(i => List(i, stepSize)).foldLeft(List.empty[Float]) { case (a, b) => a ++ b }
  }

  def getCompletePathLength: Float = getPathLengths.sum + (stepSize * numberOfSteps - 1).abs

  lazy val mapPlToTUntil = {
    logger.debug(s"getPl2: $getPl2")
    getPl2.indices.map(i => List(getPl2.take(i + 1).sum / getCompletePathLength, 1.0f).min
    )
  }

  lazy val localFunctions = (0 until numberOfSteps) map (i => new InnerEdgeLocal {
    logger.debug(s"innerEdgeLocal: innerEdgeAlpha: $innerEdgeAlpha")
    override def alpha: Float = innerEdgeAlpha
    override def d: Float = maxY.abs - (stepSize * i)
    logger.info(s"new LocalFunction. d: $d")
  })

  override def getPathLengthForStep(t: Int) = localFunctions(t).completeLocalPathlength
  override def getLocalPathForStep(t:Int): Float => List[Float] = localFunctions(t).getLocalPathFct

  def getLinearFctFor(p1: List[Float], p2: List[Float]): Float => List[Float] = { t =>
    val diffVector = (p2 zip p1).map { case (a, b) => a - b }
    (p1 zip diffVector.map(i => i * t)).map { case (a, b) => a + b }
  }

  val localFunctionsComplete = localFunctions.foldLeft(List.empty[Float => List[Float]]){
    case(f1,f2) if f1.isEmpty => List(f2.getLocalPathFct)
    case(f1,f2) => f1 :+ getLinearFctFor(f1.last(1.0f), f2.getLocalPathFct(0.0f)) :+ f2.getLocalPathFct
  }

  override def getGlobalToLocalT(t: Float): (Int, Float) = {
    //logger.info(s"checking List ($mapPlToTUntil for t:$t)")

    val stepId: Int = mapPlToTUntil.takeWhile(value => value <= t).size
    logger.debug(s"t: $t, mapPlToTUntil: $mapPlToTUntil")
    val adaptedmapPlToTUntil = 0.0f +: mapPlToTUntil
    val takenList: (Float, Float) = (adaptedmapPlToTUntil.findLast(value => t>=value).getOrElse(0.0f),
      adaptedmapPlToTUntil.find(value => t<=value).getOrElse(1.0f))
    logger.debug(s"t: $t, takenList: $takenList")
    val moduloT: Float = t - takenList._1
    val localT: Float = moduloT / (takenList._2 - takenList._1 )

    //logger.info(s"returning ($stepId, $localT)")
    (stepId, localT)
  }

  def getPoint: Float => List[Float] = { t =>
    lazy val (step, localT) = getGlobalToLocalT(t)
    if (localFunctionsComplete.size >= step + 1) {
      localFunctionsComplete(step)(localT)
    } else {
      List.empty[Float]
    }
  }

  def getCompletePath: List[List[Float]] = (0 until resolution).map(i => getPoint(1.0f * i / resolution)).filter(_.nonEmpty).toList
}

trait InnerEdgeLocal extends CircleUtils with LazyLogging {
  def alpha: Float // angle in rad
  def d: Float // Distanz von d (auf y-Achse) zu 0.0 Mitteldistanz.
  def beta:Float = (Math.PI - alpha).toFloat

  lazy val basePoint = List(0.0f, -d - computeRadius, 0.0f)
  // r/(r+d) = Sin (alpha/2)
  lazy val computeRadius: Float = (d * Math.sin(alpha / 2) / (1 - Math.sin(alpha / 2))).abs.toFloat
  lazy val circularSegmentSize = computeRadius * beta
  lazy val completeLocalPathlength = (circularSegmentSize + getXDistance).toFloat

  def getAngleForT(t: Float) = {
    val tNormalized1 = t * (completeLocalPathlength / circularSegmentSize)
    val result = (alpha / 2) + tNormalized1 * beta

    if (result > (alpha / 2) + beta)
      print("omgpls")
    result
  }

  // t in [0,1]
  def getLocalPathFct: Float => List[Float] = { t =>
    logger.info(s"LocalPath t: $t")
    if (t <= circularSegmentSize / completeLocalPathlength)
      List(( Math.cos(getAngleForT(t)) * computeRadius).toFloat,
        (Math.sin(getAngleForT(t)) * computeRadius).toFloat - d - computeRadius, 0.0f)
    else if (t >= 0 && t <= 1.0) {
      val tLineNormalized: Float =
        ((t - circularSegmentSize / completeLocalPathlength) * completeLocalPathlength / getXDistance).toFloat
      val tNormalized2 = t - 1.0f * (circularSegmentSize / completeLocalPathlength)
      val resultPoint: List[Float] = List(
        -getXDistance / 2 + tLineNormalized * getXDistance,
        (computeRadius * Math.sin(alpha / 2)).toFloat - d - computeRadius,
        0.0f)
      resultPoint
    } else {
      println(s"received invalid t: $t")
      List(0.0f, 0.0f, 0.0f)
    }
  }

  lazy val getXDistance: Float = (Math.cos(alpha/2) * computeRadius * 2).toFloat
  lazy val getMiddle: List[Float] = List(0.0f, -d - computeRadius)
}


trait InnerEdgeUtils{
  //given 3 distances, compute middistance
  def computeMidDistance(dl: Float, dm: Float, dr:Float) = ???
}

object InnerEdgeTest extends App {
  val asd = new InnerEdgeTop{
    override lazy val innerEdgeAlpha = Math.toRadians(75.0d).toFloat
    override lazy val resolution = 10000
    override lazy val stepSize: Float = 1.0f
    override lazy val maxY: Float = -10.0f
    override lazy val minY: Float = -1.0f
  }
  val p: List[List[Float]] = asd.getCompletePath
  println(p)
  ToAkka(p)
}