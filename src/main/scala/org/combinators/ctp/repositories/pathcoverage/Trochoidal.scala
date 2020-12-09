package org.combinators.ctp.repositories.pathcoverage

import org.combinators.ctp.repositories.scene.SceneUtils
import com.typesafe.scalalogging.LazyLogging
import org.combinators.ctp.repositories.toplevel.PathCoverageStepConfig
import org.locationtech.jts.algorithm.Angle
import org.locationtech.jts.geom.{Coordinate, Geometry, LineString, LinearRing, Point, Polygon}
import org.locationtech.jts.geom.impl.CoordinateArraySequence
import org.locationtech.jts.geom.util.AffineTransformation
import org.locationtech.jts.linearref.LinearLocation

trait CircleUtils extends GeoUtils {
  val twoPi = Math.PI * 2
}

trait GeoUtils {
  def translatePointBy(p: List[Float], t: List[Float]): List[Float] = (p zip t).map {
    case ((a, b)) => a + b
  }
}

trait LocalMotionPrimitive extends SceneUtils with CircleUtils with JtsUtils {
  val radius: Float
  val pointClearance: Double
  def getLocalOffset: Float => List[Float]
  val localPolygonPoints: Int
  val localPolygon: Polygon

  def drawPolygon(c: Coordinate, angle: Double): Geometry = toAffMatrix(c, angle).transform(localPolygon)
}

trait CircularMotion extends LocalMotionPrimitive {

  val getLocalOffset: Float => List[Float] =
    t => List(radius * Math.cos(t * 2 * Math.PI).toFloat,
      radius * Math.sin(t * 2 * Math.PI).toFloat, 0.0f) //local

  lazy val localPolygonPoints = Math.ceil(2 * Math.PI * radius / pointClearance).toInt

  lazy val localPolygon: Polygon = {
    val coords: IndexedSeq[Coordinate] = (0 to localPolygonPoints).map(i => { // to incl numPoints
      val newI = if (i == localPolygonPoints) 0 else i
      val rad = 2 * Math.PI * (newI.toDouble / localPolygonPoints.toDouble)
      //println(s"rad: $rad")
      //println(s"i/numpoints: ${newI.toDouble / numPoints.toDouble}")
      new Coordinate(radius * Math.cos(rad), radius * Math.sin(rad))
    })
    //println(s"coords: $coords")
    val lr = new LinearRing(new CoordinateArraySequence(coords.toArray), gf)
    new Polygon(lr,
      Array.empty[LinearRing], gf)
  }

}

trait LineStringTraversal extends JtsUtils {
  val localMotionPrimitive: LocalMotionPrimitive
  val ls: LineString
  val aeMax: Double
  lazy val lsLength = ls.getLength
  lazy val stepCount = math.ceil(lsLength / aeMax).toInt
  lazy val aeActual: Double = lsLength / stepCount
  lazy val accList: IndexedSeq[(Int, Coordinate, Double)] = {
    val coords = ls.getCoordinates
    val lengthList: Array[Double] = 0.0f +:
      (coords zip coords.tail).map { case (a, b) => getNewLineString(a, b).getLength }
    val accLengthList: Vector[Double] = accumulate(lengthList)
    (coords.indices zip coords zip accLengthList).map { case ((a, b), c) => (a, b, c) }
  }

  lazy val getPath: Float => List[Float] = localMotionPrimitive.getLocalOffset

  lazy val getSimplePath: Array[Geometry] = discretePointList.map { case (a, b) => toAffMatrix(a, b).
    transform(localMotionPrimitive.localPolygon.getExteriorRing)
  }.toArray

  lazy val getPolygon: Geometry =
    discretePointList.map(
      i => localMotionPrimitive.drawPolygon(i._1, i._2))
      .reduce((a, b) => a.union(b))

  def accumulate(seq: Array[Double]): Vector[Double] =
    seq.foldLeft(Vector.empty[Double]) { (result, e) =>
      result :+ result.lastOption.getOrElse(0.0d) + e
    }


  /**
   * returns list of tuple (coordinate, angle in rad)
   *
   * This is further documentation of what we're documenting.
   * Here are more details about how it works and what it does.
   */
  lazy val discretePointList: List[(Coordinate, Double)] =
    (0 to stepCount).map { i =>
//      val asd = getContinuousPointAndAngle(i * aeActual)
//      asd
      getContinuousPointAndAngle(i * aeActual).getOrElse((null, 0.0d))
    }.toList.filter { case (a, _) => a != null }

  def getContinuousPointAndAngle(targetLength: Double): Option[(Coordinate, Double)] = {
    accList.findLast(_._3 <= targetLength).flatMap { // find last c thats leq targetLength
      case (a, b, c) =>
        val restLength = targetLength - c
        accList.reverse.findLast(_._3 > c).map(_._2).map { // find first thats gt c
          segEndCoord =>
            (LinearLocation.pointAlongSegmentByFraction(b, segEndCoord, restLength / b.distance(segEndCoord)),
              Angle.angle(b, segEndCoord))
        }
    }
  }

  def getDiscretePointForIndex(i: Int): List[Double] = List(discretePointList(i)._1.x, discretePointList(i)._1.y)

  /**
   * selects a step number for a float parameter in between 0, 1
   * @param tpara
   * @return
   */
  def getStepForT(tpara: Float): Option[Int] = tpara match {
    case t if t < 0.0f || t > 1.0 => None
    case t =>
      accList.findLast { case (_, _, accLength) =>
        accLength < t * lsLength
      }.map(_._1)
  }
}

trait CircularMotion2 extends LocalMotionPrimitive {
  val d: Float = 5f
  override lazy val localPolygonPoints: Int = 0 // Not used
  val tLocalQuarterCircle: Float = Math.PI.toFloat * radius / (4 * (Math.PI.toFloat * radius + d))

  lazy val tLocalD: Float = d / (2 * Math.PI.toFloat * radius + 2 * d)

  lazy val getLocalOffset: Float => List[Float] = {
    case t if t <= tLocalQuarterCircle =>
      translatePointBy(
        List(radius * Math.cos(t / (4 * tLocalQuarterCircle) * 2 * Math.PI).toFloat,
          radius * Math.sin(t / (4 * tLocalQuarterCircle) * 2 * Math.PI).toFloat, 0.0f),
        List(d / 2, 0.0f, 0.0f))
    case t if t > tLocalQuarterCircle && t <= tLocalQuarterCircle + tLocalD =>
      val tNew = t - tLocalQuarterCircle
      List(
        d / 2 - (tNew / tLocalD) * d, radius, 0.0f)
    case t if t > tLocalQuarterCircle + tLocalD && t <= 3 * tLocalQuarterCircle + tLocalD =>
      val tNew = t - (tLocalQuarterCircle + tLocalD)
      translatePointBy(
        List(radius * Math.cos(tNew / (4 * tLocalQuarterCircle) * Math.PI * 2 + Math.PI / 2).toFloat,
          radius * Math.sin(tNew / (4 * tLocalQuarterCircle) * Math.PI * 2 + Math.PI / 2).toFloat, 0.0f),
        List(-d / 2, 0.0f, 0.0f))
    case t if t > 3 * tLocalQuarterCircle + tLocalD && t <= 3 * tLocalQuarterCircle + 2 * tLocalD =>
      val tNew = t - 3 * tLocalQuarterCircle - tLocalD
      List(
        (-d / 2 + (tNew / tLocalD) * d), -radius, 0.0f)
    case t if t > 3 * tLocalQuarterCircle + 2 * tLocalD =>
      val tNew = t - 3 * tLocalQuarterCircle - 2 * tLocalD
      translatePointBy(
        List(radius * Math.cos(tNew / (4 * tLocalQuarterCircle) * Math.PI * 2 + 1.5 * Math.PI).toFloat,
          radius * Math.sin(tNew / (4 * tLocalQuarterCircle) * Math.PI * 2 + 1.5 * Math.PI).toFloat,
          0.0f),
        List(d / 2, 0.0f, 0.0f))
    case a =>
      println(s"emptyList for value: $a")
      List.empty[Float]
  }
}


trait LinearStepSize {
  val maxY: Float
  val maxStepSize: Float
  lazy val actualStepSize: Float = maxY / numberOfSteps
  lazy val numberOfSteps: Int = math.ceil(maxY / maxStepSize).toInt
  lazy val getTPerStep: Float = 1.0f / numberOfSteps

  def getStepForT(t: Float): Int = math.floor(t * numberOfSteps).toInt

  def linearGlobalToLocal(t: Float): Float = (t % getTPerStep) / getTPerStep //local t from 0.0 1.0
}


//Frage Konstanter faktor vt? vgl. Trochoidal milling paper 2018
trait Trochoidal extends SceneUtils with LinearStepSize with LazyLogging with JtsUtils {
  val zVal: Float = 0.0f
  val maxPathParamLocal = 1.0f // Path is a function which maps t from 0.0 to 100.0f to a point in R3
  val xVal: Float = 0.0f
  val tResolution = 1000
  val localOffsetFct: LocalMotionPrimitive

  def getSimpleTrochoidalPath: List[List[Float]] =
    (0 until tResolution).map(i => getPathFunction(i.toFloat / tResolution)).toList

  def getLineString:Geometry =
    if (maxY <= 0) {
      emptyGeometry
    }
    else {
      new LineString(new CoordinateArraySequence(getSimpleTrochoidalPath.toArray.map(
        i => new Coordinate(i.head, i(1), i(2)))), gf)
    }

  def getPathFunction: Float => List[Float] = { // Absolute Float
    t: Float =>
      val basePointFct: List[Float] = getBasePointForTLinear(t)
      val localOffset: List[Float] = localOffsetFct.getLocalOffset(linearGlobalToLocal(t))
      val resultPoint: List[Float] = (basePointFct zip localOffset).map { case ((a, b)) => a + b }
      resultPoint
  }

  def getBasePointForTLinear: Float => List[Float] = t => List(xVal, getStepForT(t) * maxStepSize, 0.0f)

  def getPolygon: Geometry = {
    if (maxY <= 0.0) {
      emptyGeometry
    } else {
      val polygon = localOffsetFct.localPolygon
      val mats = (0 to numberOfSteps).map(i => i * actualStepSize).map(i => {
        // logger.info(s"maxY, stepSize, actualStepSize, i: $maxY, $actualStepSize, $i")
        AffineTransformation.translationInstance(0.0d, i.toDouble)
      }
      )
      val transformedPolys: IndexedSeq[Geometry] = mats.map(at => at.transform(polygon))

      val xArray: Array[Coordinate] = Array(new Coordinate(0.0d, 0.0d),
        new Coordinate(localOffsetFct.radius.toDouble, 0.0d),
        new Coordinate(localOffsetFct.radius.toDouble, maxY.toDouble),
        new Coordinate(0.0d, maxY.toDouble),
        new Coordinate(0.0d, 0.0d))
      if (transformedPolys.isEmpty) {
        logger.info("foo")
        new Point(null, gf)
      }
      else {
        val poly = if (transformedPolys.size > 1)
          transformedPolys.reduce[Geometry] { case (a: Geometry, b: Geometry) => a.union(b) }
        else
          transformedPolys.head
        val adapt = new Polygon(new LinearRing(new CoordinateArraySequence(xArray), gf), Array.empty[LinearRing], gf)
        val testStr = s"$poly"
        if (testStr.contains("�")) {
          logger.warn(s"Corrupted string in trochoidal polygon: $testStr")
        }
        //      logger.info(s"poly: $poly")
        //      logger.info(s"adapt: $adapt")
        poly union adapt
      }
    }
  }
}

object RunTrochoidalPath extends App {
  val trochPrimitive = new Trochoidal {
    override val maxY: Float = 100.0f
    override val maxStepSize: Float = 10f
    override val localOffsetFct: LocalMotionPrimitive = new CircularMotion {
      override val radius: Float = 5f
      override val pointClearance: Double = 0.2d
    }
  }
  println(trochPrimitive.getSimpleTrochoidalPath)
  ToAkka(trochPrimitive.getSimpleTrochoidalPath)
}


