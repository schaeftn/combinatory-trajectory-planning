package org.combinators.ctp.repositories.pathcoverage

import com.typesafe.scalalogging.LazyLogging
import org.locationtech.jts.geom.impl.CoordinateArraySequence
import org.locationtech.jts.geom.util.AffineTransformation
import org.locationtech.jts.geom.{Coordinate, Geometry, GeometryFactory, LineString, LinearRing, Point, Polygon}

import org.locationtech.jts.algorithm.Centroid

trait SpiralDecomposition extends GeoUtils {
  val gf = new GeometryFactory()

  def selectMidPoint() = Centroid.getCentroid(p1)

  def getRadius(midPoint: Coordinate) = p1.getExteriorRing.distance(
    new Point(new CoordinateArraySequence(Array(midPoint)), gf))

  val ae: Double
  val selectionPredicate: LineString => Boolean
  val p1: Polygon
  lazy val p2 = p1.buffer(-ae)

  val p00 = new Coordinate(0.0, 0.0)
  val p01 = new Coordinate(0.0, -1.0)

  def toAffMatrix(p1: Point, p2: Point): AffineTransformation = {
    //    calc scale, calc rotation,build affine Transform
    val (deltax, deltay) = (p2.getX - p1.getX, p2.getY - p1.getY)
    val rot = Math.atan2(deltay, deltax)
    val deg = rot

    val rotMat = new AffineTransformation().setToRotation(deg - Math.PI * 0.5)
    val transMat = new AffineTransformation().setToTranslation(p1.getX, p1.getY)
    val at = rotMat.compose(transMat) // Matrix transforms primitive to use case path

    println(s"p1: ${p1.getCoordinate}, p2: ${p2.getCoordinate}, p00: $p00, p01: $p01")
    println(s"deg: $deg")
    println(s"at: $at")
    at
  }

  def getLines: Array[LineString] =
    (p2.getCoordinates zip p2.getCoordinates.tail) map {
      case (a, b) => new LineString(new CoordinateArraySequence(Array(a, b)), gf)
    }

  def getPath: Array[Geometry] = {
    lazy val lineStrings = getLines.map(i => {
      val length = i.getStartPoint.distance(i.getEndPoint)
      val asd = new Trochoidal {
        override val maxY: Float = length.toFloat
        override val maxStepSize: Float = 0.5f
        override val localOffsetFct: LocalMotionPrimitive = new CircularMotion {
          override val radius: Float = 0.5f
          override val localPolygonPoints: Int = 90
        }
      }

      toAffMatrix(i.getStartPoint, i.getEndPoint).transform(asd.getLineString)
    })
    lineStrings
  }

  def returnPath: List[List[Float]] = getPath.toList.map(_.getCoordinates).reduce(_ ++ _).
    toList.map(i => List(i.x.toFloat, i.y.toFloat, 0.0f))
}

object SpiralDecomposition extends App with LazyLogging {
  val gf = new GeometryFactory()
  val asd = new Moat {
    val tool = CncTool(d = 0.4f, ae = 0.05f, ap = 0.2f, vf = 1.0f, 1000)
    val selectionPredicate: LineString => Boolean = i => true
    val pArray = Array(new Coordinate(0.0d, 0.0d, 0.0d),
      new Coordinate(0.0d, 1.0d, 0.0d),
      new Coordinate(5.0d, 5.0d, 0.0d),
      new Coordinate(5.0d, -5.0d, 0.0d),
      new Coordinate(0.0d, 0.0d, 0.0d))
    val p1: Polygon = new Polygon(new LinearRing(new CoordinateArraySequence(pArray), gf), Array.empty[LinearRing], gf)
  }
  println(asd.returnPath)
  ToAkka(asd.returnPath)
}


