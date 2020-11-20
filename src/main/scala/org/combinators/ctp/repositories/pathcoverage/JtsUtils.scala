package org.combinators.ctp.repositories.pathcoverage

import java.util

import com.typesafe.scalalogging.LazyLogging
import org.locationtech.jts.geom.impl.CoordinateArraySequence
import org.locationtech.jts.geom.util.AffineTransformation
import org.locationtech.jts.geom.{Coordinate, Geometry, GeometryFactory, LineString, MultiLineString, Point}
import org.locationtech.jts.util.GeometricShapeFactory

trait JtsUtils extends LazyLogging {
  val gf = new GeometryFactory()

  val emptyGeometry = new Point(null, gf)

  def pGeo(s: String, g: Geometry): Unit = logger.info(s"$s\r\n$g")
  def asFloatList(a: Array[Coordinate]): List[List[Float]] = a.map(c => List(c.x.toFloat, c.y.toFloat)).toList

  def getBoundaryRectangle(l: List[Float]): Geometry = {
    if (l.length < 4) {
      logger.warn(s"getBoundaryRectangle. Argument list length should be 4 $l")
      return emptyGeometry
    }
    List(-75.0f, 75.0f, -60.0f, 60.0f)
    val gsf: GeometricShapeFactory = new GeometricShapeFactory();
    gsf.setBase(new Coordinate(l(0), l(2) ))
    gsf.setWidth(l(1) - l(0));
    gsf.setHeight(l(3) - l(2));
    gsf.createRectangle();
  }
  def getNewLineString(a: Array[Coordinate]): LineString = new LineString(new CoordinateArraySequence(a), gf)

  def getNewLineString(a: Coordinate, b: Coordinate): LineString = new LineString(new CoordinateArraySequence(Array(a, b)), gf)

  def getNewLineString(a: Array[LineString]): LineString = {
    val coords: Option[Array[Coordinate]] = a.map(_.getCoordinates).reduceOption(_ ++ _)
    val coordArray: Option[Array[Coordinate]] =
      coords.map(c => c.zip(c.tail).filterNot { case (a, b) => a.equals2D(b) }.map(_._1))
    getNewLineString(coordArray.getOrElse(Array.empty[Coordinate]))
  }

  def toAffMatrix(p1: Point, p2: Point): AffineTransformation = {
    //    calc scale, calc rotation,build affine Transform
    val (deltax, deltay) = (p2.getX - p1.getX, p2.getY - p1.getY)
    val rot = Math.atan2(deltay, deltax)
    val deg = rot

    val rotMat = new AffineTransformation().rotate(deg - Math.PI * 0.5)
    val transMat = new AffineTransformation().translate(p1.getX, p1.getY)
    val at = rotMat.compose(transMat) // Matrix transforms primitive to use case path

    at
  }

  def toAffMatrix(p1: Coordinate, a: Double): AffineTransformation = {
    val rotMat = new AffineTransformation().rotate(a - Math.PI * 0.5)
    val transMat = new AffineTransformation().translate(p1.getX, p1.getY)
    val at = rotMat.compose(transMat)
    at
  }

  def multiGeoToGeoList(p: Geometry): List[Geometry] =
    ((0 until p.getNumGeometries) map (i => p.getGeometryN(i))).toList

  def asMultiLine(l: List[LineString]): MultiLineString = gf.createMultiLineString(l.toArray)

  def toLineStringList(s: List[Coordinate]): List[LineString] = if (s.length > 1)
    s.zip(s.tail).map { case (a, b) => getNewLineString(Array(b, a)) }
  else
    List.empty[LineString]
}
