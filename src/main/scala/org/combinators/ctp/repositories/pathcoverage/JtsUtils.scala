package org.combinators.ctp.repositories.pathcoverage

import com.typesafe.scalalogging.LazyLogging
import org.combinators.ctp.repositories.toplevel.PathCoverageStepConfig
import org.locationtech.jts.dissolve.LineDissolver
import org.locationtech.jts.geom.util.AffineTransformation
import org.locationtech.jts.geom.{Coordinate, Geometry, GeometryFactory, LineSegment, LineString, LinearRing, MultiLineString, MultiPolygon, Point, Polygon}
import org.locationtech.jts.io.WKTReader
import org.locationtech.jts.util.GeometricShapeFactory

import scala.util.Try


trait JtsUtils extends LazyLogging with CircleUtils {
  val gf = new GeometryFactory()
  val emptyLs = gf.createLineString()
  val emptyGeometry = new Point(null, gf)
  val zIdleValue = 20.0f
  val coverageErrorMargin = 0.1d

  def wktRead(s: String): Geometry = {
    val wktReader = new WKTReader()
    wktReader.read(s)
  }

  def robustIntersection(g1: Geometry, g2: Geometry): Geometry = {
    Try {
      val g1b = if (g1.getGeometryType == "LineString" || g1.getGeometryType == "MultiLineString")
        g1 else g1.buffer(0)
      g1b.intersection(g2.buffer(0))
    }.getOrElse({
      logger.info(s"Robustintersection failed for g1: \r\n$g1")
      logger.info(s"Robustintersection failed for g2: \r\n$g2")
      val g1b = if (g1.getGeometryType == "LineString" || g1.getGeometryType == "MultiLineString")
        g1 else g1.buffer(-0.001).buffer(-0.001)
      g1b.intersection(g2.buffer(-0.001).buffer(-0.001))
    })
  }

  def robustUnion(g1: Geometry, g2: Geometry): Geometry = {
    Try {
      g1.buffer(0).union(g2.buffer(0))
    }.getOrElse({
      logger.info(s"robustUnion failed for g1: \r\n$g1")
      logger.info(s"robustUnion failed for g2: \r\n$g2")
      g1.buffer(-0.001).buffer(-0.001).union(g2.buffer(-0.001).buffer(-0.001))
    })
  }

  def robustDifference(g1: Geometry, g2: Geometry): Geometry = {
    Try {
      g1.buffer(0).difference(g2.buffer(0))
    }.getOrElse({
      logger.info(s"robustDiff failed for g1: \r\n$g1")
      logger.info(s"robustDiff failed for g2: \r\n$g2")
      g1.buffer(-0.001).buffer(-0.001).difference(g2.buffer(-0.001).buffer(-0.001))
    })
  }




  def geosAreNear(g: Geometry, p: Point): Boolean = p.isWithinDistance(g, coverageErrorMargin)

  def pointsInSamePolygon(p_startPoint: List[Float],
                          p_endPoint: List[Float],
                          machinedGeo: Geometry): Boolean = {
    val (startPoint, endPoint) = (asPoint(p_startPoint), asPoint(p_endPoint))
    val tempList = getGeoListFromGeo(machinedGeo).map(
      g => (startPoint.isWithinDistance(g,coverageErrorMargin), endPoint.isWithinDistance(g, coverageErrorMargin)))
    if(tempList.map(_._1).count(_ == true) > 1 || tempList.map(_._1).count(_ == true) > 1)
      logger.warn("pointsInSamePolygon: Points are covered by multiple Elements of the machined Geo (check Code)")
    tempList.map(t => t._1 == t._2).reduce(_ && _)
  }



  def rotateFloatList(l: List[List[List[Float]]], angle: Double): List[List[List[Float]]]= {
    val lsList = l.map(singlePath => getNewLineString(singlePath.map(asCoordinate).toArray))
    val transformMatrix = AffineTransformation.rotationInstance(angle)
    val rotatedLsList = lsList.map(transformMatrix.transform)
    rotatedLsList.map(ls => asFloatList(ls.getCoordinates))
  }

  def getLineStringFromBoundaries(l: List[Float]): LineString = {
    val doubleList = l.map(_.toDouble)
    val coords = List(
      new Coordinate(doubleList(0), doubleList(2)),
      new Coordinate(doubleList(1), doubleList(2)),
      new Coordinate(doubleList(1), doubleList(3)),
      new Coordinate(doubleList(0), doubleList(3)))
    getNewLineString(coords.toArray)
  }

  def getBoundaryListFromGeo(g: Geometry): List[Float] = {
    val env = g.getEnvelopeInternal
    List(env.getMinX, env.getMaxX, env.getMinY, env.getMaxY).map(_.toFloat)
  }

  def findHoles(g: Geometry): Option[Geometry] = {
    val resultGeo = g match {
      case p: Polygon => findHoles(p.asInstanceOf[Polygon])
      case mp: MultiPolygon => findHoles(mp.asInstanceOf[MultiPolygon])
      case _ => None
    }
    pGeo("resultGeo, Finding isles", resultGeo.getOrElse(emptyGeometry))
    resultGeo
  }

  def findHoles(g: Polygon): Option[Geometry] = {
    val polygons = (0 until g.getNumInteriorRing).map { i => gf.createPolygon(g.getInteriorRingN(i)) }
    val multiPolygon = gf.createMultiPolygon(polygons.toArray)
    if (multiPolygon.isEmpty)
      None
    else
      Some(multiPolygon)
  }

  def findHoles(g: MultiPolygon): Option[Geometry] = {
    val polygons = (0 until g.getNumGeometries).map { i => g.getGeometryN(i) }
    val geoOptions = polygons.map(findHoles)
    val resultGeo = geoOptions.filter { i =>
      i match {
        case Some(_) => true
        case None => false
      }
    }.map(_.get).reduceOption(
      _ union _
    )
    resultGeo
  }

  def getGeoCollection(l: List[Geometry]) = gf.createGeometryCollection(l.toArray)

  def withIdleZ(c: List[Float]): List[Float] =
    if (c.length == 2) c :+ zIdleValue else if (c.length == 3) c.dropRight(1) :+ zIdleValue else List.empty[Float]

  def repositionPathSteps(c1: List[Float], c2: List[Float]): List[List[Float]] = {
    val c1Connect: List[Float] = withIdleZ(c1)
    val c2Connect: List[Float] = withIdleZ(c2)
    List(c1Connect, c2Connect)
  }

  def saniPoly(s:Geometry, cfg: PathCoverageStepConfig): Geometry =
  {
    val buffer1 = cfg.bufferFct(cfg.bufferFct(s, -0.001), 0.001)
    buffer1.getGeometryType match {
      case "Polygon" => buffer1
      case "MultiPolygon" =>
        val geoList = getGeoListFromGeo(buffer1).filter(_.getArea > cfg.areaIgnoreVal).map(_.asInstanceOf[Polygon])
        gf.createMultiPolygon(geoList.toArray)
      case _ =>
        logger.error("should not happen")
        emptyGeometry
    }
  }

  def pointAlongByDistance(p1: List[Float], p2: List[Float], distance: Float): List[Float] = {
    val ls = new LineSegment(asCoordinate(p1), asCoordinate(p2))
    asFloatList3(Array(ls.pointAlong(distance / ls.getLength))).head
  }



  def filterGeoCollectionPolyOnly(g: Geometry) = g.getGeometryType match {
    case "Polygon" => g
    case "MultiPolygon" => g
    case "GeometryCollection" => val arr = getGeoListFromGeo(g).filter(_.getGeometryType == "Polygon").toArray
      gf.createGeometryCollection(arr)
    case _ => emptyGeometry // intersection: LS
  }

  def filterGeoCollectionLsOnly(g: Geometry): Option[LineString] = g.getGeometryType match {
    case "GeometryCollection" => val arr = getGeoListFromGeo(g).filter(_.getGeometryType == "LineString").toArray
      if (arr.isEmpty)
        None
      else
        Some(arr.maxBy(_.getLength).asInstanceOf[LineString])
    case _ => None // Should not happen
  }


  def circleUpperLineString(g: Geometry): LineString =
    if (g.getGeometryType != "Polygon" || g.isEmpty) {
      logger.warn(s"circleUpperLineString called for geometry type: ${g.getGeometryType}, empty: ${g.isEmpty}")
      gf.createLineString()
    } else {
      val env = g.getEnvelopeInternal
      val poly = getPolygonByCoordList(List(
        new Coordinate(env.getMinX, g.getCentroid.getY),
        new Coordinate(env.getMinX, env.getMaxY),
        new Coordinate(env.getMaxX, env.getMaxY),
        new Coordinate(env.getMaxX, g.getCentroid.getY),
      ))
      poly.intersection(g).asInstanceOf[Polygon].getExteriorRing
    }

  def safeCastToPolygon(g: Geometry): Polygon =
    if (g.getGeometryType == "Polygon") g.asInstanceOf[Polygon] else gf.createPolygon()

  def smartCastToPolygon(g: Geometry, g2: Geometry = emptyGeometry): Polygon =
    if (g.getGeometryType == "Polygon")
      g.asInstanceOf[Polygon]
    else if (g.getGeometryType == "MultiPolygon" || g.getGeometryType == "GeometryCollection") {
      val lGeoList = getGeoListFromGeo(g).filter(_.getGeometryType == "Polygon").filter(_.getArea > 0.1)
      if (lGeoList.nonEmpty)
        if (g2.isEmpty)
          lGeoList.maxBy(_.getArea).asInstanceOf[Polygon]
        else
          lGeoList.minBy(_.distance(g2)).asInstanceOf[Polygon]
      else
        gf.createPolygon()
    } else {
      gf.createPolygon()
    }


  def paraBuffer(minPointClearance: Double)(g: Geometry, d: Double): Geometry = {
    val lengthNinetyArc = twoPi / 4 * d
    val numberOfPointsOnArc = math.min(math.ceil(lengthNinetyArc / minPointClearance), 8.0d).toInt
    g.buffer(d, numberOfPointsOnArc)
    //g.buffer(d)
  }

  def pGeo(s: String, g: Geometry): Unit = logger.debug(s"$s\r\n$g")

  def asFloatList(a: Array[Coordinate]): List[List[Float]] = a.map(c => List(c.x.toFloat, c.y.toFloat)).toList
  def coordAsFloatList(a: Coordinate): List[Float] = List(a.x.toFloat, a.y.toFloat, a.z.toFloat)

  def asFloatList3(a: Array[Coordinate]): List[List[Float]] =
    a.map(c => List(c.x.toFloat, c.y.toFloat, c.getZ.toFloat)).toList

  def getBoundaryRectangle(l: List[Float]): Geometry = {
    if (l.length < 4) {
      logger.warn(s"getBoundaryRectangle. Argument list length should be 4 $l")
      return emptyGeometry
    }
    val gsf: GeometricShapeFactory = new GeometricShapeFactory()
    gsf.setBase(new Coordinate(l(0), l(2)))
    gsf.setWidth(l(1) - l(0))
    gsf.setHeight(l(3) - l(2))
    gsf.createRectangle()
  }

  def getNewLineString(a: Array[Coordinate]): LineString = gf.createLineString(a)

  def getNewLineString(a: Coordinate, b: Coordinate): LineString = getNewLineString(Array(a, b))

  def getNewLineString(a: Array[LineString]): LineString = {
    val coords: Option[Array[Coordinate]] = a.map(_.getCoordinates).reduceOption(_ ++ _)
    val coordArray: Option[Array[Coordinate]] = coords.map(i => i.distinct)
    //      coords.map(c => c.zip(c.tail).filterNot { case (a, b) => a.equals2D(b) }.map(_._1))
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

  def translateGeo(g: Geometry, deltaX: Double, deltaY: Double): Geometry = {
    val at = AffineTransformation.translationInstance(deltaX, deltaY)
    at.transform(g)
  }

  def filterLsAndReturnMultiPoly(g: Geometry): Geometry = {
    if (g.getNumGeometries != 1) {
      val l = getGeoListFromGeo(g).filter(_.getGeometryType == "Polygon")
      if (l.size == 1)
        l.head
      else if (l.size > 1)
        gf.createMultiPolygon(l.map(_.asInstanceOf[Polygon]).toArray)
      else
        emptyGeometry
    } else {
      g
    }
  }
  def toAffMatrix(p1: Coordinate, a: Double): AffineTransformation = {
    val rotMat = new AffineTransformation().rotate(a - Math.PI * 0.5)
    val transMat = new AffineTransformation().translate(p1.getX, p1.getY)
    val at = rotMat.compose(transMat)
    at
  }

  def multiGeoToGeoList(p: Geometry): List[Geometry] =
    ((0 until p.getNumGeometries) map (i => p.getGeometryN(i))).toList

  def asMultiLine(l: List[LineString]): MultiLineString =
    gf.createMultiLineString(l.toArray)

  def getLongestLineString(mls: Geometry, cfg: PathCoverageStepConfig): Geometry = {
    if (mls.isEmpty)
      emptyGeometry
    else
      mls.getGeometryType match {
        case "LineString" => mls
        case "MultiLineString" =>
          val dis = LineDissolver.dissolve(mls)
          getGeoListFromGeo(dis).maxBy(_.getLength)
        case "GeometryCollection" =>
          val lsOnly = getGeoListFromGeo(mls).filter(_.getGeometryType == "LineString").
            map(LineDissolver.dissolve).filter(_.getLength > cfg.pathIgnoreVal)
          lsOnly.maxBy(_.getLength)
      }
  }

  def getTopLineString(mls: Geometry, cfg: PathCoverageStepConfig): Geometry = {
    if (mls.isEmpty)
      emptyGeometry
    else
      mls.getGeometryType match {
        case "LineString" => mls
        case "MultiLineString" =>
          val dis = LineDissolver.dissolve(mls)
          getGeoListFromGeo(dis).maxBy(_.getEnvelopeInternal.getMaxY)
        case "GeometryCollection" =>
          val lsOnly = getGeoListFromGeo(mls).filter(_.getGeometryType == "LineString").
            map(LineDissolver.dissolve).filter(_.getLength > cfg.pathIgnoreVal)
          lsOnly.maxBy(_.getEnvelopeInternal.getMaxY)
      }
  }
  def asLineString(l: List[List[Float]]): Geometry = gf.createLineString(l.map(c => asCoordinate(c)).toArray)

  def asPoint(p: List[Float]) = gf.createPoint(asCoordinate(p))

  def distanceToPoint(g: Geometry, p: List[Float]): Double = {
    g.distance(asPoint(p))
  }
  def asCoordinate(a: List[Float]): Coordinate =
    if (a.size > 1)
      new Coordinate(a.head, a.tail.head) // a float list can also contain velocity...
    else emptyGeometry.getCoordinate

  def asCoordinateD(a: List[Double]): Coordinate =
    if (a.size > 1)
      new Coordinate(a.head, a.tail.head) // a can also contain velocity...
    else emptyGeometry.getCoordinate

  def toLineStringList(s: List[Coordinate]): List[LineString] = if (s.length > 1)
    s.zip(s.tail).map { case (a, b) => getNewLineString(Array(a, b)) }
  else
    List.empty[LineString]

  def reverseLs(ls: LineString): LineString = ls.reverse()

  def asLineString(g: Geometry): LineString =
    g match {
      case ls: LineString => ls
      //Fix for ExRing intersection results
      case mls: MultiLineString =>
        val ls = gf.createLineString(mls.getCoordinates)
        if (ls.getLength == mls.getLength)
          ls
        else
          emptyLs
      case _ => emptyLs
    }

  def getPolygonByLineStrings(ls1: List[Coordinate], ls2: List[Coordinate]): Polygon =
    new Polygon(gf.createLinearRing((ls1 ++ ls2).toArray), Array.empty[LinearRing], gf)

  def getPolygonByCoordList(l: List[Coordinate]): Polygon =
    new Polygon(gf.createLinearRing(l.toArray :+ l.head), Array.empty[LinearRing], gf)

  def getFirstExteriorFromPolygon(ls: Geometry): Geometry = {
    ls.getGeometryType match {
      case "Polygon" =>
        ls.asInstanceOf[Polygon].getExteriorRing
      case "MultiPolygon" =>
        logger.warn(s"getFirstInteriorFromPolygon called for Multipolygon $ls. Using first geometry.")
        ls.asInstanceOf[MultiPolygon].getGeometryN(0).asInstanceOf[Polygon].getExteriorRing
      case _ => emptyGeometry
    }
  }

  def getFirstGeometry(g: Geometry): Geometry = if (g.getNumGeometries > 0) g.getGeometryN(0) else emptyGeometry

  def getGeoListFromGeo(g: Geometry): List[Geometry] =
    if (g.getNumGeometries > 0)
      (0 until g.getNumGeometries).map(gIndex => g.getGeometryN(gIndex)).toList
    else
      List(g)

  def isPolygon(g: Geometry): Boolean = g.getGeometryType == "Polygon"

  def getLongestExteriorFromPolygon(g: Geometry): Option[LineString] = {
    g.getGeometryType match {
      case "Polygon" =>
        Some(g.asInstanceOf[Polygon].getExteriorRing)
      case "MultiPolygon" =>
        logger.warn(s"getFirstInteriorFromPolygon called for Multipolygon $g. Using first geometry.")
        val geoList = getGeoListFromGeo(g)
        val longestExGeoTuple = geoList.map(i => (i, getLongestExteriorFromPolygon(i))).maxBy(
          _._2.map(ls => ls.getLength))
        longestExGeoTuple._2
      case _ => None
    }
  }
  def getFirstInteriorFromPolygon(ls: Geometry): Geometry = {
    ls.getGeometryType match {
      case "Polygon" => if (ls.asInstanceOf[Polygon].getNumInteriorRing > 0)
        gf.createPolygon(ls.asInstanceOf[Polygon].getInteriorRingN(1).getCoordinates) //TODO Change back
      else
        emptyGeometry
      case "MultiPolygon" =>
        logger.warn(s"getFirstInteriorFromPolygon called for Multipolygon $ls. Using first geometry.")
        val poly = ls.asInstanceOf[MultiPolygon].getGeometryN(0).asInstanceOf[Polygon]

        if (poly.getNumInteriorRing > 0)
          gf.createPolygon(poly.getInteriorRingN(0).getCoordinates)
        else
          emptyGeometry

      case _ => emptyGeometry
    }
  }

//  def exportGeo(g1: Geometry, g2: Geometry, fileName: String): Unit = {
//    val pngWriter = new CustomPngWriter()
//    var tc = new TestCaseEdit(Array(g1,g2), "test123")
//    tc.setGeometry(0,g1)
//    tc.setGeometry(1,g2)
//    pngWriter.createPNGFile("test.png", g1,g2, 200,200)
//   // (File outputDirectory, TestCaseEdit testCase, PrecisionModel precisionModel)
//  }
}
