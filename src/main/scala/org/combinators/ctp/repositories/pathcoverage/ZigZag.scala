package org.combinators.ctp.repositories.pathcoverage

import com.typesafe.scalalogging.LazyLogging
import org.combinators.ctp.repositories.scene.SceneUtils
import org.locationtech.jts.algorithm.ConvexHull
import org.locationtech.jts.dissolve.LineDissolver
import org.locationtech.jts.geom.impl.CoordinateArraySequence
import org.locationtech.jts.geom.util.AffineTransformation
import org.locationtech.jts.geom.{Coordinate, Geometry, LineString, LinearRing, MultiLineString, MultiPolygon, Point, Polygon}
import org.locationtech.jts.math.Vector2D
import org.locationtech.jts.simplify.DouglasPeuckerSimplifier

trait ZigZag extends SceneUtils with LazyLogging with JtsUtils {
  // mit StepBased? Stepsize (flexibel, fest) Auf/Abrunden, letzter Schritt
  /*
  // Parameter: Gleichlauf, Gegenlauf,
   */
  val width: Float = 1.0f
  val depth: Float = 1.0f
  val toolDia: Float
  val a_e: Float
  val restGeo: Geometry
  val machinedGeo: Geometry
  val targetWorkpiece: Geometry
  val targetGeometry: Geometry
  val stepDirection: Vector2D
  lazy val feedDirection: Vector2D = stepDirection.rotateByQuarterCircle(1) //perpendicular to stepdir

  // frage: Anfang ende nur in free oder auch ohne? wahrscheinlich beides. Prio nur in free

  lazy val multiLines: Geometry = {
    //    val at = new AffineTransformation().setToTranslation(feedDirection.normalize().multiply(toolDia).getX.toFloat,
    //      feedDirection.normalize().multiply(toolDia).getY.toFloat)
    //
    //    val at2 = new AffineTransformation().setToTranslation(-feedDirection.normalize().multiply(toolDia).getX.toFloat,
    //      -feedDirection.normalize().multiply(toolDia).getY.toFloat)

    val normalizeRotMat = new AffineTransformation().setToRotation(-stepDirection.angle())
    logger.info(s"ZigZag Restgeo: $restGeo")

    val polyAndBoundaryAreas = restGeo.buffer(toolDia)
    //.transform(p1).union(at2.transform(p1))
    logger.info(s"polyAndBoundaryAreas (restGeo.buffer(toolDia)): $polyAndBoundaryAreas")

    logger.info(s"ZigZag machinedGeo: $machinedGeo")

    val polyAndBoundaryAreasIntersectionOhneBuffer = polyAndBoundaryAreas.intersection(machinedGeo)
    logger.info(s"polyAndBoundaryAreasIntersectionOhneBuffer: $polyAndBoundaryAreasIntersectionOhneBuffer")

    val polyAndBoundaryAreasIntersection = polyAndBoundaryAreas.intersection(machinedGeo).buffer(-0.001).buffer(0.001)
    logger.info(s"polyAndBoundaryAreasIntersection (polyAndBoundaryAreas.intersection(machinedGeo)): $polyAndBoundaryAreasIntersection")

    val polyAndBoundaryAreasIntersectionUnion = restGeo.buffer(0.001).union(polyAndBoundaryAreasIntersection.buffer(0.001d))
    logger.info(s"polyAndBoundaryAreasIntersectionUnion: $polyAndBoundaryAreasIntersectionUnion")

    //bisher gleich
    val polyAndBoundaryAreasIntersectionUnionSmol = polyAndBoundaryAreasIntersectionUnion.buffer(-0.001)
    logger.info(s"polyAndBoundaryAreasIntersectionSmol: $polyAndBoundaryAreasIntersectionUnionSmol")

    val normalizedArea = normalizeRotMat.transform(polyAndBoundaryAreasIntersectionUnionSmol)
    logger.info(s"normalizedArea: $polyAndBoundaryAreasIntersectionUnionSmol")

    // take 3 points Seq with x<x1<x2, take convex hull, intersect with poly if linestring then all good

    val c = normalizedArea.getGeometryType match {
      case "Polygon" => {
        logger.info(s"zipzag coord extraction Polygon")
        val a = normalizedArea.asInstanceOf[Polygon]
        val exRing = a.getExteriorRing
        logger.info(s"zipzag coord extraction Exring: $exRing")
        a.getExteriorRing.getCoordinates
      }
      case "MultiPolygon" => {
        logger.info(s"zipzag coord extraction MultiPolygon")
        val a = normalizedArea.asInstanceOf[MultiPolygon]
        a.getCoordinates
      }
    }

    val xSelectedNonConvexPointTriple = if (c.length >= 3) {
      (c.zip(c.tail).zip(c.tail.tail)).
        filter {
          case ((a, b), c) => a.x > b.x && b.x <= c.x
        }.filter { case ((a, b), c) =>
        restGeo.covers(new Point(new CoordinateArraySequence(Array(a)), gf)) &&
          restGeo.covers(new Point(new CoordinateArraySequence(Array(b)), gf)) &&
          restGeo.covers(new Point(new CoordinateArraySequence(Array(c)), gf))
      }.
        filter {
          case ((a, b), c) => {
            val ch = new ConvexHull(Array(a, b, c), gf)
            restGeo.contains(ch.getConvexHull)
          }
        }
    }
    else {
      Array.empty[((Coordinate, Coordinate), Coordinate)]
    }

    logger.info(
      s"""selectedPoint:
         |${if (xSelectedNonConvexPointTriple.nonEmpty) xSelectedNonConvexPointTriple.min else "empty"}""".stripMargin)

    val envelope = normalizedArea.getEnvelopeInternal
    val xIntervalLength =
      if (xSelectedNonConvexPointTriple.isEmpty)
        envelope.getMaxX - envelope.getMinX
      else
        xSelectedNonConvexPointTriple.min._1._2.x - envelope.getMinX
    val segmentCount = math.ceil(xIntervalLength / a_e).toInt
    val stepDistance = xIntervalLength / segmentCount
    val lines = (1 to segmentCount).map(i => {
      val xVal = normalizedArea.getEnvelopeInternal.getMinX + stepDistance * i
      val maxY = envelope.getMaxY
      val minY = envelope.getMinY
      new LineString(new CoordinateArraySequence(Array(new Coordinate(xVal, minY), new Coordinate(xVal, maxY))), gf)
    })

//    val linesIntersectionWithNormalizedArea = lines.map(i => i.intersection(normalizedArea))

    val multiLinesList = lines.map(i => i.intersection(normalizedArea)).map(i => asLineString(i)).takeWhile(ls => !ls.isEmpty).toArray

//    val foo = lines.map(i => i.intersection(normalizedArea)).filter(g => g.getGeometryType == "MultiLineString").map(i => {
//      val mls = i.asInstanceOf[MultiLineString]
//      val geoList = (0 until mls.getNumGeometries).map(g => mls.getGeometryN(g)).filter(geo => geo.getLength > a_e * 2)
//      geoList.headOption.map(i => i.asInstanceOf[LineString])
//    })

//    val multiLines = foo.filter(i => i match {
//      case Some(g) => true
//      case _ => false
//    }).map(i => i.get)
    //    val multiLines = normalizedArea.intersection(new MultiLineString(lines.toArray, gf))
    //MultiLineStrings

    val multiLineGeometry = gf.createMultiLineString(multiLinesList)
    logger.info(s"multiLineGeometry: ${multiLineGeometry}")

    pGeo("initialScene.getMachinedMultiGeo", machinedGeo)
    val invalidToolPositions = targetWorkpiece.buffer(toolDia/2.0d) // ???
    pGeo("invalidToolPositions", invalidToolPositions)

    // val validPos = invalidToolPositions.asInstanceOf[Polygon].getInteriorRingN(0)

    val validPosPoly = targetGeometry.buffer(-toolDia/2.0d)
    //      getFirstInteriorFromPolygon(invalidToolPositions)
    pGeo("validPosPoly", validPosPoly)
    val resultMultiLines1 = validPosPoly.intersection(multiLineGeometry)
    pGeo("resultMultiLines1", resultMultiLines1)

    val resultMultiLines: Geometry =
    new AffineTransformation().setToRotation(stepDirection.angle()).transform(resultMultiLines1)
    logger.info(s"resultMultiLines.buffer(toolDia): ${resultMultiLines.buffer(toolDia / 2.0)}")

    val rawLineList = (0 until resultMultiLines.getNumGeometries).map { i =>
      val asd = resultMultiLines.getGeometryN(i)
      logger.info(s"temp: $asd")
      val asd2 = if (asd.getGeometryType == "Point") asd else LineDissolver.dissolve(asd)
      logger.info(s"asd2: $asd2")

      DouglasPeuckerSimplifier.simplify(
        asd2, 0.001)
    }.filter(_.getGeometryType != "Point")

    val filteredLineList =
      if (rawLineList.length <= 1) -1
    else
      rawLineList.zip(rawLineList.tail).indexWhere {
        case (g1, g2) => {
          val ch = new ConvexHull((g1.getCoordinates ++ g2.getCoordinates).toArray, gf)
          logger.info(s"ch: ${ch.getConvexHull}")
          logger.info(s"intersection: ${ch.getConvexHull.buffer(-0.01).intersection(normalizedArea)}")
          val notContains =
            ch.getConvexHull.buffer(-0.01).getArea -
              ch.getConvexHull.buffer(-0.01).intersection(normalizedArea).getArea > ch.getConvexHull.buffer(-0.01).getArea * 0.1
          logger.info(s"notContains: ${notContains}")
          notContains
        }
      }

    logger.info(s"filteredLineList $filteredLineList")
    if (filteredLineList == -1) {
      rawLineList.reduceOption[Geometry](_ union _).getOrElse(emptyGeometry)
    } else {
      (0 to filteredLineList).map(i => rawLineList(i)).reduceOption[Geometry](_ union _).getOrElse(emptyGeometry)
    }
  }

  lazy val findPoints: List[List[Float]] = {
    val resultMultiLines = multiLines
    logger.info(s"resultMultiLines: ${resultMultiLines}")
    //    val reversedMultiLines: List[Array[Coordinate]] = (0 until resultMultiLines.getNumGeometries).map(i =>
    //      resultMultiLines.getGeometryN(i).getCoordinates :+ resultMultiLines.getGeometryN(i).getCoordinates.head).toList
    //TODO Does not work for non trivial cases
    val reversedMultiLines2: List[Array[Coordinate]] = (0 until resultMultiLines.getNumGeometries).map(i =>
      if (i % 2 == 0)
        resultMultiLines.getGeometryN(i).getCoordinates.sortBy(_.y)
      else resultMultiLines.getGeometryN(i).getCoordinates.sortBy(_.y).reverse).toList

    val rPath: List[List[Float]] =
      if (reversedMultiLines2.nonEmpty)
        reversedMultiLines2.reduce(_ ++ _).map(c => List(c.x.toFloat, c.y.toFloat)).toList
      else
        List.empty[List[Float]]
    rPath
  }

  val directionVector: List[Float] = List(1.0f, 0.0f) // in positive x direction

}

object runZigZag extends App with LazyLogging with JtsUtils {
  val pArray = Array(new Coordinate(0.0d, 0.0d, 0.0d),
    new Coordinate(0.0d, 10.0d, 0.0d),
    new Coordinate(50.0d, 50.0d, 0.0d),
    new Coordinate(50.0d, 0.0d, 0.0d),
    new Coordinate(0.0d, 0.0d, 0.0d))
  val poly1: Polygon = new Polygon(
    new LinearRing(new CoordinateArraySequence(pArray), gf), Array.empty[LinearRing], gf)
  val zz = new ZigZag {
    override val toolDia: Float = 5f
    override val a_e: Float = 3f
    override val restGeo: Polygon = poly1
    override val machinedGeo: MultiPolygon = new MultiPolygon(Array(poly1.buffer(6f).asInstanceOf[Polygon]), gf)
    override val stepDirection: Vector2D = new Vector2D(1.0f, 0.0f)
    override val targetWorkpiece: Geometry = poly1
    override val targetGeometry: Geometry = poly1
  }

  val p = zz.findPoints.map(i => i :+ 0.0f) // z-Coord
  println(p)
  val ls = getNewLineString(p.map(asCoordinate).toArray)
  println(s"ls: \r\n$ls")
  //ToAkka(p)
}