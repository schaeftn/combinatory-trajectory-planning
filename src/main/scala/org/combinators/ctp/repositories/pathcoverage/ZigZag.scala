package org.combinators.ctp.repositories.pathcoverage

import com.typesafe.scalalogging.LazyLogging
import org.combinators.ctp.repositories.scene.SceneUtils
import org.locationtech.jts.algorithm.ConvexHull
import org.locationtech.jts.dissolve.LineDissolver
import org.locationtech.jts.geom.impl.CoordinateArraySequence
import org.locationtech.jts.geom.util.AffineTransformation
import org.locationtech.jts.geom.{Coordinate, Geometry, LineSegment, LineString, LinearRing, MultiLineString, MultiPolygon, Point, Polygon}
import org.locationtech.jts.io.WKTReader
import org.locationtech.jts.math.Vector2D
import org.locationtech.jts.simplify.{DouglasPeuckerSimplifier, TaggedLineStringSimplifier, TaggedLinesSimplifier, VWLineSimplifier, VWSimplifier}


//TODO Must be able to handle open pockets
trait ZigZag extends SceneUtils with LazyLogging with JtsUtils {
  // mit StepBased? Stepsize (flexibel, fest) Auf/Abrunden, letzter Schritt
  /*
  // Parameter: Gleichlauf, Gegenlauf,
   */
  val steelSpecific: Boolean = false
  lazy val zagOnly: Boolean = steelSpecific
  lazy val startOutside = steelSpecific
  val width: Float = 1.0f
  val depth: Float = 1.0f
  val toolDia: Float
  val a_e: Float
  val convexDetection: Boolean = false
  /**
   * Geo to be processed
   */
  val restGeo: Geometry
  val machinedGeo: Geometry
  /**
   * desirec workpiece, target machines subtracted from base area
   */
  val targetWorkpiece: Geometry
  /**
   * targeted machined area
   */
  val targetGeometry: Geometry // targetGeometry as defined in 2dModel
  val stepDirection: Vector2D
  lazy val feedDirection: Vector2D = stepDirection.rotateByQuarterCircle(1) //perpendicular to stepdir

  // frage: Anfang ende nur in free oder auch ohne? wahrscheinlich beides. Prio nur in free

  lazy val normalizeRotMat = new AffineTransformation().setToRotation(-stepDirection.angle())
  lazy val polyAndBoundaryAreas = restGeo.buffer(toolDia)
  lazy val polyAndBoundaryAreasIntersectionOhneBuffer = polyAndBoundaryAreas.intersection(machinedGeo)
  lazy val polyAndBoundaryAreasIntersection = polyAndBoundaryAreas.
    intersection(machinedGeo).buffer(-0.001).buffer(0.001)
  lazy val polyAndBoundaryAreasIntersectionUnion = restGeo.buffer(0.001).
    union(polyAndBoundaryAreasIntersection.buffer(0.001d))

  lazy val polyAndBoundaryAreasIntersectionUnionSmol = polyAndBoundaryAreasIntersectionUnion.buffer(-0.001)

  lazy val invalidToolPositions = targetWorkpiece.buffer(toolDia / 2.0d)

  lazy val normalizedArea = normalizeRotMat.transform(polyAndBoundaryAreasIntersectionUnionSmol)

  lazy val multiLines: Geometry = {
    logger.debug(s"ZigZag Restgeo: $restGeo")
    logger.debug(s"Target Geo: \r\n$targetGeometry")
    logger.debug(s"polyAndBoundaryAreas (restGeo.buffer(toolDia)): $polyAndBoundaryAreas")
    logger.debug(s"ZigZag machinedGeo: $machinedGeo")
    logger.debug(s"polyAndBoundaryAreasIntersectionOhneBuffer: $polyAndBoundaryAreasIntersectionOhneBuffer")
    logger.debug(s"polyAndBoundaryAreasIntersection (polyAndBoundaryAreas.intersection(machinedGeo)): " +
      s"$polyAndBoundaryAreasIntersection")
    logger.debug(s"polyAndBoundaryAreasIntersectionUnion: $polyAndBoundaryAreasIntersectionUnion")
    logger.debug(s"polyAndBoundaryAreasIntersectionSmol: $polyAndBoundaryAreasIntersectionUnionSmol")
    logger.debug(s"normalizedArea: $normalizedArea")
    pGeo("invalidToolPositions", invalidToolPositions)


    val validToolPositions = restGeo.buffer(0).difference(targetWorkpiece.buffer(toolDia / 2.0d))
    pGeo("validStartingPoints", validToolPositions)

    //    val at = new AffineTransformation().setToTranslation(feedDirection.normalize().multiply(toolDia).getX.toFloat,
    //      feedDirection.normalize().multiply(toolDia).getY.toFloat)
    //
    //    val at2 = new AffineTransformation().setToTranslation(-feedDirection.normalize().multiply(toolDia).getX.toFloat,
    //      -feedDirection.normalize().multiply(toolDia).getY.toFloat)



    // take 3 points Seq with x<x1<x2, take convex hull, intersect with poly if linestring then all good

    val c = validToolPositions.getGeometryType match {
      case "Polygon" => {
        logger.debug(s"zipzag coord extraction Polygon")
        val a = validToolPositions.asInstanceOf[Polygon]
        val exRing = a.getExteriorRing
        logger.debug(s"zipzag coord extraction Exring: $exRing")
        a.getExteriorRing.getCoordinates
      }
      case "MultiPolygon" => {
        logger.debug(s"zipzag coord extraction MultiPolygon")
        val a = validToolPositions.asInstanceOf[MultiPolygon]
        a.getCoordinates
      }
    }

    // Fix for small values
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

    logger.debug(
      s"""selectedPoint:
         |${if (xSelectedNonConvexPointTriple.nonEmpty) xSelectedNonConvexPointTriple.min else "empty"}""".stripMargin)

    val env = restGeo.getEnvelopeInternal
    val (envelopegetMaxX, envelopeMinX) = (env.getMaxX, env.getMinX)
    val envelopeNormalized = validToolPositions.getEnvelopeInternal
    val (envelopeNMaxX, envelopeNMinX) = (envelopeNormalized.getMaxX, envelopeNormalized.getMinX)

    val xIntervalLength =
      if (xSelectedNonConvexPointTriple.nonEmpty && convexDetection)
        xSelectedNonConvexPointTriple.min._1._2.x - envelopeMinX
    else
        envelopegetMaxX - envelopeMinX
    logger.debug(s"sIntervalLength: $xIntervalLength")
    val segmentCount = math.ceil(xIntervalLength / a_e).toInt
    val stepDistance = xIntervalLength / segmentCount
    val lines = (0 to segmentCount + 1).map(i => {
      val xVal = envelopeNMinX + (stepDistance * i).toFloat
      val maxY = envelopeNormalized.getMaxY
      val minY = envelopeNormalized.getMinY
      new LineString(new CoordinateArraySequence(Array(new Coordinate(xVal, minY), new Coordinate(xVal, maxY))), gf)
    })

    pGeo("ls list to geoCollection: ", getGeoCollection(lines.toList))


//    val lsIntersect = lines.map(i =>
//      i.intersection(normalizedArea)).map(i => asLineString(i))

// Line list might contain artefacts that cause the intersection to fail
    val lsIntersect = lines.takeWhile(_.getLength > 0.1).drop(1).map(i =>
        i.difference(invalidToolPositions.buffer(0))).foldLeft(List.empty[LineString]){
      case (lsList, newGeo) => newGeo match {
        case ls:LineString => lsList :+ ls
        case mls: MultiLineString => lsList :+ getGeoListFromGeo(mls).map(_.asInstanceOf[LineString]).minBy(
          geo => lsList.lastOption.map(geo.distance(_)).getOrElse(geo.getCoordinates.map(_.y).min))
        case p: Point => lsList
      }
    }

    pGeo("lsIntersections list to geoCollection: ", getGeoCollection(lsIntersect.toList))

    val multiLinesList = lsIntersect.takeWhile(ls => !ls.isEmpty).toArray
    pGeo("MultiLinesList: ", getGeoCollection(multiLinesList.toList))

    val multiLineGeometryA = gf.createMultiLineString(multiLinesList)
    pGeo("multiLineGeometry", multiLineGeometryA)

    val multiLineGeometry = VWSimplifier.simplify(multiLineGeometryA, 0.01)
    pGeo(s"multiLineGeometry simplified", multiLineGeometryA)

    pGeo("targetWorkpiece", targetWorkpiece)
    pGeo("initialScene.getMachinedMultiGeo", machinedGeo)


    val validStartingPoints = if (startOutside)
      restGeo.buffer(toolDia / 2.0)
    else restGeo.buffer(0).difference(targetWorkpiece.buffer(toolDia / 2.0d))

    pGeo("validStartingPoints", validStartingPoints)

    val resultMultiLines1 = asMultiLine(getGeoListFromGeo(multiLineGeometry.difference(invalidToolPositions.buffer(0))).
      sortBy(_.getEnvelopeInternal.getMinX).map(_.asInstanceOf[LineString]).map(ls =>
      gf.createLineString(ls.getCoordinates.sortBy(_.y)))) // more rubust?
//    val resultMultiLines1 = asMultiLine(getGeoListFromGeo(validStartingPoints.intersection(multiLineGeometry)).
//      sortBy(_.getEnvelopeInternal.getMinX).map(_.asInstanceOf[LineString]).map(ls => gf.createLineString(ls.getCoordinates.sortBy(_.y)) )) // more rubust?

    val resultMultiLines = LineDissolver.dissolve(resultMultiLines1)
    pGeo("resultMultiLines", resultMultiLines)

    val rMultiLinesDirty = resultMultiLines.getCoordinates.groupBy(_.x).map{
      case (xCoord, cArray) => getNewLineString(cArray.sortBy(_.y))
    }

    //    val resultMultiLines: Geometry =
    //      new AffineTransformation().setToRotation(stepDirection.angle()).transform(resultMultiLines1)
    pGeo("resultMultiLines.buffer(toolDia)", resultMultiLines.buffer(toolDia / 2.0))

    val rawLineList = (0 until resultMultiLines.getNumGeometries).map { i =>
      val asd = resultMultiLines.getGeometryN(i)
      //logger.debug(s"temp: $asd")
      val asd2 = if (asd.getGeometryType == "Point") asd else LineDissolver.dissolve(asd)
      // logger.debug(s"asd2: $asd2")

      DouglasPeuckerSimplifier.simplify(
        asd2, 0.001)
    }.filter(_.getGeometryType != "Point").sortBy(g => g.getEnvelopeInternal.getMinX)

    val filteredLineList =
      if (rawLineList.length <= 1) -1
      else
        rawLineList.zip(rawLineList.tail).indexWhere {
          case (g1, g2) => {
            val ch = new ConvexHull((g1.getCoordinates ++ g2.getCoordinates).toArray, gf)
            //logger.debug(s"ch: ${ch.getConvexHull}")
            //logger.debug(s"intersection: ${ch.getConvexHull.buffer(-0.01).intersection(normalizedArea)}")
            val notContains =
            ch.getConvexHull.buffer(-0.01).getArea -
              ch.getConvexHull.buffer(-0.01).intersection(normalizedArea).getArea > ch.getConvexHull.buffer(-0.01).getArea * 0.1
            //logger.debug(s"notContains: ${notContains}")
            notContains
          }
        }

    logger.debug(s"filteredLineList $filteredLineList")
    //TODO hack
    val oldResult = if (filteredLineList == -1) {
      rawLineList.reduceOption[Geometry](_ union _).getOrElse(emptyGeometry)
    } else {
      (0 to filteredLineList).map(i => rawLineList(i)).reduceOption[Geometry](_ union _).getOrElse(emptyGeometry)
    }

    pGeo("rMultiLinesDirty", asMultiLine(rMultiLinesDirty.toList))
    rMultiLinesDirty.toList.sortBy(_.getCoordinate.x).reduceOption[Geometry](_ union _).getOrElse(emptyGeometry)
  }

  lazy val findPoints: List[List[Float]] = {
    val resultMultiLines:Geometry = multiLines
    logger.debug(s"resultMultiLines val after eval: ${resultMultiLines}")
    val reversedMultiLines: List[Array[Coordinate]] =
      if (zagOnly) {
        (0 until resultMultiLines.getNumGeometries).map(i => {
          val srted = resultMultiLines.getGeometryN(i).getCoordinates.sortBy(_.y)
          if (srted.nonEmpty) srted :+ srted.head
          else Array.empty[Coordinate]
        }
        ).toList
      }
      else {
        (0 until resultMultiLines.getNumGeometries).map(i =>
          if (i % 2 == 0)
            resultMultiLines.getGeometryN(i).getCoordinates.sortBy(_.y)
          else resultMultiLines.getGeometryN(i).getCoordinates.sortBy(_.y).reverse).toList
      }

    val rPath: List[List[Float]] =
      if (reversedMultiLines.nonEmpty){ //TODO FoldLeft ggf array ergänzen
        // check connxLine
        val reduceFct: (Array[Coordinate], Array[Coordinate]) => Array[Coordinate] = {
          case (a: Array[Coordinate], b: Array[Coordinate]) =>
            val bufferedStraightConnector = getNewLineString(a.last, b.head).buffer(toolDia.toDouble / 2.0d)
            pGeo("bufferedStraightConnector",bufferedStraightConnector)
            val connectorInvalid = !bufferedStraightConnector.coveredBy(normalizedArea)
            pGeo("normalizedArea",normalizedArea)
            logger.debug(s"connectorInvalid: $connectorInvalid")

            if (connectorInvalid) {
              val connectTop:Boolean = b.head.y > b(1).y
              val ls1 = new LineSegment(a.last, a.dropRight(1).last)
              val ls2 = new LineSegment(b.head, b(1))
              if(ls1.maxX() == ls2.maxX())
                logger.warn("should not happen")
              if(ls1.maxX() != ls1.minX())
                logger.warn("should not happen")

              pGeo("ls1", ls1.toGeometry(gf))
              pGeo("ls2", ls2.toGeometry(gf))
              val projectionOption  = Option(ls2.project(ls1))
              projectionOption match {
                case None =>
                  logger.debug("empty projection")
                  a
                case Some(projection) =>
                  pGeo("projection", projection.toGeometry(gf))

                  val yValsTopDown = (0 to 10).map { i => projection.pointAlong(i / 10.0) }
                  val yVals = if(connectTop) yValsTopDown else yValsTopDown.reverse

                  pGeo("yVals point list", getGeoCollection(yVals.map(gf.createPoint).toList))
                  val horizontalSegments = yVals.map(newCoord =>
                    new LineSegment(new Coordinate(ls1.maxX(), newCoord.y), newCoord).
                      toGeometry(gf))
                  pGeo("horizontalLines", getGeoCollection(horizontalSegments.toList))
                  horizontalSegments.foreach(ls =>
                    assert(ls.getLength>0.01)
                  )
                  val selectedSegment = horizontalSegments.find(ls => ls.buffer(toolDia.toDouble / 2.0d).coveredBy(normalizedArea))
                  selectedSegment.foreach(pGeo("selectedSegment",_))

                  val selectedSegmentAsCoords = selectedSegment.map(_.getCoordinates)

                  def buildCoordList(newLs: Array[Coordinate]): Array[Coordinate] = {
                    val midCoords: Array[Coordinate] = (if (a.last == newLs.head)
                      newLs.tail else if (b.head == newLs.last) Array(newLs.head) else newLs)
                    (a ++ midCoords) ++ b
                  }

                  val resultCoordArrayOption: Option[Array[Coordinate]] = selectedSegmentAsCoords.map(buildCoordList)
                  // Do not add b if no valid segment was found
                  val resultCoordArray = resultCoordArrayOption.getOrElse(a)
                  resultCoordArray
              }
            }
            else {
              a ++ b
            }
        }

        val revLineString = getNewLineString(reversedMultiLines.reduce(reduceFct))
        pGeo("reversedMultiLines", revLineString)
        pGeo("reversedMultiLines buffered", revLineString.buffer(toolDia/2.0f))
        reversedMultiLines.reduce(reduceFct).map(c => List(c.x.toFloat, c.y.toFloat)).toList
      }
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
  val tgtGeo = new WKTReader()
  //  val tGeo = tgtGeo.read("""POLYGON ((-16 62, 69 62, 69 -12, -16 -12, -16 62),
  //                           |  (0 -5, 50 -5, 50.97545161008064 -4.903926402016152, 51.91341716182545 -4.619397662556434, 52.777851165098014 -4.157348061512726, 53.53553390593274 -3.5355339059327373, 54.15734806151273 -2.777851165098011, 54.61939766255644 -1.913417161825449, 54.903926402016154 -0.9754516100806412, 55 0, 55 50, 54.896178702647276 51.01362424583435, 54.58902635530269 51.985154177976426, 54.09129854004077 52.87424359723742, 53.4236651558651 53.64396993682922, 52.61385202841614 54.26236760187866, 51.69548949749589 54.70375545323959, 50.70671579886446 54.94980330716639, 49.688593239793924 54.99029316069687, 48.68340294226299 54.82354353018175, 47.73288897058207 54.456479280810306, 46.87652476222788 53.904344047215154, -3.1234752377721215 13.904344047215151, -3.7694139873094845 13.285044625614029, -4.2946155960944905 12.560522775095343, -4.682257464970686 11.753985470785402, -4.919923135825062 10.891266703951901, -5 10, -5 0, -4.903926402016152 -0.9754516100806419, -4.619397662556434 -1.9134171618254483, -4.157348061512727 -2.77785116509801, -3.5355339059327386 -3.5355339059327373, -2.777851165098011 -4.157348061512726, -1.9134171618254516 -4.619397662556432, -0.9754516100806433 -4.903926402016151, 0 -5))""".stripMargin)
  val tGeo = tgtGeo.read("""POLYGON ((0 -20, -3.901806440322573 -19.615705608064605, -7.6536686473018065 -18.47759065022573, -11.111404660392044 -16.629392246050905, -14.142135623730955 -14.14213562373095, -16.62939224605091 -11.11140466039204, -18.477590650225736 -7.653668647301793, -19.61570560806461 -3.9018064403225674, -20 0, -20 10, -19.67969254330025 13.565066815807606, -18.729029859882743 17.015941883141608, -17.178462384377962 20.242091100381376, -15.077655949237938 23.140178502456116, -12.493900951088486 25.617376188860607, 37.506099048911516 65.6173761888606, 40.93155588232824 67.82591712324123, 44.73361176905193 69.294174120727, 48.75437295917568 69.96117264278747, 52.82686319545783 69.79921322866558, 56.781957989983546 68.81502181295835, 60.45540811366455 67.04947040751465, 63.6946606234604 64.5758797473169, 66.36519416016307 61.49697438894967, 68.35610542121078 57.9406167119057, 69.58471481058909 54.054496983337415, 70 50, 70 0, 69.61570560806462 -3.9018064403225647, 68.47759065022574 -7.653668647301796, 66.62939224605091 -11.111404660392044, 64.14213562373095 -14.14213562373095, 61.11140466039205 -16.629392246050905, 57.6536686473018 -18.477590650225736, 53.90180644032257 -19.61570560806461, 50 -20, 0 -20))""")
  val tGeo2 = tgtGeo.read("""POLYGON ((-25 -25, -25 75, 80 75, 80 -25, -25 -25),
                            |  (0 -20, 50 -20, 53.90180644032257 -19.61570560806461, 57.6536686473018 -18.477590650225736, 61.11140466039205 -16.629392246050905, 64.14213562373095 -14.14213562373095, 66.62939224605091 -11.111404660392044, 68.47759065022574 -7.653668647301796, 69.61570560806462 -3.9018064403225647, 70 0, 70 50, 69.58471481058909 54.054496983337415, 68.35610542121078 57.9406167119057, 66.36519416016307 61.49697438894967, 63.6946606234604 64.5758797473169, 60.45540811366455 67.04947040751465, 56.781957989983546 68.81502181295835, 52.82686319545783 69.79921322866558, 48.75437295917568 69.96117264278747, 44.73361176905193 69.294174120727, 40.93155588232824 67.82591712324123, 37.506099048911516 65.6173761888606, -12.493900951088486 25.617376188860607, -15.077655949237938 23.140178502456116, -17.178462384377962 20.242091100381376, -18.729029859882743 17.015941883141608, -19.67969254330025 13.565066815807606, -20 10, -20 0, -19.61570560806461 -3.9018064403225674, -18.477590650225736 -7.653668647301793, -16.62939224605091 -11.11140466039204, -14.142135623730955 -14.14213562373095, -11.111404660392044 -16.629392246050905, -7.6536686473018065 -18.47759065022573, -3.901806440322573 -19.615705608064605, 0 -20))""".stripMargin)
 val tGeoStrictBounds = tgtGeo.read("""POLYGON ((56 -5, -7 -5, -7 59, 56 59, 56 -5),
                                      |  (0 0, 50 0, 50 50, 0 10, 0 0))""".stripMargin)
  val poly1: Polygon = new Polygon(
    new LinearRing(new CoordinateArraySequence(pArray), gf), Array.empty[LinearRing], gf)
  val zz = new ZigZag {
    override val steelSpecific = true
    override val toolDia: Float = 5f
    override val a_e: Float = 2.5f
    override val restGeo: Polygon = poly1
    override val machinedGeo: MultiPolygon = new MultiPolygon(Array(poly1.buffer(6f).asInstanceOf[Polygon]), gf)
    override val stepDirection: Vector2D = new Vector2D(1.0f, 0.0f)
    override val targetWorkpiece: Geometry = tGeoStrictBounds
    override val targetGeometry: Geometry = tGeo
  }

  val p = zz.findPoints.map(i => i :+ 0.0f) // z-Coord
  logger.debug(s"$p")
  val ls = getNewLineString(p.map(asCoordinate).toArray)
  logger.debug(s"ls: \r\n$ls")
  //ToAkka(p)
}