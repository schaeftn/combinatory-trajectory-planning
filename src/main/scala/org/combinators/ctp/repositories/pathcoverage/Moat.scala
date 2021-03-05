package org.combinators.ctp.repositories.pathcoverage

import com.typesafe.scalalogging.LazyLogging
import org.combinators.ctp.repositories.pathcoverage.runZigZag.{asCoordinate, getNewLineString, gf, p}
import org.combinators.ctp.repositories.toplevel.PathCoverageStepConfig
import org.locationtech.jts.algorithm.Angle
import org.locationtech.jts.geom.impl.CoordinateArraySequence
import org.locationtech.jts.geom.{Coordinate, Geometry, LineString, LinearRing, MultiPolygon, Point, Polygon}
import org.locationtech.jts.dissolve.LineDissolver
import org.locationtech.jts.io.WKTReader
import org.locationtech.jts.math.Vector2D
import org.locationtech.jts.simplify.DouglasPeuckerSimplifier

import scala.annotation.tailrec

trait Moat extends GeoUtils with LazyLogging with JtsUtils {
  val tool: CncTool
  lazy val ae: Double = tool.ae
  lazy val circMotionRadius: Double = tool.d / 2.0d

  val config: PathCoverageStepConfig

  val p1: Polygon
  lazy val p2 = p1.buffer(-tool.d)
  lazy val p1diffToolRadius = p1.buffer(tool.d / 2.0)


  lazy val getLines: List[LineString] =
    if (p2.isEmpty)
      List.empty[LineString]
    else {
      logger.info(s"p2 geo type ${p2.getGeometryType}")
      // logger.info(s"p2 cast ${p2.asInstanceOf[Polygon].getExteriorRing}")

      val nnn = p2.getGeometryType match {
        case "MultiPolygon" =>
          LineDissolver.dissolve(p2.asInstanceOf[MultiPolygon].getGeometryN(0).asInstanceOf[Polygon].getExteriorRing)
        case "Polygon" => LineDissolver.dissolve(p2.asInstanceOf[Polygon].getExteriorRing)
        case a => logger.info(s"Unhandled geometry type: $a")
          new Point(null, gf)
      }

      val nnn1 = DouglasPeuckerSimplifier.simplify(nnn, 0.0001)
      val asd = toLineStringList(p2.getCoordinates.toList)

      logger.info(s"current ls: $asd")
      logger.info(s"new dissolved ls: $nnn")
      logger.info(s"new dissolved simplified ls: $nnn1")

//      (nnn1.getCoordinates zip nnn1.getCoordinates.tail) map {
//        case (a, b) => new LineString(new CoordinateArraySequence(Array(a, b)), gf)
//      }
      toLineStringList(nnn1.getCoordinates.toList)
    }
  lazy val minY: Double = p2.getEnvelopeInternal.getMinY
  lazy val maxY: Double = p2.getEnvelopeInternal.getMaxY

  lazy val selectLines: Array[LineString] = {
    val allLines = getLines
    logger.info(s"allLines Length: ${allLines.length}")
    val minX = p2.getEnvelopeInternal.getMinX
    val orderByY: Ordering[Coordinate] = (c1: Coordinate, c2: Coordinate) => c1.y.compareTo(c2.y)

    val orderLsByY: Ordering[LineString] = (x: LineString, y: LineString) =>
      x.getCoordinates.max(orderByY).compareTo(y.getCoordinates.max(orderByY))

    val orderByX: Ordering[Coordinate] = (c1: Coordinate, c2: Coordinate) => c1.x.compareTo(c2.x)

    lazy val startCoord = p2.getCoordinates.filter(c => c.x == minX).min(orderByY)
    lazy val startLines = allLines.filter(i => i.getCoordinates.contains {
      startCoord
    })
    if (startLines.length != 2) logger.warn(s"Found ${startLines.length} startLines. Expected 2." +
      s"""startLines: $startLines. allLines: $allLines""")

    /**
     *
     * @param cumulated comulated list of coordinates
     * @param leftOverArray set of linestrings that were not consumed during traversal
     * @param extensionValue up/down y-value, determined by polygon envelope,
     *                       used to draw vertival lines in non-convex polygons
     * @return
     */
    @tailrec
    def traverseAlongLines(cumulated: List[Coordinate], leftOverArray: Set[LineString], extensionValue: Double
                          ): (List[Coordinate], List[LineString]) = {
      val p = cumulated.last
      //logger.info(s"cumulated: $cumulated")

      val nextCoordX = leftOverArray.filter(lStr =>
        lStr.getCoordinates.contains(p) && lStr.getCoordinates.min(orderByX).equals2D(p)) // next Coordinate with _.x >= p.x
      if (nextCoordX.nonEmpty) {
        val c = nextCoordX.head.getCoordinates.filterNot(coord => cumulated.contains(coord))
        // logger.info(s"adding c: $c")
        traverseAlongLines(cumulated ++ c,
          leftOverArray - nextCoordX.head, extensionValue)
      } else {
        if (extensionValue != 0.0d) {
          val extensionLine = new LineString(new CoordinateArraySequence(Array(p, new Coordinate(p.x, extensionValue))), gf)
          val newLineString: Set[(LineString, LineString)] = leftOverArray.filterNot(lStr =>
            lStr.getCoordinates.contains(p)). //avoid self intersections
            filter(i => i.intersects(extensionLine)).
            map(i => (i, i.intersection(extensionLine))).map {
            case (ls, p) => (ls, new LineString(
              new CoordinateArraySequence(Array(
                ls.getCoordinates.filter(coo => !coo.equals2D(p.getCoordinate)).maxBy(_.x), p.getCoordinate)), gf))
          }

          if (newLineString.nonEmpty) {
            val (intersectedLine, selectedNewLineString) =
              if (extensionValue == maxY)
                newLineString.minBy(_._2)(orderLsByY)
              else
                newLineString.maxBy(_._2)(orderLsByY)

            val addList = List(
              if (extensionValue == maxY)
                selectedNewLineString.getCoordinates.maxBy(_.y)
              else
                selectedNewLineString.getCoordinates.minBy(_.y)
              , intersectedLine.getCoordinates.maxBy(_.x))

            logger.info(s"Traversal: cumulated $cumulated")
            logger.info(s"Traversal: leftOverArray $leftOverArray")
            logger.info(s"Traversal: Adding $addList")

            traverseAlongLines(cumulated ++ addList
              ,
              leftOverArray - intersectedLine, extensionValue)
          } else {
            (cumulated, leftOverArray.toList)
          }
        } else {
          (cumulated, leftOverArray.toList)
        }
      }
    }

    val emptyTravRes = (List.empty[Coordinate], List.empty[LineString])
    logger.info(s"allLines.toSet: ${allLines.toSet}")
    logger.info(s"allLines: ${asMultiLine(allLines)}")

    def getStartLs(index: Int): List[Coordinate] = {
      if (startLines.length > index) {
        val l = startLines(index).getCoordinates.toList
        if (l.head.equals2D(startCoord))
          l
        else
          l.reverse
      }
      else {
        List.empty[Coordinate]
      }
    }

    lazy val getStartUpperLower: (List[Coordinate], List[Coordinate]) = {
      val l = List(getStartLs(0), getStartLs(1))
      val sortedList = l.sortBy(i => if (i.nonEmpty) Angle.angle(i.head, i.last) else 0.0f) //empty selectLines
      (sortedList(1), sortedList(0))
    }

    val (startLsUpper, startLsLower) = getStartUpperLower
    logger.info(s"startLsUpper, $startLsUpper")
    pGeo("startLsUpper",asMultiLine(toLineStringList(startLsUpper)))
    logger.info(s"startLsLower, $startLsLower")
    pGeo("startLsLower",asMultiLine(toLineStringList(startLsLower)))

    val (upperString, leftOverLineStringsTop) = if (startLines.nonEmpty)
      traverseAlongLines(startLsUpper, allLines.toSet, maxY)
    else
      emptyTravRes

    val (lowerString, leftOverLineStringsBot) = if (startLines.nonEmpty)
      traverseAlongLines(startLsLower, allLines.toSet, minY)
    else
      emptyTravRes

    /**
     * looks for LineString ls with following properties
     * - ls intersection with p2 results in single geometry, (?)
     * - is in upperString and lowerString
     * - is non-convex in x direction,
     * - is in Polygon which is l1 union l2
     */
    val cuttingLs: List[Option[LineString]] = (leftOverLineStringsTop intersect leftOverLineStringsBot).flatMap(i => i.getCoordinates.map(i => {
      val lls = getNewLineString(Array(new Coordinate(i.x, maxY), new Coordinate(i.x, minY)))
      val i1 = lls.intersection(getNewLineString(upperString.toArray))
      val i2 = lls.intersection(getNewLineString(lowerString.toArray))
      if (!i1.isEmpty && !i2.isEmpty) {
        val newLsIntersections: LineString = getNewLineString(Array(i1.getCoordinate, i2.getCoordinate))
        if (newLsIntersections.intersection(p2).getNumGeometries == 1)
          Some(newLsIntersections)
        else None
      } else {
        None
      }
    }
    ))

    //        val xLimiter: Option[LineString] = {
    //      val cList = cuttingLs.filter(i => i match {
    //        case Some(a) => true
    //        case None => false
    //      }).map(_.get)
    //      if (cList.nonEmpty) {
    //        Some(cList.maxBy(_.getEnvelopeInternal.getMaxX))
    //      } else {
    //        None
    //      }
    //    }
    //
    //
    val lsPoly = getPolygonByLineStrings(upperString, lowerString.reverse)
    val xLimiter: Option[LineString] = {
      val filteredCoordList = p1diffToolRadius.getCoordinates.filter(i => lsPoly.contains(gf.createPoint(i)))
      val coordOption = if (filteredCoordList.nonEmpty) Some(filteredCoordList.minBy(_.x))
      else None
      coordOption.map(delimiterCoord => getNewLineString(Array(new Coordinate(delimiterCoord.x, maxY),
        new Coordinate(delimiterCoord.x, minY))))
    }


    pGeo("p1diffToolRadius", p1diffToolRadius)

    logger.info(s"upperString: ${asMultiLine(toLineStringList(upperString))}")
    logger.info(s"lowerString: ${asMultiLine(toLineStringList(lowerString))}")

    val upperRework = xLimiter match {
      case Some(lineString) =>
        upperString.filter(x => x.getX < lineString.getCoordinate.getX) :+ lineString.getCoordinates.maxBy(_.y)
      case None => upperString
    }

    val botRework = xLimiter match {
      case Some(lineString) =>
        lowerString.filter(x => x.getX < lineString.getCoordinate.getX) :+ lineString.getCoordinates.minBy(_.y)
      case None => lowerString
    }

    logger.info(s"upperRework: ${asMultiLine(toLineStringList(upperRework))}")
    logger.info(s"botRework: ${asMultiLine(toLineStringList(botRework))}")

    val upperLsArray = toLineStringList(upperRework)
    val botLsArray = toLineStringList(botRework)

    (upperLsArray.map(i => reverseLs(i)).reverse ++ botLsArray).toArray
  }

  def getPolygon: Geometry = {

    selectLines.map(i => {
      val length = i.getStartPoint.distance(i.getEndPoint)

      lazy val asd = new Trochoidal {
        override val maxY: Float = length.toFloat
        override val maxStepSize: Float = ae.toFloat
        override val localOffsetFct: LocalMotionPrimitive = new CircularMotion {
          override val radius: Float = circMotionRadius.toFloat
          override val pointClearance: Double = config.minPointClearanceOnPath
        }
      }

      //logger.info(s"buffered poly in moat before transformation: ${asd.getPolygon}")
      val p = toAffMatrix(i.getStartPoint, i.getEndPoint).transform(asd.getPolygon)

//      logger.info(s"buffered poly in moat: ${p.buffer(tool.d / 2.0f)}")
      p.buffer(tool.d / 2.0f)
    }).reduceOption[Geometry] { case (a: Geometry, b: Geometry) => a.union(b) } match {
      case Some(g) => g
      case None => new Point(null, gf)
    }
  }

  def getPolygon2: Geometry = {
    val moatLs = getNewLineString(selectLines)
    logger.info(s"selectLines: $selectLines")
    logger.info(s"moatLs: $moatLs")
    if (moatLs.isEmpty)
      emptyGeometry
    else {
      val lsTraversal = new LineStringTraversal {
        override val ls: LineString = getNewLineString(selectLines)
        override val aeMax: Double = tool.ae
        override val localMotionPrimitive: LocalMotionPrimitive = new CircularMotion {
          override val radius: Float = circMotionRadius.toFloat
          override lazy val localPolygonPoints: Int = 90
          override val pointClearance: Double = config.minPointClearanceOnPath
        }
      }
      lsTraversal.getPolygon.buffer(tool.d / 2.0f)
    }

    //      logger.info(s"buffered poly in moat before transformation: ${asd.getPolygon}")
    //      val p = toAffMatrix(i.getStartPoint, i.getEndPoint).transform(asd.getPolygon)
    //
    //      logger.info(s"buffered poly in moat: ${p.buffer(tool.d / 2.0f)}")
    //      p.buffer(tool.d / 2.0f)
    //    }).reduceOption[Geometry] { case (a: Geometry, b: Geometry) => a.union(b) } match {
    //      case Some(g) => g
    //      case None => new Point(null, gf)
    //    }
  }

  def getPath: Array[Geometry] = {
    val wktReader = new WKTReader()
    // val moatLs = wktReader.read("LINESTRING (45 39.59687576256715, 5 7.596875762567151,5 5,47.5 5)").asInstanceOf[LineString]

   val moatLs = getNewLineString(selectLines)

    logger.info(s"Path LineString: $moatLs")
    if (moatLs.isEmpty)
      Array.empty[Geometry]
    else {
      val lsTraversal = new LineStringTraversal {
        override val ls: LineString = moatLs
        override val aeMax: Double = tool.ae
        override val localMotionPrimitive: LocalMotionPrimitive = new CircularMotion {
          override val radius: Float = circMotionRadius.toFloat
          override lazy val localPolygonPoints: Int = 90
          override val pointClearance: Double = config.minPointClearanceOnPath
        }
      }
      logger.info(s"lsTraversal.getSimplePath: ${getNewLineString(lsTraversal.getSimplePath.map(_.asInstanceOf[LineString]))}")
      lsTraversal.getSimplePath
    }
  }

//  def getPath: Array[Geometry] = {
//    lazy val lineStrings = selectLines.map(i => {
//      val length = i.getStartPoint.distance(i.getEndPoint)
//
//      val asd = new Trochoidal {
//        override val maxY: Float = length.toFloat
//        override val maxStepSize: Float = ae.toFloat
//        override val localOffsetFct: LocalMotionPrimitive = new CircularMotion {
//          override val radius: Float = circMotionRadius.toFloat
//          override val localPolygonPoints: Int = 90
//        }
//      }
//
//      toAffMatrix(i.getStartPoint, i.getEndPoint).transform(asd.getLineString)
//    })
//    lineStrings
//  }

  def returnPath: List[List[Float]] = {
    val a = getPath.map(_.getCoordinates)
    val b: List[Coordinate] = a.reduceOption(_ ++ _).getOrElse(Array.empty[Coordinate]).toList
    val floatList  = b.map(i => List(i.x.toFloat, i.y.toFloat, 0.0f))
    logger.info(s"floatList: $floatList")
    floatList
  }

}

object MoatExample extends App with LazyLogging with JtsUtils{
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

  val moatPrimitive = new Moat {
    val tool = CncTool(
      d = 5.0f,
      ae = 2.5f,
      ap = 6.0f,
      vf = 1.2750f,
      n = 7960,
      description = "Alu Roughing, 12mm, Stirnfräsen, TODO Werte aktualisieren",
      idString = "123")

    override val config: PathCoverageStepConfig = PathCoverageStepConfig()
    override val p1: Polygon = poly1
  }

  val ls = getNewLineString(moatPrimitive.returnPath.map(asCoordinate).toArray)
  println(s"ls: \r\n$ls")

  println("Done")
}


