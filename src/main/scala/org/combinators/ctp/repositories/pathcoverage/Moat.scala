package org.combinators.ctp.repositories.pathcoverage

import com.typesafe.scalalogging.LazyLogging
import org.locationtech.jts.geom.impl.CoordinateArraySequence
import org.locationtech.jts.geom.{Coordinate, Geometry, LineString, LinearRing, MultiPolygon, Point, Polygon}
import org.locationtech.jts.dissolve.LineDissolver
import org.locationtech.jts.simplify.DouglasPeuckerSimplifier

import scala.annotation.tailrec

trait Moat extends GeoUtils with LazyLogging with JtsUtils {
  val tool: CncTool
  lazy val ae: Double = tool.ae
  lazy val circMotionRadius: Double = tool.d / 2.0d

  val p1: Polygon
  lazy val p2 = p1.buffer(-tool.d)

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
  lazy val maxX: Double = p2.getEnvelopeInternal.getMinY
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

    val (upperString, l1) = if (startLines.nonEmpty)
      traverseAlongLines(startLines(0).getCoordinates.toList, allLines.toSet, maxY)
    else
      emptyTravRes

    val (lowerString, l2) = if (startLines.nonEmpty)
      traverseAlongLines(startLines(1).getCoordinates.toList, allLines.toSet, maxX)
    else
      emptyTravRes

    /**
     * looks for LineString with following properties
     * - intersection completely in p2,
     * - is in upperString and lowerString
     * - is non-convex in x direction,
     * - is in Polygon which is l1 union l2
     */
    val cuttingLs: List[Option[LineString]] = (l1 intersect l2).flatMap(i => i.getCoordinates.map(i => {
      val lls = getNewLineString(Array(new Coordinate(i.x, maxY), new Coordinate(i.x, maxX)))
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

    val xLimiter: Option[LineString] = {
      val cList = cuttingLs.filter(i => i match {
        case Some(a) => true
        case None => false
      }).map(_.get)
      if (cList.nonEmpty) {
        Some(cList.maxBy(_.getEnvelopeInternal.getMaxX))
      } else {
        None
      }
    }

    logger.info(s"upperString: $upperString")
    logger.info(s"botStromg: $lowerString")

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

    logger.info(s"upperRework: $upperRework")
    logger.info(s"botRework: $botRework")

    val upperLsArray = toLineStringList(upperRework)
    val botLsArray = toLineStringList(botRework)

    (upperLsArray.reverse ++ botLsArray).toArray
  }

  def getPolygon2: Geometry = {
    selectLines.map(i => {
      val length = i.getStartPoint.distance(i.getEndPoint)

      lazy val asd = new Trochoidal {
        override val maxY: Float = length.toFloat
        override val maxStepSize: Float = ae.toFloat
        override val localOffsetFct: LocalMotionPrimitive = new CircularMotion {
          override val radius: Float = circMotionRadius.toFloat
          override val localPolygonPoints: Int = 90
        }
      }

      logger.info(s"buffered poly in moat before transformation: ${asd.getPolygon}")
      val p = toAffMatrix(i.getStartPoint, i.getEndPoint).transform(asd.getPolygon)

      logger.info(s"buffered poly in moat: ${p.buffer(tool.d / 2.0f)}")
      p.buffer(tool.d / 2.0f)
    }).reduceOption[Geometry] { case (a: Geometry, b: Geometry) => a.union(b) } match {
      case Some(g) => g
      case None => new Point(null, gf)
    }
  }

  def getPolygon: Geometry = {
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
          override val localPolygonPoints: Int = 90
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
          override val localPolygonPoints: Int = 90
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
  val moatPrimitive = new Moat {
    val tool = CncTool(d = 0.4f, ae = 0.05f, ap = 0.2f, vf = 1.0f, 1000)

    val pArray = Array(new Coordinate(0.0d, 0.0d, 0.0d),
      new Coordinate(0.0d, 1.0d, 0.0d),
      new Coordinate(5.0d, 5.0d, 0.0d),
      new Coordinate(5.0d, -5.0d, 0.0d),
      new Coordinate(0.0d, 0.0d, 0.0d))
    val p1: Polygon = new Polygon(new LinearRing(new CoordinateArraySequence(pArray), gf), Array.empty[LinearRing], gf)
  }
  println(moatPrimitive.returnPath)
  ToAkka(moatPrimitive.returnPath)
}


