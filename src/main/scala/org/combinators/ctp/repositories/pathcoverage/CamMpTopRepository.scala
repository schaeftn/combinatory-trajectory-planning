package org.combinators.ctp.repositories.pathcoverage

import com.typesafe.scalalogging.LazyLogging
import org.combinators.ctp.repositories.toplevel.{Cnc2DModel}
import org.combinators.ctp.repositories._
import org.combinators.cls.interpreter.combinator
import org.combinators.cls.types.Constructor
import org.combinators.cls.types.syntax._
import org.locationtech.jts.geom.impl.CoordinateArraySequence
import org.locationtech.jts.geom.{Coordinate, Envelope, Geometry, GeometryFactory, LineString, LinearRing, MultiLineString, Polygon}
import org.locationtech.jts.math.Vector2D

case class CncTool(d: Float, ae: Float, ap: Float, vf: Float, n: Int)

trait CamMpTopRepository extends LazyLogging with JtsUtils {

  val pathCoverageFct = Constructor("pathCoverageFct")
  val coveragePrimitive = Constructor("coveragePrimitive")
  val millingTool = Constructor("millingTool")
  val roughing = Constructor("roughing")
  val finishing = Constructor("finishing")

  // Moat first, rest later
  @combinator object MoatDecomposition {
    def apply(t: CncTool,
              restMp: Cnc2DModel => (List[List[List[Float]]], Cnc2DModel)):
    Cnc2DModel => (List[List[List[Float]]], Cnc2DModel) =
    { initialScene =>
      val moatPrimitive = new Moat {
        val tool: CncTool = t
        val restCoords = initialScene.rest.head.getCoordinates
        val pArray = if (restCoords.nonEmpty) restCoords :+ restCoords.head else Array.empty[Coordinate]
        val p1: Polygon = new Polygon(new LinearRing(new CoordinateArraySequence(pArray), gf), Array.empty[LinearRing], gf)
      }

      logger.info(s"tool: d: ${t.d}")
      logger.info(s"computung diffgeo Moat")
      logger.info(s"initialScene.rest.head ${initialScene.rest.head}")
      val selectLines = moatPrimitive.selectLines
      if (selectLines.nonEmpty) {
        logger.info(s"asd.selectLines: $selectLines")
        val ml = new MultiLineString(selectLines, new GeometryFactory())
        logger.info(s"asd.selectLines: $ml")
        logger.info(s"asd.selectLines.buffer: ${ml.buffer(t.d)}")
      }
      else
        logger.info(s"asd.selectLines: empty")
      logger.info(s"asd.returnPath Length: ${moatPrimitive.returnPath.length}")

      val diffGeo = moatPrimitive.getPolygon
       logger.info(s"diffgeo: $diffGeo")
      val newScene = initialScene.withMachinedGeo(diffGeo)

      val (resultPath, resultScene) = restMp(newScene)
      logger.info(s"After restMp")
      logger.info(s"resultPath Length: ${resultPath.length}")
      val rPath: List[List[List[Float]]] = moatPrimitive.returnPath +: resultPath
      logger.info(s"After rPath")
      (rPath, resultScene)
    }

    val semanticType = millingTool =>: pathCoverageFct =>: pathCoverageFct
    }

  @combinator object GenericDecomposition {
    def apply(
               alg1: Cnc2DModel => (List[List[List[Float]]], Cnc2DModel),
               alg2: Cnc2DModel => (List[List[List[Float]]], Cnc2DModel)):
    Cnc2DModel => (List[List[List[Float]]], Cnc2DModel) = { initialScene =>
      logger.info(s"computung diffgeo Moat")
      logger.info(s"initialScene.rest.head ${initialScene.rest.head}")

      val (resultPath, resultScene) = alg1(initialScene)
      val (resultPath2, resultScene2) = alg2(resultScene)
      logger.info(s"After restMp")
      logger.info(s"resultPath Length: ${resultPath.length}")
      val rPath: List[List[List[Float]]] = resultPath ++ resultPath2
      logger.info(s"After rPath")
      (rPath, resultScene2)
    }

    val semanticType = pathCoverageFct =>: pathCoverageFct =>: pathCoverageFct
  }

  @combinator object ZigZagPrimitive {
    def apply(t: CncTool): Cnc2DModel => (List[List[List[Float]]], Cnc2DModel) = {
      initialScene: Cnc2DModel =>
        initialScene.rest.foreach(a => logger.info(s"initialScene Rest: $a"))
        val gf = new GeometryFactory()
        val zigZag = new ZigZag {
          override val toolDia = t.d
          override val a_e: Float = t.ae
          override val restGeo: Geometry = initialScene.rest.head
          override val machinedGeo: Geometry = initialScene.machined.reduceOption[Geometry] {
            case (a: Geometry, b: Geometry) => a.union(b)
          }.getOrElse(gf.toGeometry(new Envelope()))
          override val stepDirection: Vector2D = new Vector2D(1.0f, 0.0f)
        }
        logger.info(s"ZiGZag after init")
        logger.info(s"ZigZag starting for geometry: ${zigZag.restGeo}")
        val boundingPolygon = zigZag.multiLines.buffer(t.d.toDouble/2.0d)
        val path = zigZag.findPoints
        logger.info(s"zigzag boundingPolygon: $boundingPolygon")
        val newScene = initialScene.withMachinedGeo(boundingPolygon)
        logger.info(s"zigzag after machinedGeo")

        (List(path), newScene)
    }

    val semanticType = millingTool =>: coveragePrimitive :&: pathCoverageFct
  }


  @combinator object SingleContourStep {
    def apply(t: CncTool): Cnc2DModel => (List[List[List[Float]]], Cnc2DModel) = {
      initialScene: Cnc2DModel =>
        logger.info(s"tool dia, ae: ${t.d}, ${t.ae}")
        initialScene.rest.foreach(a => logger.info(s"initialScene Rest: $a"))
        logger.info(s"targetWorkpiece: ${initialScene.targetWorkpiece}")

        pGeo("initialScene.getMachinedMultiGeo", initialScene.getMachinedMultiGeo)
        val invalidToolPositions = initialScene.targetWorkpiece.buffer(t.d / 2.0d) // ???
        pGeo("invalidToolPositions", invalidToolPositions)

        val newRest = initialScene.rest.head.difference(invalidToolPositions)
        pGeo("newRest", newRest)

        val newPossibleMillingArea = initialScene.getMachinedMultiGeo.buffer(t.ae).
          difference(initialScene.getMachinedMultiGeo)
        pGeo("newPossibleMillingArea", newPossibleMillingArea)

        val bufferedNewMachinedArea = newPossibleMillingArea.buffer(t.d / 2 - t.ae)
        pGeo("bufferedNewMachinedArea", bufferedNewMachinedArea)

        val firstInteriorRing = {
          val geoType = bufferedNewMachinedArea.getGeometryType
          val asPoly = geoType match {
            case "Polygon" => bufferedNewMachinedArea.asInstanceOf[Polygon]
            case "MultiPolygon" => bufferedNewMachinedArea.getGeometryN(0).asInstanceOf[Polygon]
          }

          if (asPoly.getNumInteriorRing == 0) {
            asPoly.getExteriorRing
          } else {
            asPoly.getInteriorRingN(0)
          }
        }

        logger.info(s"firstInteriorRing \r\n$firstInteriorRing")

        val newModuloMachined = firstInteriorRing.intersection(initialScene.getMachinedMultiGeo)
        pGeo("newModuloMachined", newModuloMachined)

        val test2 = newModuloMachined.difference(invalidToolPositions)
        pGeo("test2", test2)

        logger.info(s"test2.isEmpty: ${test2.isEmpty}")
        if (test2.isEmpty)
          (List.empty[List[List[Float]]], initialScene)
        else {
          val lsList: List[LineString] =
            (0 until test2.getNumGeometries).map(i => test2.getGeometryN(i).asInstanceOf[LineString]).toList

          val toolPath = lsList.maxBy(_.getLength)
          logger.info(s"SingleContour.toolPath: $toolPath")

          val path = toolPath.getCoordinates
          val toolpathBuffered = toolPath.buffer(t.d / 2.0)
          pGeo("toolpathBuffered", toolpathBuffered)
          val newScene = initialScene.withMachinedGeo(toolPath.buffer(t.d / 2.0))
          logger.info(s"Contour after machinedGeo")

          (List(asFloatList(path)), newScene)
        }
    }

    val semanticType = millingTool =>: coveragePrimitive :&: pathCoverageFct
  }

  @combinator object DummyTool {
    def apply: CncTool = CncTool(6.0f, 1.5f, 9.0f, 7440.0f, 29180)

    val semanticType = millingTool
  }

  @combinator object DummyTool2 {
    def apply: CncTool = CncTool(d = 18.0f, ae = 1.5f, ap = 9.0f,
      vf = 7440.0f, n = 29180)

    //42CrMo4
    val semanticType = millingTool :&: roughing
  }

// Material types:
//  42CrMo4
//  EN AW 6082, Alu
  @combinator object DummyTool3 {
    def apply: CncTool = CncTool(d = 3.0f, ae = 0.5f, ap = 9.0f, vf = 7440.0f, n = 29180)

    val semanticType = millingTool :&: finishing
  }
}
