package org.combinators.ctp.repositories.runinhabitation

import java.io.{File, FileWriter}

import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.math3.util.{FastMath, MathUtils}
import org.combinators.cls.interpreter.{InhabitationResult, ReflectedRepository}
import org.combinators.cls.types.{Constructor, Taxonomy}
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.pathcoverage.{CamMpTopRepository, CncTool, JtsUtils, PathCoverageResult, PathFeedUtils}
import org.combinators.ctp.repositories.toplevel._
import org.locationtech.jts.algorithm.Angle
import org.locationtech.jts.geom.{Coordinate, Geometry, GeometryFactory, LineString, LinearRing, MultiLineString, Polygon}
import org.locationtech.jts.geom.impl.CoordinateArraySequence
import org.locationtech.jts.io.WKTReader
import org.locationtech.jts.util.Stopwatch
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.parser.decode

import scala.io.Source


object RunCncTestLinear extends App with JtsUtils {
  def printTable(t: List[(Float, Float, Float)]): Unit = {
    t.takeWhile { case (a, b, c) => a < 3990.0f }.foreach {
      case (a, b, c) => println(f"[${"%.5f".formatLocal(java.util.Locale.US, a)}, " +
        f"${"%.5f".formatLocal(java.util.Locale.US, b)}, " +
        f"${"%.5f".formatLocal(java.util.Locale.US, c)}]")
    }
  }

  val tool = CncTool(12.0f, 3.0f, 6.0f, 3990f, 13300,
    "Alu Roughing, d 12mm, ae 3mm, vf 3990 mm/min, n 13300", "1 Z S13300")
  val pFct: (Cnc2DModel, PathCoverageStepConfig) => (List[List[List[Float]]], Cnc2DModel) = {
    case (a, c) => (List(List(List(0.0f, 0.0f, 0.0f), List(0.0f, 1000.0f, 0.0f), List(1.0f, 1000.0f, 0.0f))), a)
  }

  val wktReader = new WKTReader()

  val wktStr: String = Source.fromResource("models/machiningUc1.wkt").getLines.mkString("\r\n")
  val tgtGeo = wktReader.read(wktStr)

  val bounds = List[Float](0.0f, 50.0f, -15.0f, 40.0f)

  val machinedGeo = wktReader.read("""POLYGON ((0 -15, 0 0, 50 0, 50 -15, 0 -15))""")
  val scene = Cnc2DModel(boundaries = bounds, targetGeometry = tgtGeo, rest = List(tgtGeo), machined = List(),
    machinedMultiPolygon = emptyGeometry, initialMachined = emptyGeometry).withInitialMachinedGeo(machinedGeo)
  val config = PathCoverageStepConfig(false)
  val pcs = PathCoverageStep(Some(pFct), Some(tool), List.empty[PathCoverageStep], "testrun")
  val pcr = PathCoverageResult(scene, config, List(pcs))

  printTable(config.xAccLookup)
  println("#########")
  printTable(config.yAccLookup)

  //PathFeeds(pcr).withMaxVfByAcc.foreach(_.foreach(println))
}