package org.combinators.ctp.repositories.pathcoverage

import com.typesafe.scalalogging.LazyLogging
import org.combinators.ctp.repositories.cncPathFct
import org.combinators.ctp.repositories.runinhabitation.RunCncPathCoverage.{gf, logger}
import org.combinators.ctp.repositories.toplevel.{Cnc2DModel, PathCoverageResult, PathCoverageStep, PathCoverageStepConfig}
import org.locationtech.jts.geom.impl.CoordinateArraySequence
import org.locationtech.jts.geom.{Coordinate, CoordinateFilter, Geometry, GeometryFactory, LineString, MultiLineString, MultiPolygon, Polygon}
import org.locationtech.jts.io.WKTReader
import org.locationtech.jts.util.CoordinateArrayFilter

import scala.annotation.tailrec

trait Contour extends LazyLogging with JtsUtils {

  lazy val singleContourStep: CncTool => cncPathFct = { t => {
    (initialScene: Cnc2DModel, pcConfig: PathCoverageStepConfig) =>
      initialScene.rest.foreach(a => pGeo("initialScene Rest", a))
      pGeo("targetWorkpiece", {
        initialScene.targetWorkpiece
      })

      pGeo("initialScene.getMachinedMultiGeo", initialScene.getMachinedMultiGeo)
      val invalidToolPositions = pcConfig.bufferFct(initialScene.targetWorkpiece, t.d / 2.0d) // ???
      pGeo("invalidToolPositions", invalidToolPositions)

      val machinableArea = pcConfig.bufferFct(initialScene.getMachinedMultiGeo, t.ae)
      pGeo("machinableArea", machinableArea)

      val validPosPoly = pcConfig.bufferFct(initialScene.targetGeometry, -t.d / 2.0d).intersection(
        pcConfig.bufferFct(machinableArea, -t.d / 2.0d))
      pGeo("validPosPoly", validPosPoly)

      val restBuffered = pcConfig.bufferFct(initialScene.getRestMultiGeo, t.d / 2)
      pGeo("restBuffered", restBuffered)

      val restfilterBuffer = pcConfig.bufferFct(pcConfig.bufferFct(initialScene.getRestMultiGeo, -t.ae), t.ae)
      pGeo("restfilterBuffer", restfilterBuffer)

      val restBufferedTest = pcConfig.bufferFct(
        pcConfig.bufferFct(
          pcConfig.bufferFct(initialScene.getRestMultiGeo, -t.ae), t.ae), t.d / 2 - t.ae)
      pGeo("restBufferedTest", restBufferedTest)

      val restBufferedInters = pcConfig.bufferFct(
        pcConfig.bufferFct(restBuffered.intersection(invalidToolPositions), -0.0001), 0.0002) //Randbereiche
      pGeo("restBufferedInters", restBufferedInters)

      val fullStuff = restBufferedTest.union(restBufferedInters)
      pGeo("fullStuff", fullStuff)
      val filteredFull = filterLsAndReturnMultiPoly(fullStuff)
      pGeo("filteredFull", filteredFull)

      val f1 = validPosPoly.difference(filteredFull) // mit difference noch holes benötigt?
      pGeo("f1", f1)

      val toolPath = if (f1.isEmpty)
        emptyGeometry
      else {
        getFirstExteriorFromPolygon(f1)
      }

      pGeo("toolPath", toolPath)

      val path = toolPath.getCoordinates
      val toolpathBuffered = pcConfig.bufferFct(toolPath, t.d / 2.0)
      pGeo("toolpathBuffered", toolpathBuffered)
      val newScene = initialScene.withMachinedGeo(toolpathBuffered)
      logger.info(s"Contour after machinedGeo")

      pGeo("path aggregated single", {
        new LineString(
          new CoordinateArraySequence(
            asFloatList(path).map(i => new Coordinate(i(0), i(1))).toArray), gf)
      })

      (List(asFloatList(path)), newScene)
  }
  }

  def singleContourStep2(poly: Geometry, t: CncTool, initialScene: Cnc2DModel, pcConfig: PathCoverageStepConfig): (List[List[Float]], Geometry) = {
    initialScene.rest.foreach(a => pGeo("initialScene Rest", a))
    pGeo("targetWorkpiece", initialScene.targetWorkpiece)
    pGeo("initialScene.getMachinedMultiGeo", initialScene.getMachinedMultiGeo)
    pGeo("poly", poly)

    val invalidToolPositions = pcConfig.bufferFct(initialScene.targetWorkpiece, t.d / 2.0d)
    pGeo("invalidToolPositions", invalidToolPositions)

    val machinableArea = pcConfig.bufferFct(initialScene.getMachinedMultiGeo, t.ae)
    pGeo("machinableArea", machinableArea)

    val validPosPoly = pcConfig.bufferFct(initialScene.targetGeometry, -t.d / 2.0d).intersection(
      pcConfig.bufferFct(machinableArea, -t.d / 2.0d))
    pGeo("validPosPoly", validPosPoly)

    val polyBuffered = pcConfig.bufferFct(poly, t.d / 2)
    pGeo("polyBuffered", polyBuffered)

    val restBufferedInters = pcConfig.bufferFct(
      pcConfig.bufferFct(polyBuffered.intersection(validPosPoly), -0.0001), 0.0002) //Randbereiche
    pGeo("restBufferedInters", restBufferedInters)

    val filteredFull = polyBuffered.intersection(filterLsAndReturnMultiPoly(restBufferedInters))
    pGeo("filteredFull", filteredFull)

    val f1 = smartCastToPolygon(filteredFull.intersection(validPosPoly)).getExteriorRing.
      intersection(validPosPoly)
    pGeo("f1", f1)

    val toolPath = getLongestLineString(f1, pcConfig)
    pGeo("toolPath", toolPath)


    //TODO filter toolpath. must not be further away than ...
    val toolPath2 = toolPath // asLineString(asFloatList(toolPath.getCoordinates).filter(c => asPoint(c).distance(poly)<= t.d/2.0 + pcConfig.maxPointClearanceOnPath))

    // prio2 fix and test filter for repositioning path
    val path = toolPath2.getCoordinates

    val toolpathBuffered = pcConfig.bufferFct(toolPath2, (t.d / 2.0)) //Rundungsfehler
    pGeo("toolpathBuffered", toolpathBuffered)

    pGeo("path aggregated single", {
      new LineString(
        new CoordinateArraySequence(
          asFloatList(path).map(i => new Coordinate(i(0), i(1))).toArray), gf)
    })

    (asFloatList(path), toolpathBuffered)
  }

  def createFinishContourStep(t:CncTool) : PathCoverageStep = {
    lazy val combinatorPcFunction: cncPathFct = singleContourStep(t)

    PathCoverageStep(Some(combinatorPcFunction), Some(t), List.empty[PathCoverageStep], "Aluminum Finishing")
  }

  def createMultiContourStep(t: CncTool): PathCoverageStep = {
    lazy val combinatorPcFunction: cncPathFct = {
      (initialScene: Cnc2DModel, pcConfig: PathCoverageStepConfig) =>
        initialScene.rest.foreach(a => pGeo("initialScene Rest", a))
        pGeo("targetWorkpiece", {
          initialScene.targetWorkpiece
        })
        pGeo("initialScene.getMachinedMultiGeo", initialScene.getMachinedMultiGeo)

        val invalidToolPositions = initialScene.targetWorkpiece.buffer(t.d / 2.0d) // ???
        pGeo("invalidToolPositions", invalidToolPositions)

        val selectedRestGeo: Option[Geometry] = if (initialScene.rest.nonEmpty) Some(
          initialScene.rest.maxBy(_.getArea))
        else None

        /**
         * extrahiert für die gewählte fräsfläche jene Grenze, welche an machined Bereich grenzt
         * None, if initialscene.rest is empty
         */
        lazy val extractInitialMachinableFrontier: Option[LineString] = selectedRestGeo.flatMap(getLongestExteriorFromPolygon)

        /**
         * currentScene that will be
         */

        @tailrec
        def performAndEvaluateStep(
                                    polyOption: Option[Geometry],
                                    aggregatedPath: List[List[List[Float]]],
                                    s: Cnc2DModel): (List[List[List[Float]]], Cnc2DModel) = {
          logger.info(s"saniPoly: \r\n${polyOption.get}")
          val poly = saniPoly(polyOption.get, pcConfig)

          logger.info(s"saniPoly: \r\n$poly")
          if (polyOption.isEmpty || poly.isEmpty || poly.getArea < 0.01) {
            (aggregatedPath, s)
          } else {
            val (path, bufferedToolPath) =   singleContourStep2(poly, t, s, pcConfig)
            logger.info(s"path Ls: ${asLineString(path)}")
            pGeo("poly",poly)
            pGeo("bufferedToolPath",bufferedToolPath)
            val diff = poly.getArea - poly.difference(bufferedToolPath).getArea
            if (diff < 0.5)
              (aggregatedPath, s)
            else {
              val newPoly = poly.difference(bufferedToolPath)
              val (selectedPoly, newModel) = newPoly.getGeometryType match {
                case "MultiPolygon" => {
                  val selPoly = if (aggregatedPath.isEmpty)
                    getGeoListFromGeo(newPoly).
                      map(_.asInstanceOf[Polygon]).maxBy(_.getArea)
                  else
                    getGeoListFromGeo(newPoly).
                      map(_.asInstanceOf[Polygon]).maxBy(_.getArea)
                  //minBy(i => distanceToPoint(i, aggregatedPath.last.last))
                  (selPoly,s.withMachinedGeo(bufferedToolPath))
                }
                case "Polygon" => (newPoly, s.withMachinedGeo(bufferedToolPath))
              }
              if (aggregatedPath.nonEmpty && path.nonEmpty && path.head.nonEmpty)
                performAndEvaluateStep(Some(selectedPoly),
                  aggregatedPath :+ path,
                  newModel)
              else
                performAndEvaluateStep(Some(selectedPoly), aggregatedPath :+ path, newModel)
            }
          }
        }

        val (path, endScene) = performAndEvaluateStep(initialScene.rest.headOption, List.empty[List[List[Float]]], initialScene)

        path.foreach(p => pGeo("path aggregated Multi", {
          new LineString(
            new CoordinateArraySequence(
              p.map(i => new Coordinate(i(0), i(1))).toArray), gf)
        }))

        (path, endScene)
    }
    PathCoverageStep(Some(combinatorPcFunction), Some(t), List.empty[PathCoverageStep], "Contour-based machining step")
  }
}

object RunContourExample extends App with LazyLogging with JtsUtils {
  val wktReader = new WKTReader()
  val wktStrP1 = """POLYGON ((70 40, 69.42355841209691 36.098193559677426, 67.7163859753386 32.34633135269819, 64.94408836907635 28.888595339607956, 61.21320343559642 25.857864376269045, 56.66710699058805 23.37060775394909, 51.4805029709527 21.522409349774268, 45.852709660483846 20.38429439193539, 39.99999999999999 20, 34.14729033951614 20.384294391935395, 28.51949702904729 21.52240934977427, 23.332893009411933 23.370607753949095, 18.78679656440357 25.85786437626905, 15.055911630923635 28.88859533960796, 12.283614024661393 32.34633135269821, 10.576441587903087 36.09819355967743, 10 40, 10 60, 40 60, 70 60, 70 40))"""
  val wktMachined = """POLYGON ((10 0, 10 40, 10.576441587903087 36.09819355967743, 12.283614024661393 32.34633135269821, 15.055911630923635 28.88859533960796, 18.78679656440357 25.85786437626905, 23.332893009411933 23.370607753949095, 28.51949702904729 21.52240934977427, 34.14729033951614 20.384294391935395, 39.99999999999999 20, 45.852709660483846 20.38429439193539, 51.4805029709527 21.522409349774268, 56.66710699058805 23.37060775394909, 61.21320343559642 25.857864376269045, 64.94408836907635 28.888595339607956, 67.7163859753386 32.34633135269819, 69.42355841209691 36.098193559677426, 70 40, 70 0, 10 0))"""
  val tgtGeo = wktReader.read(wktStrP1)
  val machinedGeo = wktReader.read(wktMachined)

  val model = Cnc2DModel(boundaries = List(0.0f, 50.0f, 0.0f, 50.0f),
    targetGeometry = tgtGeo.union(machinedGeo), rest = List(tgtGeo), machined = List(), machinedMultiPolygon = emptyGeometry, emptyGeometry).withInitialMachinedGeo(machinedGeo)
  val cont = new Contour {}.createMultiContourStep(CncTool(8.0f, 4.0f, 4.0f, 1.4300f, 11935,
  "Alu finishing, 8mm, Stirnfraesen, radiale Zustellung 4mm, vf 1430mm/min, n 11935", "2 Z S2000"))

  val res = PathCoverageResult(model, PathCoverageStepConfig(), List(cont))

  logger.info(s"res.computeModelHistory._1.last.getRestMultiGeo: \r\n ${res.computeModelHistory._1.last.getRestMultiGeo}")
  val toPaths = res.pathList.map{ singleCompPathlist =>
    new LineString(
      new CoordinateArraySequence(
        singleCompPathlist.map(c => new Coordinate(c(0), c(1))).toArray)
      , gf)}

  val mlString = new MultiLineString(toPaths.toArray,gf)

  logger.info(s"mlString: \r\n ${mlString}")

  // s: Cnc2DModel, config: PathCoverageStepConfig, l: List[PathCoverageStep]
}