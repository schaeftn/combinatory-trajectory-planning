package org.combinators.ctp.repositories.pathcoverage

import com.typesafe.scalalogging.LazyLogging
import org.combinators.ctp.repositories.cncPathFct
import org.combinators.ctp.repositories.runinhabitation.RunCncPathCoverage.{gf, logger}
import org.combinators.ctp.repositories.toplevel.{Cnc2DModel, PathCoverageStep, PathCoverageStepConfig}
import org.locationtech.jts.geom.impl.CoordinateArraySequence
import org.locationtech.jts.geom.{Coordinate, CoordinateFilter, Geometry, GeometryFactory, LineString, MultiLineString, MultiPolygon, Polygon}
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

    // prio2 fix and test filter for repositioning path
    val path = toolPath.getCoordinates.filter(coord => gf.createPoint(coord).isWithinDistance(poly, t.d/2.0))

    val toolpathBuffered = pcConfig.bufferFct(toolPath, (t.d / 2.0)) //Rundungsfehler
    pGeo("toolpathBuffered", toolpathBuffered)

    pGeo("path aggregated single", {
      new LineString(
        new CoordinateArraySequence(
          asFloatList(path).map(i => new Coordinate(i(0), i(1))).toArray), gf)
    })

    (asFloatList(path), toolpathBuffered)
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
        // val validPos = invalidToolPositions.asInstanceOf[Polygon].getInteriorRingN(0)

        val selectedRestGeo: Option[Geometry] = initialScene.rest.headOption

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
          val poly = saniPoly(polyOption.get, pcConfig)

          if (polyOption.isEmpty || poly.isEmpty || poly.getArea < 0.01) {
            (aggregatedPath, s)
          } else {
            val (path, bufferedToolPath) = singleContourStep2(poly, t, s, pcConfig)
            logger.info(s"path Ls: ${asLineString(path)}")
            pGeo("poly",poly)
            pGeo("bufferedToolPath",bufferedToolPath)
            val diff = poly.getArea - poly.difference(bufferedToolPath).getArea
            if (diff < 2.0)
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
                      map(_.asInstanceOf[Polygon]).minBy(i => distanceToPoint(i, aggregatedPath.last.last))
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
