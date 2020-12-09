package org.combinators.ctp.repositories.pathcoverage

import com.typesafe.scalalogging.LazyLogging
import org.combinators.ctp.repositories.cncPathFct
import org.combinators.ctp.repositories.runinhabitation.RunCncPathCoverage.{gf, logger}
import org.combinators.ctp.repositories.toplevel.{Cnc2DModel, PathCoverageStep, PathCoverageStepConfig}
import org.locationtech.jts.geom.impl.CoordinateArraySequence
import org.locationtech.jts.geom.{Coordinate, Geometry, GeometryFactory, LineString, MultiLineString, MultiPolygon, Polygon}

import scala.annotation.tailrec

trait Contour extends LazyLogging with JtsUtils {

  lazy val singleContourStep: CncTool => cncPathFct = { t => {
    (initialScene: Cnc2DModel, pcConfig: PathCoverageStepConfig) =>
      initialScene.rest.foreach(a => pGeo("initialScene Rest", a))
      pGeo("targetWorkpiece", {
        initialScene.targetWorkpiece
      })

      pGeo("initialScene.getMachinedMultiGeo", initialScene.getMachinedMultiGeo)
      val invalidToolPositions = pcConfig.bufferFct(initialScene.targetWorkpiece,t.d / 2.0d ) // ???
      pGeo("invalidToolPositions", invalidToolPositions)

      val machinableArea = pcConfig.bufferFct(initialScene.getMachinedMultiGeo, t.ae)
      pGeo("machinableArea", machinableArea)

      val validPosPoly = pcConfig.bufferFct(initialScene.targetGeometry, -t.d/2.0d).intersection(
        pcConfig.bufferFct(machinableArea,-t.d/2.0d))
      pGeo("validPosPoly", validPosPoly)

      val restBuffered = pcConfig.bufferFct(initialScene.getRestMultiGeo,t.d / 2)
      pGeo("restBuffered", restBuffered)

      val restfilterBuffer = pcConfig.bufferFct(pcConfig.bufferFct(initialScene.getRestMultiGeo,-t.ae),t.ae)
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
            asFloatList(path).map(i => new Coordinate(i(0), i(1))).toArray), gf)})

      (List(asFloatList(path)), newScene)
  }
  }

  def createMultiContourStep(t: CncTool): PathCoverageStep = {
    lazy val scStep: cncPathFct = singleContourStep(t)
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
        def performAndEvaluateStep(aggregatedPath: List[List[Float]], s: Cnc2DModel): (List[List[Float]], Cnc2DModel) = {
          val (path, newModel) = scStep(s, pcConfig)
          val diff = s.getRestMultiGeo.getArea - newModel.getRestMultiGeo.getArea
          if (diff < 2.0)
            (aggregatedPath, s)
          else {
            if(aggregatedPath.nonEmpty && path.nonEmpty && path.head.nonEmpty)
            performAndEvaluateStep(aggregatedPath ++ repositionPathSteps(aggregatedPath.last, path.head.head) ++ path.head, newModel)
            else
            performAndEvaluateStep(aggregatedPath ++ path.head, newModel)
          }
        }

        val (path, endScene) = performAndEvaluateStep(List.empty[List[Float]], initialScene)

        pGeo("path aggregated Multi", {
          new LineString(
            new CoordinateArraySequence(
              path.map(i => new Coordinate(i(0), i(1))).toArray), gf)
        })

        (List(path), endScene)
    }

    PathCoverageStep(Some(combinatorPcFunction), Some(t), List.empty[PathCoverageStep], "Contour-based machining step")
  }
}
