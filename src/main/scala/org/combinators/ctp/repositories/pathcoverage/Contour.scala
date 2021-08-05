package org.combinators.ctp.repositories.pathcoverage

import com.typesafe.scalalogging.LazyLogging
import org.combinators.ctp.repositories.cncPathFct
import org.combinators.ctp.repositories.toplevel.{Cnc2DModel, PathCoverageStep, PathCoverageStepConfig}
import org.locationtech.jts.geom.impl.CoordinateArraySequence
import org.locationtech.jts.geom.{Coordinate, Geometry, LineString, MultiLineString, Point, Polygon}
import org.locationtech.jts.io.WKTReader
import org.locationtech.jts.operation.distance.DistanceOp
import org.locationtech.jts.operation.overlay.snap.GeometrySnapper

import scala.annotation.tailrec

trait Contour extends LazyLogging with JtsUtils {

  lazy val finishingContourStep: CncTool => cncPathFct = { t => {
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

      val restBuffered = pcConfig.bufferFct(initialScene.getRestMultiGeo, t.d / 2.0f)
      pGeo("restBuffered", restBuffered)

      val restfilterBuffer = pcConfig.bufferFct(pcConfig.bufferFct(initialScene.getRestMultiGeo, -t.ae), t.ae)
      pGeo("restfilterBuffer", restfilterBuffer)

      val restBufferedTest = pcConfig.bufferFct(
        pcConfig.bufferFct(
          pcConfig.bufferFct(initialScene.getRestMultiGeo, -t.ae), t.ae), t.d / 2.0f - t.ae)
      pGeo("restBufferedTest", restBufferedTest)

      val restBufferedInters = pcConfig.bufferFct(
        pcConfig.bufferFct(restBuffered.intersection(invalidToolPositions), -0.0001), 0.0002) //Randbereiche
      pGeo("restBufferedInters", restBufferedInters)

      val fullStuff = restBufferedTest.union(restBufferedInters)
      pGeo("fullStuff", fullStuff)
      val filteredFull = filterLsAndReturnMultiPoly(fullStuff)
      pGeo("filteredFull", filteredFull)

      val gs = new GeometrySnapper(validPosPoly)
      val newValidPosPoly = gs.snapTo(filteredFull, 0.01d)

      val f1 = newValidPosPoly.difference(filteredFull) // mit difference noch holes benötigt?
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
      logger.debug(s"Contour after machinedGeo")

      pGeo("path aggregated single", {
        new LineString(
          new CoordinateArraySequence(
            asFloatList(path).map(i => new Coordinate(i(0), i(1))).toArray), gf)
      })

      (List(asFloatList(path)), newScene)
  }
  }

  def singleContourStep(poly: Geometry, t: CncTool, initialScene: Cnc2DModel, pcConfig: PathCoverageStepConfig, lastGeo: Geometry): (List[List[Float]], Geometry) = {
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

    val polyBuffered = pcConfig.bufferFct(poly, t.d / 2.0f)
    pGeo("polyBuffered", polyBuffered)

    val restBufferedInters = pcConfig.bufferFct(
      pcConfig.bufferFct(polyBuffered.intersection(validPosPoly), -0.0001), 0.0002) //Randbereiche
    pGeo("restBufferedInters", restBufferedInters)

    val filteredFull = polyBuffered.intersection(filterLsAndReturnMultiPoly(restBufferedInters))
    pGeo("filteredFull", filteredFull)

    val f1 = smartCastToPolygon(filteredFull.intersection(validPosPoly), lastGeo).getExteriorRing.
      intersection(validPosPoly)
    pGeo("f1", f1)

    val toolPath = getLongestLineString(f1, pcConfig)
    pGeo("toolPath", toolPath)

    //TODO filter toolpath. must not be further away than ...
    val toolPath2 = toolPath // asLineString(asFloatList(toolPath.getCoordinates).filter(c => asPoint(c).distance(poly)<= t.d/2.0 + pcConfig.maxPointClearanceOnPath))

    // prio2 fix and test filter for repositioning path
    val path = toolPath2.getCoordinates

    val toolpathBuffered = pcConfig.bufferFct(toolPath2, t.d / 2.0) //Rundungsfehler
    pGeo("toolpathBuffered", toolpathBuffered)

    pGeo("path aggregated single", {
      new LineString(
        new CoordinateArraySequence(
          asFloatList(path).map(i => new Coordinate(i(0), i(1))).toArray), gf)
    })

    (asFloatList(path), toolpathBuffered)
  }

  def singleContourStep2(poly: Geometry, t: CncTool, initialScene: Cnc2DModel,
                         pcConfig: PathCoverageStepConfig, lastGeo: Geometry): (List[List[Float]], Geometry, Geometry) = {
    assert(poly.getGeometryType == "Polygon")
    //initialScene.rest.foreach(a => pGeo("initialScene Rest", a))
    pGeo("targetWorkpiece", initialScene.targetWorkpiece)
    pGeo("initialScene.getMachinedMultiGeo", initialScene.getMachinedMultiGeo)
    pGeo("poly", poly)
    pGeo("lastGeo", lastGeo)

    val invalidToolPositions = initialScene.targetWorkpiece.buffer(t.r)
    pGeo("invalidToolPositions", invalidToolPositions)

    val invalidToolPositionsBuffered = invalidToolPositions.buffer(-0.001)
    pGeo("invalidToolPositionsBuffered", invalidToolPositionsBuffered)

    val machinableArea = initialScene.getMachinedMultiGeo.buffer(t.ae)
    pGeo("machinableArea", machinableArea)

    val machinableAreaStrip = machinableArea.intersection(poly)
    pGeo("machinableAreaStrip", machinableAreaStrip)

    val machinableAreaStripPolygonsOnly = gf.createMultiPolygon(
      getGeoListFromGeo(machinableAreaStrip).filter(g => g.getGeometryType == "Polygon").
        map(_.asInstanceOf[Polygon]).toArray)
    pGeo("machinableAreaStripPolygonsOnly", machinableAreaStripPolygonsOnly)

    val machinableAreaStripMinusWorkpiece = machinableAreaStripPolygonsOnly.difference(initialScene.targetWorkpiece)
    pGeo("machinableAreaStripMinusWorkpiece", machinableAreaStripMinusWorkpiece)

    // Select polygon with largest area
    val selectedPoly = smartCastToPolygon(machinableAreaStripMinusWorkpiece, lastGeo)
    pGeo("selectedPoly", selectedPoly)

    def getValidTpString(p_selectedPolygon: Polygon, model: Cnc2DModel): Geometry ={
      val machinableLineString = p_selectedPolygon.getExteriorRing
      pGeo("machinableLineString", machinableLineString)

      val machinedGrown = pcConfig.bufferFct(model.getMachinedMultiGeo, 0.001)
      pGeo("machinedGrown", machinedGrown)

      val gsMachinableLineString = new GeometrySnapper(machinableLineString)
      val machinableLineStringSnapped = gsMachinableLineString.snapTo(machinedGrown,0.01d)
      pGeo("machinableLineStringSnapped", machinableLineStringSnapped)
      val bclearedMachinableLineStringSnapped = machinableLineStringSnapped.buffer(0)
      pGeo("bclearedMachinableLineStringSnapped", bclearedMachinableLineStringSnapped)

      //TODO here
      val onlyValidMs = machinableLineStringSnapped.buffer(0).difference(machinedGrown.buffer(0))
      pGeo("onlyValidString", onlyValidMs)

      val validTpString = getTopLineString(onlyValidMs, pcConfig)
      pGeo("validTpString", validTpString)

      val gs = new GeometrySnapper(validTpString)
      val validTpStringClean = gs.snapToSelf(0.01, true)
      pGeo("validTpStringClean", validTpStringClean)

      val toolPath = validTpStringClean.buffer(t.d / 2.0f, 8, 2).
        asInstanceOf[Polygon].getExteriorRing.difference(invalidToolPositionsBuffered)
      pGeo("toolPath", toolPath)

      toolPath
    }

    def getValidTpString2(p_selectedPolygon: Polygon, model: Cnc2DModel): Geometry ={
      logger.debug(s"bufferVal: ${-(t.d / 2.0f - t.ae)}")
      pGeo("machinedMultiGeo", model.getMachinedMultiGeo)
      val toolPathsAe = model.getMachinedMultiGeo.buffer(-(t.d / 2.0f - t.ae))
      pGeo("toolPathsAe", toolPathsAe)
      val toolPathsAeDiffInvalid = toolPathsAe.difference(invalidToolPositions)
      pGeo("toolPathsAeDiffInvalid", toolPathsAeDiffInvalid)
      val toolPathsAeStrings = smartCastToPolygon(toolPathsAeDiffInvalid, p_selectedPolygon).getExteriorRing
      pGeo("toolPathsAeStrings", toolPathsAeStrings)
      toolPathsAeStrings.intersection(p_selectedPolygon.buffer(t.r))
    }

    val toolPath = getValidTpString2(selectedPoly, initialScene)
    pGeo("getValidTp Result", toolPath)

    val selectedToolPath = toolPath.getGeometryType match {
      case "LineString" => toolPath
      case "MultiLineString" => getGeoListFromGeo(toolPath).filter(_.getLength > pcConfig.pathIgnoreVal).
        minBy(_.distance(lastGeo))
      case "GeometryCollection" => getGeoListFromGeo(toolPath).filter(_.getGeometryType =="LineString").
        filter(_.getLength > pcConfig.pathIgnoreVal).
        minBy(_.distance(lastGeo))
    }

    val gSnap = new GeometrySnapper(selectedToolPath)
    val tpSnapped = gSnap.snapToSelf(0.001, true)
    pGeo("tpSnapped", tpSnapped)

    val miniMalTp = tpSnapped.intersection(initialScene.getMachinedMultiGeo)

    val path = miniMalTp.getCoordinates

    val toolpathBuffered = miniMalTp.buffer(t.r)
    pGeo("toolpathBuffered", toolpathBuffered)

    pGeo("path aggregated single", {
      new LineString(
        new CoordinateArraySequence(
          asFloatList(path).map(i => new Coordinate(i(0), i(1))).toArray), gf)})

    val reservePoly = getGeoListFromGeo(poly.difference(toolpathBuffered)).filter {
      g => g.getGeometryType == "Polygon"}.map(_.asInstanceOf[Polygon])

    (asFloatList(path), toolpathBuffered, reservePoly.maxBy(_.getArea))
  }

  def createFinishContourStep(t:CncTool) : PathCoverageStep = {
    lazy val combinatorPcFunction: cncPathFct = finishingContourStep(t)

    PathCoverageStep(Some(combinatorPcFunction), Some(t), List.empty[PathCoverageStep], "Aluminum Finishing")
  }

  def findConnectorPath(newModel: Cnc2DModel, aggregatedPath: List[List[List[Float]]], newPath: List[List[Float]], t: CncTool):
  List[List[List[Float]]] = {
    //find first point
    val firstPointOption = aggregatedPath.lastOption.flatMap(_.lastOption)
    val returnVal = firstPointOption match {
      case None => List.empty[List[List[Float]]] // StartingPoint: Or: Go to machined area
      case Some(startPoint) =>
        // find second point. It should be inside of the machined area and close to the first path point
        val endPoint = newPath.head
        // find closest point in polygon
        val machinedToolAreas = newModel.getMachinedMultiGeo.buffer(-t.r)
        pGeo("aggregatedPath in findConnectorPath", getGeoCollection(aggregatedPath.map(asLineString)))
        pGeo("machinedToolAreas", machinedToolAreas)
        val selectedPoints = DistanceOp.nearestPoints(asPoint(endPoint), machinedToolAreas)
        val selectedEndPoint = if (selectedPoints.size > 1) coordAsFloatList(selectedPoints(1)) else endPoint

        val startP = asPoint(startPoint)
        val endP = asPoint(selectedEndPoint)

        if (asLineString(List(startPoint, selectedEndPoint)).coveredBy(machinedToolAreas)) {
          // aggregated path and new path
          //check if new selectedEndpoint is first point in path...
          List.empty[List[List[Float]]]
        }else{
          // select current polygon start
          if (pointsInSamePolygon(startPoint, selectedEndPoint, machinedToolAreas)) {
            //select Polygon
            val l = getGeoListFromGeo(machinedToolAreas).filter(g => geosAreNear(g,startP) && geosAreNear(g,endP))
            val returnValue = if (l.nonEmpty) {
              val exRing = l.maxBy(_.getArea).asInstanceOf[Polygon].getExteriorRing
              val coordListWIndex = exRing.getCoordinates.zipWithIndex

              def getNearestPointIndex(p: Point) =
                coordListWIndex.map { case (a, index) => (a.distance(p.getCoordinate), index) }.minBy(_._1)._2

              val endIndex = getNearestPointIndex(endP)
              val startIndex = getNearestPointIndex(startP)

              val firstList = (coordListWIndex ++ coordListWIndex.reverse).drop(startIndex).reverse.dropWhile(_._2 != endIndex).reverse.map(_._1)
              val secondList = (coordListWIndex ++ coordListWIndex.reverse).drop(endIndex).reverse.dropWhile(_._2 != startIndex).reverse.map(_._1)

              if(secondList.length == 1)
                (coordListWIndex ++ coordListWIndex.reverse).foreach(println)

              val ls1 = gf.createLineString(firstList)
              val ls2 = gf.createLineString(secondList)
              val asd = List(asFloatList(List(ls1, ls2).minBy(_.getLength).getCoordinates))
              asd
            }
            else {
              List.empty[List[List[Float]]]
            }
            returnValue
          }else {
            logger.error(s"ERROR Contour paths cannot be connected")
            pGeo("machinedToolAreas",machinedToolAreas)
            pGeo("StartPoint Endpoint", getGeoCollection(List(startPoint, selectedEndPoint).map(asPoint)))
            pGeo("Aggregated path", getGeoCollection(aggregatedPath.map(asLineString)))
            pGeo("new path", asLineString(newPath))
            List.empty[List[List[Float]]]
          }
        }
    }
    returnVal
  }

  def createMultiContourStep(t: CncTool): PathCoverageStep = {
    lazy val combinatorPcFunction: cncPathFct = {
      (initialScene: Cnc2DModel, pcConfig: PathCoverageStepConfig) =>
        pGeo("initialScene Rest", getGeoCollection(initialScene.rest))
        pGeo("targetWorkpiece", {
          initialScene.targetWorkpiece
        })
        pGeo("initialScene.getMachinedMultiGeo", initialScene.getMachinedMultiGeo)

        val invalidToolPositions = initialScene.targetWorkpiece.buffer(t.d / 2.0d)
        pGeo("invalidToolPositions", invalidToolPositions)

       // val selectedRestGeo: Option[Geometry] = Option(initialScene.rest).filter(_.nonEmpty).map(_.maxBy(_.getArea))

//        /**
//         * extrahiert für die gewählte fräsfläche jene Grenze, welche an machined Bereich grenzt
//         * None, if initialscene.rest is empty
//         */
//        lazy val extractInitialMachinableFrontier: Option[LineString] =
//          selectedRestGeo.flatMap(getLongestExteriorFromPolygon)
        @tailrec
        def performAndEvaluateStep(polyOption: Option[Geometry],
                                   aggregatedPath: List[List[List[Float]]],
                                   s: Cnc2DModel): (List[List[List[Float]]], Cnc2DModel) = {
          logger.debug(s"saniPoly: \r\n${polyOption.get}")
          val poly = saniPoly(polyOption.get, pcConfig)

          logger.debug(s"saniPoly: \r\n$poly")
          if (polyOption.isEmpty || poly.isEmpty || poly.getArea < 0.01) {
            (aggregatedPath, s)
          } else {
            val lastPoint: Option[List[Float]] = aggregatedPath.lastOption.flatMap(_.lastOption)
            val lastGeo = lastPoint.map(lp => gf.createPoint(asCoordinate(lp))).getOrElse(emptyGeometry)
            val cleanedLastGeo = smartCastToPolygon(poly, lastGeo)
            pGeo("cleanedLastGeo",cleanedLastGeo)
            val (path, bufferedToolPath, sPoly) = singleContourStep2(cleanedLastGeo, t, s, pcConfig, lastGeo)
            pGeo("outerLoop: reserved Polygon from singleStep",sPoly)
            logger.debug(s"path Ls: ${asLineString(path)}")
            pGeo("poly",poly)
            pGeo("bufferedToolPath",bufferedToolPath)
            val diff = poly.getArea - poly.difference(bufferedToolPath).getArea
            if (diff < 0.5) // Abbruchkrit
              (aggregatedPath, s)
            else {
              pGeo("outer: sPoly", sPoly)
              pGeo("outer: bufferedToolPath", bufferedToolPath)

              val bufferedSPoly = sPoly.buffer(0)

              val gs = new GeometrySnapper(bufferedToolPath)
              val bufferedToolPathSnappedToPoly = gs.snapTo(bufferedSPoly, 0.001d)

              val newPoly = bufferedSPoly.difference(bufferedToolPathSnappedToPoly.buffer(0))
              val (selectedPoly, newModel) = newPoly.getGeometryType match {
                case "MultiPolygon" =>
                  val selPoly:Polygon = if (aggregatedPath.isEmpty)
                    getGeoListFromGeo(newPoly).
                      map(_.asInstanceOf[Polygon]).maxBy(_.getArea)
                  else
                    getGeoListFromGeo(newPoly).
                      map(_.asInstanceOf[Polygon]).maxBy(_.getArea)
                  //minBy(i => distanceTasd()0oPoint(i, aggregatedPath.last.last))
                  (selPoly,s.withMachinedGeo(bufferedToolPath))
                case "Polygon" => (newPoly, s.withMachinedGeo(bufferedToolPath))
              }

              val newAggregatedPath = aggregatedPath ++ findConnectorPath(newModel, aggregatedPath,path,t) ++ List(path)
              performAndEvaluateStep(Some(selectedPoly),newAggregatedPath , newModel)
            }
          }
        }

        val foo = initialScene.rest.headOption.map(g => smartCastToPolygon(g, emptyGeometry))
        pGeo("outer: polygon foo", foo.getOrElse(emptyGeometry))
        val (path, endScene) = performAndEvaluateStep(foo, List.empty[List[List[Float]]], initialScene)

        pGeo("path aggregated Collection", getGeoCollection(path.map(p => {
          new LineString(
            new CoordinateArraySequence(
              p.map(i => new Coordinate(i(0), i(1))).toArray), gf)
        })))

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

  val res = PathCoverageResult(model, PathCoverageStepConfig(), cont)

  logger.info(s"res.computeModelHistory._1.last.getRestMultiGeo: \r\n ${res.computeModelHistory._1.last._1.getRestMultiGeo}")
  val toPaths = res.pathList.map{ singleCompPathlist =>
    new LineString(
      new CoordinateArraySequence(
        singleCompPathlist.map(c => new Coordinate(c(0), c(1))).toArray)
      , gf)}

  val mlString = new MultiLineString(toPaths.toArray,gf)

  logger.info(s"mlString: \r\n $mlString")

  // s: Cnc2DModel, config: PathCoverageStepConfig, l: List[PathCoverageStep]
}