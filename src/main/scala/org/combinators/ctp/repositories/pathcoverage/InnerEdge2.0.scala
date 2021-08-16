package org.combinators.ctp.repositories.pathcoverage

import com.typesafe.scalalogging.LazyLogging
import org.combinators.ctp.repositories.pathcoverage.RunTrochoidalPath.trochPrimitive
import org.combinators.ctp.repositories.scene.SceneUtils
import org.combinators.ctp.repositories.toplevel.{Cnc2DModel, PathCoverageStepConfig}
import org.locationtech.jts.algorithm.MinimumBoundingCircle
import org.locationtech.jts.geom.impl.CoordinateArraySequence
import org.locationtech.jts.geom.{Coordinate, Geometry, LineString, MultiLineString}
import org.locationtech.jts.io.WKTReader


trait DirectedRadial extends LazyLogging with JtsUtils {
  val model: Cnc2DModel
  lazy val p1: Geometry = model.rest.head
  val tool: CncTool
  val config: PathCoverageStepConfig

  lazy val selectBoundaryLine: Option[LineString] = {
    val intersectionLineCandidate: Geometry = model.machinedMultiPolygon.intersection(p1)
    pGeo("p1", p1)
    pGeo("machMultiPoly", model.machinedMultiPolygon)
    pGeo("il Candidate", intersectionLineCandidate)
    if (intersectionLineCandidate.isEmpty) None
    else
      intersectionLineCandidate.getGeometryType match {
        case "LineString" => Some(intersectionLineCandidate.asInstanceOf[LineString])
        case "MultiLineString" =>
          Some(multiGeoToGeoList(intersectionLineCandidate.asInstanceOf[MultiLineString]).
            maxBy(_.getLength).asInstanceOf[LineString])
        case "GeometryCollection" => filterGeoCollectionLsOnly(intersectionLineCandidate)
        case _ => None
      }
  }

  lazy val directionY: Boolean = selectBoundaryLine.map(_.getEnvelopeInternal).forall(i => i.getWidth > i.getHeight)

  lazy val getSteps: List[List[Float]] = {
    val mbc = new MinimumBoundingCircle(selectBoundaryLine.getOrElse(emptyGeometry))

    pGeo("selectedBoundaryLine", selectBoundaryLine.getOrElse(emptyGeometry))

    val numberOfSteps = Math.ceil(mbc.getRadius / tool.ae).toInt
    val stepSize = tool.ae
    val firstCirc = translateGeo(mbc.getCircle, 0.0d, -mbc.getRadius * 2.0d)
    val firstLs = firstCirc

    val env = firstCirc.getBoundary.getEnvelopeInternal

    logger.debug(s"env: $env")
    if (directionY)
      env.expandBy(0.0f, 100.0f)
    else
      env.expandBy(100.0f, 0.0f)

    val coords = if (directionY)
      Array(new Coordinate(env.getMinX, env.getMinY),
        new Coordinate(env.getMinX, model.boundaries(3)),
        new Coordinate(env.getMaxX, model.boundaries(3)),
        new Coordinate(env.getMaxX, env.getMinY),
        new Coordinate(env.getMinX, env.getMinY)
      )
    else
      Array(new Coordinate(env.getMinX, env.getMinY),
        new Coordinate(env.getMinX, env.getMaxY),
        new Coordinate(model.boundaries(1), env.getMaxY),
        new Coordinate(model.boundaries(1), env.getMinY),
        new Coordinate(env.getMinX, env.getMinY)
      )

    val poly = gf.createPolygon(coords)

    val testWp = filterGeoCollectionPolyOnly(poly.intersection(model.targetWorkpiece))
    pGeo("testWp", testWp)

    def geoHitsBounds(g: Geometry): Boolean = g.isWithinDistance(testWp, 0)

    def geoOutOfPoly(g: Geometry): Boolean = !g.isWithinDistance(p1, 0)

    @scala.annotation.tailrec
    def replaceLast(c: List[Geometry]): List[Geometry] = {
      val circ = c.last
      if (geoHitsBounds(circ)) {
        val xDisplacement = if (directionY) 0.0 else -0.01
        val yDisplacement = if (directionY) -0.01 else 0.0
        replaceLast(c.dropRight(1) :+ translateGeo(circ, xDisplacement, yDisplacement))
      } else {
        c
        }
      }

    @scala.annotation.tailrec
    def spawnCircles(circles: List[Geometry]): List[Geometry] = {
      if ((geoHitsBounds(circles.last)) ||
        (circles.size > 1 && geoOutOfPoly(circles.last)) || circles.isEmpty || circles.head.isEmpty)
        replaceLast(circles)
      else {
        val newTab = if (geoOutOfPoly(circles.last)) circles.dropRight(1) else circles
        if (directionY)
          spawnCircles(newTab :+ translateGeo(circles.last, 0.0d, tool.ae))
        else
          spawnCircles(circles :+ translateGeo(circles.last, tool.ae, 0.0d))
      }
    }

    logger.debug(s"p1: $p1")
    logger.debug(s"firstLs: $firstLs")
    val circs = spawnCircles(List(firstLs))
    logger.debug(s"circs: $circs")


    val tempDebug = circs.map(g => gf.createPolygon(g.getCoordinates))
    logger.debug(s"tempDebug: $tempDebug")

    val unbufferedCircs = tempDebug.map(g => g.buffer(-tool.d.toDouble / 2.0d))
    logger.debug(s"unbufferedCircs: $unbufferedCircs")

    val upper = unbufferedCircs.map(_.norm).map(circleUpperLineString)
    logger.debug(s"upper: $upper")

    val sortedCoords = if (directionY)
      upper.map(geo => asFloatList(geo.getCoordinates.sortBy(_.x)))
    else
      upper.map(geo => asFloatList(geo.getCoordinates.sortBy(_.y)))

    logger.debug("getSteps done")
    sortedCoords.map(i => i ++ i.headOption.toList).reduceOption(_ ++ _).getOrElse(List.empty[List[Float]])
  }

  lazy val machinedGeo = {
    logger.debug("getting Machined geo")
    val geo = config.bufferFct(getNewLineString(getSteps.map(asCoordinate).toArray), tool.d / 2.0)
    logger.debug("after Machined geo")
    pGeo("machinedGeo steelradial", geo)
    geo
  }
}

object InnerEdge2Test extends App with JtsUtils {
  val wktReader = new WKTReader()

  //val tgtGeo = wktReader.read("""POLYGON ((10 0, 10 8, 10 20, 10.151922225952148 21.736482620239258, 10.603074073791504 23.42020034790039, 11.339746475219727 25, 12.339555740356445 26.427875518798828, 13.572123527526855 27.660444259643555, 15 28.660253524780273, 16.57979965209961 29.396926879882812, 18.263517379760742 29.84807777404785, 20 30, 34 30, 35.04188919067383 29.90884780883789, 36.052120208740234 29.63815689086914, 36.68108367919922 29.344865798950195, 37 29.196151733398438, 37.856727600097656 28.596267700195312, 38.59626770019531 27.856725692749023, 39.19615173339844 27, 39.63815689086914 26.052120208740234, 39.90884780883789 25.041889190673828, 40 24, 40 14, 39.939231872558594 13.30540657043457, 39.75876998901367 12.631918907165527, 39.46410369873047 12, 39.064178466796875 11.428850173950195, 38.57115173339844 10.935821533203125, 38 10.535898208618164, 37.368080139160156 10.241230010986328, 36.6945915222168 10.060769081115723, 36 10, 30 10, 30 -0, 10 0))""".stripMargin)
  val tgtGeo = wktReader.read("""POLYGON ((10 40, 40 40, 40 20, 10 20, 10 40))""".stripMargin)
  val machinedGeo = wktReader.read("""POLYGON ((10 20, 40 20, 40 0, 10 0, 10 20))""")
  val scene = Cnc2DModel(boundaries = List(0.0f, 50.0f, 0.0f, 50.0f),
    targetGeometry = tgtGeo.union(machinedGeo), rest = List(tgtGeo), machined = List(),
    machinedMultiPolygon = emptyGeometry, initialMachined = emptyGeometry).withInitialMachinedGeo(machinedGeo)


  val ie = new DirectedRadial {
    override val model: Cnc2DModel = scene
    override val tool: CncTool = CncTool(12.0f, 2.0f, 6.0f, 1.2750f, 7960,
      "Alu Roughing, 12mm, Stirnfräsen, TODO Werte aktualisieren", "1 Z S2000")
    override val config: PathCoverageStepConfig = PathCoverageStepConfig()
  }


  val toPaths =
    new LineString(
      new CoordinateArraySequence(
        ie.getSteps.map(c => new Coordinate(c(0), c(1))).toArray)
      , gf)

  logger.debug(s"mlString: ${toPaths}")
}