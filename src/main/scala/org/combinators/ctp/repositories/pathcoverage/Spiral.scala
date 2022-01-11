package org.combinators.ctp.repositories.pathcoverage

import org.combinators.ctp.repositories.toplevel.{Cnc2DModel, PathCoverageStepConfig}
import org.locationtech.jts.algorithm.construct.MaximumInscribedCircle
import org.locationtech.jts.geom.impl.CoordinateArraySequence
import org.locationtech.jts.geom.{Coordinate, LineString, MultiLineString}
import org.locationtech.jts.io.WKTReader
import org.locationtech.jts.util.GeometricShapeFactory

trait Spiral {
  val minRadius: Double
  val maxRadius: Double
  val midPoint: List[Double]
  val ae: Float
  val config: PathCoverageStepConfig

  //    if (Math.abs(Math.sin(t)) < 0.5)
//    0.5 else Math.abs(Math.sin(t))

  def morphingFunction(t: Double) = 1.0

  lazy val linearRadiusGrowFactor = (maxRadius - minRadius) + ae // divided by Parameter interval 1.0f
  lazy val omega: Double = ((linearRadiusGrowFactor / ae)) * math.Pi * 2 // rad per parameter 1.0f 100*pi = 50 circles
  lazy val linearRadiusGrowFactorOh = maxRadius * math.Pi * 2
  lazy val resolution = 360

  def direction(t: Double): List[Double] = List(math.cos(omega * t), math.sin(omega * t), 0.0) //TODO
  def radius(t: Double) = List(maxRadius, minRadius + (linearRadiusGrowFactor) * t * morphingFunction(omega*t) ).min
  def position(t: Double):List[Float] = (midPoint zip direction(t).map(i => i * radius(t))).
    map { case (mpCoord, vecCoord) => (mpCoord + vecCoord).toFloat }

  def discretePath: List[List[Float]] = (0 until resolution).map(i => position(1.0f * i / resolution)).filter(_.nonEmpty).toList
}

object RunSpiral extends App with JtsUtils {
    val pcConfig = PathCoverageStepConfig()

  val wktReader = new WKTReader()
  val wktStrP1 = """POLYGON ((10 10, 10 40, 40 40, 40 10, 10 10))"""
  val tgtGeo = wktReader.read(wktStrP1)

  val initialScene = Cnc2DModel(boundaries = List(0.0f, 50.0f, 0.0f, 50.0f),
    targetGeometry = tgtGeo, rest = List(tgtGeo), machined = List(), machinedMultiPolygon = gf.createPolygon(), initialMachined = emptyGeometry)

  val t = CncTool(4.0f, 2.0f, 4.0f, 1.280f, 8000,
    "Alu finishing, d 8mm, ae 4mm, vf 1280 mm/min, n 8000", "2 Z S8000", "Alu Finishing")
  val largestEmptyCircle = new MaximumInscribedCircle(initialScene.rest.head, 1.0)
  val c1 = largestEmptyCircle.getCenter.getCoordinate
  val c11 = new Coordinate(c1.x, c1.y, 20.0f)
  val c2 = largestEmptyCircle.getCenter.getCoordinate
  val c22 = new Coordinate(c2.x, c2.y, 0.0f)

  val spiral = new Spiral {
    override val minRadius: Double = t.d/2
    override val maxRadius: Double = largestEmptyCircle.getRadiusLine.getLength-t.d/2
    override val midPoint: List[Double] = List(c22.x,c22.y,c22.getZ)
    override val ae: Float = t.ae
    override val config: PathCoverageStepConfig = pcConfig
  }

  lazy val spiralGeo = {
    val gsf = new GeometricShapeFactory
    gsf.setSize(largestEmptyCircle.getRadiusLine.getLength*2)
    gsf.setNumPoints(100)
    gsf.setCentre(largestEmptyCircle.getCenter.getCoordinate)
    gsf.createCircle()
  }

 //  pGeo("mlPath", spiral.discretePath)

  val toPaths =
    new LineString(
      new CoordinateArraySequence(
        spiral.discretePath.map(c => new Coordinate(c(0), c(1))).toArray)
      , gf)

pGeo("wktStrP1", tgtGeo)
  pGeo("spiralGeo", spiralGeo)
  pGeo("toPaths", toPaths)
}