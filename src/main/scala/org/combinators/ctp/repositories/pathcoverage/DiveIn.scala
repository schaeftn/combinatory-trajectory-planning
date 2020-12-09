package org.combinators.ctp.repositories.pathcoverage

import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.math3.geometry.euclidean.threed.Vector3D
import org.apache.commons.math3.util.{FastMath, MathUtils}
import org.combinators.ctp.repositories.toplevel.PathCoverageStepConfig
import org.locationtech.jts.geom.{Coordinate, Geometry, LineString}
import org.locationtech.jts.linearref.LinearLocation
import org.locationtech.jts.util.GeometricShapeFactory

trait DiveIn extends JtsUtils {
  val tool: CncTool
  val config: PathCoverageStepConfig
  val lineString: LineString
  lazy val lineStringCoords = lineString.getCoordinates

  lazy val lmp: LocalMotionPrimitive = new CircularMotion {
    override val radius: Float = (tool.d / 2.0).toFloat
    override val pointClearance: Double = config.minPointClearanceOnPath
  }

  lazy val zCoords = lineString.getCoordinates.map(_.getZ)
  lazy val deltaZ = Math.abs(zCoords(1) - zCoords.head)

  lazy val LocalParamMultiplier = {
    deltaZ / tool.ap + 1 // end circle
  }

  lazy val imaginaryZFactor = (deltaZ + tool.ap) / deltaZ
  lazy val imaginaryZPoint = {
    val vector = new Vector3D(lineStringCoords(1).x - lineStringCoords.head.x,
      lineStringCoords(1).y - lineStringCoords.head.y,
      lineStringCoords(1).getZ - lineStringCoords.head.getZ)
    val newV = new Vector3D(imaginaryZFactor, vector)
    new Coordinate(lineStringCoords.head.x + newV.getX,
      lineStringCoords.head.y + newV.getY,
      lineStringCoords.head.getZ + newV.getZ)
  }

  def baseFct(t: Float): List[Float] = {
    val coordinate = LinearLocation.pointAlongSegmentByFraction(lineStringCoords.head, imaginaryZPoint, t)
    List(coordinate.x, coordinate.y, coordinate.getZ).map(_.toFloat)
  }

  def localFunction(t: Float): List[Float] = lmp.getLocalOffset(t)

  def completeFunction(t: Float): List[Float] = {
    val basePoint = if (t > 1.0 / imaginaryZFactor )
      List(lineStringCoords(1).x,lineStringCoords(1).y,lineStringCoords(1).getZ).map(_.toFloat)
    else
      baseFct(t)

    val zipped = (basePoint zip localFunction(t))
    zipped.map { case (a, b) => a + b }
  }

  lazy val numMulti = Math.ceil(lineString.getLength / tool.ap + 1).toInt
  lazy val numPointsLocal = Math.floor(2* Math.PI * lmp.radius / config.minPointClearanceOnPath).toInt
  lazy val totalNumPoints = numMulti * numPointsLocal

  def getPath : List[List[Float]] = (0 to totalNumPoints).map(i => completeFunction((i.toFloat/totalNumPoints.toFloat))).toList

  def getGeometry = {
    val gsf = new GeometricShapeFactory()
    if(tool != null)
      gsf.setSize(tool.d)
    else
      logger.info("fppp")
    gsf.setNumPoints(100)
    gsf.setCentre(lineString.getCoordinates()(1))
    gsf.createCircle()
  }
}

object TestDiveIn  extends App with LazyLogging with JtsUtils {
  val ls = getNewLineString(new Coordinate(0.0,0.0,20.0), new Coordinate(0.0,0.0,0.0))
  val mp = new DiveIn {
    override val tool: CncTool =  CncTool(
      d = 12.0f,
      ae = 12.0f,
      ap = 6.0f,
      vf = 1.2750f,
      n = 7960,
      description = "Alu Roughing, 12mm, Stirnfräsen, TODO Werte aktualisieren",
      idString = "123")
    override val config: PathCoverageStepConfig = PathCoverageStepConfig()
    override val lineString: LineString = ls
  }
  logger.info(s"Path: \r\n ${mp.getPath}")
}
