package org.combinators.ctp.repositories.toplevel

import java.io.FileInputStream
import java.util.Properties

import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.math3.util.{FastMath, MathUtils}
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.geometry.{PpPolyhedronMesh, PpSurfaceMesh}
import org.combinators.ctp.repositories.pathcoverage.{CncTool, JtsUtils}
import org.locationtech.jts.geom.{ Geometry, Polygon}
import org.locationtech.jts.simplify.DouglasPeuckerSimplifier
import scalax.collection.Graph
import scalax.collection.edge.WUnDiEdge
import org.locationtech.jts.geom.util.AffineTransformation
import org.locationtech.jts.io.WKTReader
import scala.io.Source
import scala.language.implicitConversions
import scala.util.{Failure, Try}


case class Scene(boundaries: List[Float], obstacles: List[MqttCubeData]) {
  def empty = Scene(List.empty, List.empty)
}

trait MachineAccelerationModel extends JtsUtils {
  def machineModelAccX(v: Double): Double

  def machineModelAccY(v: Double): Double

  def vfMaxByAngle(angle: Double): Double

  def newVf(angle: Double, startV: Double): Double

  val deltaT: Double
}

trait PathCoverageStepConfig extends JtsUtils with MachineAccelerationModel {
  val minPointClearanceOnPath: Float
  val pathIgnoreVal: Float
  val areaIgnoreVal: Float
  val xAccLookup: List[(Float, Float, Float)]
  val yAccLookup: List[(Float, Float, Float)]

  def bufferFct(g: Geometry, d: Double): Geometry = paraBuffer(minPointClearanceOnPath)(g, d)
}

object PathCoverageStepConfig {
  def apply(pathRef: Boolean = false): PathCoverageStepConfig = new PathCoverageStepConfig {
    val pathRefinement = pathRef
    val minPointClearanceOnPath: Float = 0.01f
    val maxPointClearanceOnPath: Float = 0.1f
    val min_ae: Float = 0.01f
    val areaIgnoreVal: Float = 5.0f
    val pathIgnoreVal: Float = 0.5f


    @scala.annotation.tailrec
    override def machineModelAccX(velocity: Double): Double = {
      velocity match {
        case v if 0.0 <= v && v < 1.0 => -0.0624 * Math.pow(v, 2) + 0.2603 * v + 0.0216
        case v if 1.0 <= v && v <= 10.0 => -0.0027 * Math.pow(v, 2) + 0.0880 * v + 0.1536
        case v if v > 10.0 => machineModelAccX(10.0)
        case _ => machineModelAccX(0.0)
      }
    }

    @scala.annotation.tailrec
    override def machineModelAccY(velocity: Double): Double = {
      velocity match {
        case v if 0.0 <= v && v < 1.0 => -0.1979 * Math.pow(v, 2) + 0.3970 * v + 0.0184
        case v if 1.0 <= v && v <= 10.0 => -0.0036 * Math.pow(v, 2) + 0.0967 * v + 0.1427
        case v if v > 10.0 => machineModelAccY(10.0)
        case _ => machineModelAccY(0.0)
      }
    }

    //mm/min

    override def vfMaxByAngle(angle: Double): Double = {
      lookupMaxVfByAngle(angle)

//      val normalizedAngle = Math.PI - Math.abs(MathUtils.normalizeAngle(angle, 0.0))
//      val normalizedDegree = FastMath.toDegrees(normalizedAngle)
//      if (normalizedDegree < 1)
//        Double.PositiveInfinity
//      else {
//        val vf = (-14.8311 / (Math.pow(normalizedDegree, 2)) + 28.4664 / normalizedDegree - 0.1496)
//        // if(vf < 0)
//        //   logger.info(s"vf lt zero: angle: $angle, normalizedDegree: $normalizedDegree, vf: $vf")
//        vf
//      }
    }

    def lookupMaxVfByAngle(angle:Double): Float ={
      val normalizedAngle = Math.PI - Math.abs(MathUtils.normalizeAngle(angle, 0.0))
      val normalizedDegree = FastMath.floor(FastMath.toDegrees(normalizedAngle)).toInt
      vByAngleLookup(normalizedDegree)
    }

    lazy val vByAngleLookup = setUpMaxByAngleNormalized()

    def retrieveLookup(machineModelAcc: Double => Double,
                       xDir: Boolean): List[(Float, Float, Float)] = {
      val fileName = if (xDir) "accmodels/xDirection.txt" else "accmodels/yDirection.txt"

      val src = Source.fromResource(fileName)
      if (src.iter.hasNext) {
        val tuples = src.getLines().toList.map {
          s => s.split(",").map(_.toFloat)
        }.map(i => (i.head, i(1), i(2)))
        src.close
        tuples
      } else {
        logger.warn("Acceleration model file not found. Computing lookup table.")
        buildLookup(machineModelAcc)
      }
    }

    def buildLookup(machineModelAccX: Double => Double): List[(Float, Float, Float)] = {
      logger.warn("Building acc models from scratch.")
      val deltaT = 0.00005f // 50 ms
      (0 to 100000).foldLeft((List[(Float, Float, Float)]((0.0f, 0.0f, 0.0f)), (0.0f, 0.0f, 0.0f))) {
        case ((a, lastBufferedElement), newIndex) =>
          val vNew: Float = lastBufferedElement._1 +
            deltaT * machineModelAccX(lastBufferedElement._1).toFloat * 60.0f * 1000.0f
          val tNew: Float = lastBufferedElement._2 + deltaT
          val sNew: Float = lastBufferedElement._3 + deltaT * lastBufferedElement._1 / 60
          if (sNew - a.last._3 < 0.1)
            (a, (vNew, tNew, sNew))
          else
            (a :+ (vNew, tNew, sNew), (vNew, tNew, sNew))
      }._1
    }

    val xAccLookup: List[(Float, Float, Float)] = {
      val l = retrieveLookup(machineModelAccX, xDir = true)
      logger.debug(s"Last Element full xList ${l.last}")
      l
    }

    val yAccLookup: List[(Float, Float, Float)] = {
      retrieveLookup(machineModelAccY, xDir = false)
    }

    def setUpMaxByAngleNormalized(): List[Float] = {
      val vList = Float.PositiveInfinity +: (1 to 180).map { nDegree =>
        (-14.8311 / Math.pow(nDegree.toDouble, 2) + 28.4664 / nDegree.toDouble - 0.1496).toFloat
      }.map { _ * 1000.0f }

      logger.debug(s"vListByAngle: \r\n $vList")
      vList.toList
    }


    // v_0: Winkel, in richtige Richtung
    //v in angle direction
    override def newVf(angle: Double, startV: Double): Double = {
      // logger.info(s"newVf: angle: $angle")
      val angleModPi = Math.abs(MathUtils.normalizeAngle(angle, 0.0))

      //  logger.info(s"newVf: angle mod pi: $angleModPi")
      val (vx, vy) = (Math.abs(Math.cos(angleModPi)) * startV, Math.sin(angleModPi) * startV)

      // logger.info(s"newVf: vx: $vx, vy: $vy")
      val maxPossibleVx = vx + machineModelAccX(vx) * deltaT * 60
      val maxPossibleVy = vy + machineModelAccY(vy) * deltaT * 60

      //  logger.info(s"maxPossibleVx: $maxPossibleVx, maxPossibleVy: $maxPossibleVy")
      val tanAngle = Math.tan(angleModPi)
      val maxRelativeVx = if (tanAngle == 0.0) Double.PositiveInfinity else maxPossibleVy / tanAngle
      val maxRelativeVy = tanAngle * maxPossibleVx

      val achievableVx = List(maxPossibleVx, maxRelativeVx).map(Math.abs).min
      val achievableVy = List(maxPossibleVy, maxRelativeVy).map(Math.abs).min
      // logger.info(s"max pos: achievableVx: $achievableVx, achievableVy: $achievableVy")

      val vNew = math.sqrt(math.pow(achievableVx, 2) + math.pow(achievableVy, 2))

      //      logger.info(s"vOld: $startV, vNew: $vNew, vNew in m/min: $vNewMetersPerMinute")
      //      logger.info(s"angle: $angle, angleModPi: $angleModPi")
      //      logger.info(s"old vx: $vx, old vy: $vy")
      //      logger.info(s"maxPossibleVx: $maxPossibleVx, maxPossibleVy: $maxPossibleVy")
      //      logger.info(s"maxRelativeVx: $maxRelativeVx, maxRelativeVy: $maxRelativeVy")
      //      logger.info(s"achievable vx: $achievableVx, achievable vy: $achievableVy")
      //      logger.info(s"vNew: $vNew")

      vNew
    }

    override val deltaT: Double = 0.15d // Paper example 0 to 5 m/min in 0.2 secs taken from Example
  }
}

// e.g. for fixed compositions and motion primitives (leafs)
//Pathfct can be empty, ie. for generic decomposition combinator
case class PathCoverageStep(pcFct: Option[cncPathFct],
                            tool: Option[CncTool],
                            pcrList: List[PathCoverageStep],
                            description_param: String = "no description",
                            resultTransform : Option[List[List[List[Float]]] => List[List[List[Float]]]] = None) {self =>
  def withPathTransformFunction(t: List[List[List[Float]]] => List[List[List[Float]]]): PathCoverageStep =
    PathCoverageStep(self.pcFct, self.tool, self.pcrList, self.description_param, Some(t))

//  def getResultPathAndModel(cncModel: Cnc2DModel, config: PathCoverageStepConfig):
//  (List[List[List[Float]]], List[CncTool], Cnc2DModel) = {
//    pcFct match {
//      case Some(a) => a(cncModel, config)
//      case None => (List.empty[List[List[Float]]], cncModel)
//    }
//  }
//
//  lazy val getDescription = description_param + tool.map(i => s" using tool ${i.description}").getOrElse("")
//
//  def getSubResults(cncModel: Cnc2DModel, config: PathCoverageStepConfig):
//  List[(List[List[List[Float]]], List[CncTool], Cnc2DModel)] =
//    pcrList.map(i => i.getCompleteResultPathAndModel(cncModel, config))
//
//  def getCompleteResultPathAndModel(cncModel: Cnc2DModel, config: PathCoverageStepConfig):
//  (List[List[List[Float]]], List[CncTool], Cnc2DModel) = {
//    val currentResult = getResultPathAndModel(cncModel, config)
//    if (pcrList.isEmpty) {
//      currentResult
//    } else {
//      getSubResults(currentResult._2, tool, config).foldLeft(currentResult)((t, m) => (t._1 ++ m._1, m._2))
//    }
//  }
}

/**
 *
 * @param boundaries     x,y boundaries, workpiece area
 * @param targetGeometry area that must be machined. Difference from boundaries = leftover Material
 * @param rest           geometry list that is left to be processed
 * @param machined       geometry list that was already machined (can be used as entry points for further tool paths)
 */
case class Cnc2DModel(boundaries: List[Float],
                      targetGeometry: Geometry,
                      rest: List[Geometry],
                      machined: List[Geometry],
                      machinedMultiPolygon: Geometry,
                      initialMachined: Geometry,
                      transformStack: List[(Cnc2DModel, Geometry)] = List.empty[(Cnc2DModel, Geometry)])
  extends LazyLogging with JtsUtils {
  self =>
  def empty = Scene(List.empty, List.empty)

  def rotateModel(angle: Double): Cnc2DModel = {
    val transformMatrix = AffineTransformation.rotationInstance(angle)
    val boundaryLineString = getLineStringFromBoundaries(boundaries)
    val transFormedBoundaryLineString = transformMatrix.transform(boundaryLineString)
    val newBoundaryList = getBoundaryListFromGeo(transFormedBoundaryLineString)

    Cnc2DModel(boundaries = newBoundaryList,
      targetGeometry = transformMatrix.transform(targetGeometry), rest = rest.map(transformMatrix.transform),
      machined = machined.map(transformMatrix.transform),
      machinedMultiPolygon = transformMatrix.transform(machinedMultiPolygon),
      initialMachined = transformMatrix.transform(initialMachined),
      transformStack = self.transformStack)
  }

  /**
   * Boundary rectangle - targetGeometry, area that must not be machined
   */
  lazy val targetWorkpiece: Geometry = {
    lazy val boundRect = getBoundaryRectangle(boundaries)
    pGeo("boundRect", boundRect)
    pGeo("targetGeometry", targetGeometry)
    lazy val diff = boundRect.difference(targetGeometry).difference(initialMachined)
    pGeo("diff", diff)
    diff
  }
  lazy val getMachinedMultiGeo: Geometry = {
    machinedMultiPolygon
  }

  lazy val machinedPolygonHistory = {
    //logger.info(s"attempting to unionize.")
    //logger.info(s"attempting to unionize: Machined Geos: ${machined.length}")
    // machined.foreach(g => logger.info(s"Machined geo element: \r\n$g"))
    // machined.map(m => m.asInstanceOf[Polygon].getExteriorRing)
    //    machined.reduceOption[Geometry] { case (a, b) =>
    //      pGeo("a", a)
    //      pGeo("b", b)
    //      a.union(b)
    //    }.getOrElse(emptyGeometry)
    //    val cpu = new CascadedPolygonUnion(machined.asJava)
    //    val unary = new UnaryUnionOp(machined.asJava)

    //    machined.reduceOption[Geometry]{case (a,b) =>
    //      logger.info(s"union a,b: \r\n$a \r\n$b")
    //      SnapIfNeededOverlayOp.union(a,b)}.getOrElse(emptyGeometry)

    val machinedTyped = machined.filter(isPolygon).map(_.asInstanceOf[Polygon]).toArray
    machined.map(_.getGeometryType).filterNot(_.equals("Polygon")).foreach(i => logger.info(s"notPolygon: $i"))
    gf.createMultiPolygon(machinedTyped)
    //unary.union()
  }

  lazy val getRestMultiGeo: Geometry = {
    //val cpu = new CascadedPolygonUnion(rest.asJava)
    //cpu.union()
    lazy val multiPoly = gf.createMultiPolygon(rest.filter(isPolygon).map(_.asInstanceOf[Polygon]).toArray)
    multiPoly
  }
  //rest.reduce(_.union(_))

  def withTargetGeo(g:Geometry): Cnc2DModel = {
    Cnc2DModel(self.boundaries, g, self.rest,
      self.machined, self.machinedMultiPolygon, self.initialMachined, self.transformStack)
  }

  def blockGeometry(g:Geometry): Cnc2DModel = {
    val newTargetGeometry = self.targetGeometry.difference(g)
    val newRest = self.rest.map(_.difference(g))
    Cnc2DModel(self.boundaries, newTargetGeometry, newRest,
      self.machined, self.machinedMultiPolygon, self.initialMachined, List((self, g)) ++ self.transformStack)
  }

  def releaseGeometry(): Cnc2DModel = {
    val (initialModel: Cnc2DModel, g: Geometry) = self.transformStack.head
    val updatedModel = initialModel.withMachinedGeo(self.machinedMultiPolygon)

    Cnc2DModel(updatedModel.boundaries, updatedModel.targetGeometry, updatedModel.rest,
      updatedModel.machined, updatedModel.machinedMultiPolygon, updatedModel.initialMachined, updatedModel.transformStack) // pop
  }

  def withInitialMachinedGeo(g: Geometry): Cnc2DModel = {
    val nModel = self.withMachinedGeo(g)
    Cnc2DModel(nModel.boundaries, nModel.targetGeometry, nModel.rest, nModel.machined, nModel.machinedMultiPolygon, g, nModel.transformStack)
  }

  def withMachinedGeo(g0: Geometry): Cnc2DModel = {
    val t = Try[Cnc2DModel]({
      pGeo("with machinedGeo start:", getMachinedMultiGeo)

      pGeo("g0", g0)
      logger.debug(s"g0.getFactory ${g0.getFactory}")

      val g = DouglasPeuckerSimplifier.simplify(g0, 0.01)
      pGeo("g", g)

      val restGeos1 = self.rest.map(i => {
        robustDifference(i, g)
      })

      val restGeos2 = restGeos1.map(g => multiGeoToGeoList(g)).reduceOption(_ ++ _).filter(_.nonEmpty)
      logger.debug("with machinedGeo: Got restgeos, attempting to sort")

      val restGeos = restGeos2.getOrElse(List.empty[Geometry]).sortBy(_.getArea)(Ordering[Double].reverse)
      logger.debug("with machinedGeo after restGeos")
      logger.debug("attempting to unionize")
      pGeo("machinedMultiPolygon", machinedMultiPolygon)
      pGeo("g", g)
      val newMachGeo = robustUnion(machinedMultiPolygon,g)
      logger.debug("gotMultiPoly")
      pGeo("newMachGeo", newMachGeo)
      Cnc2DModel(self.boundaries, self.targetGeometry, restGeos, self.machined :+ g, newMachGeo, self.initialMachined, self.transformStack)
    }
    )
    t match {
      case Failure(e) => logger.error(e.getMessage)
      case _ => ()
    }

    t.getOrElse(self)
  }
}

object Cnc2DModel extends JtsUtils with LazyLogging {
  def apply(s: String,bounds: List[Float]): Cnc2DModel = {
    val t0 = System.currentTimeMillis()
    val wktReader = new WKTReader()
    val wktStr: String = Source.fromResource(s).getLines.mkString("\r\n")
    val tgtGeo = wktReader.read(wktStr)
    val t1 = System.currentTimeMillis()
    logger.debug("Reading wktString elapsed time: " + (t1 - t0) + "ms")

    Cnc2DModel(boundaries = bounds,
    targetGeometry = tgtGeo, rest = List(tgtGeo), machined = List(),
    machinedMultiPolygon = emptyGeometry, initialMachined = emptyGeometry)
  }

}

case class SceneSRT(boundaries: List[Float], obstacles: List[MqttObstacleSRT]) {
  def empty = Scene(List.empty, List.empty)
}

case class MeshScene(boundaries: List[List[Float]], obstacles: List[PpSurfaceMesh]) {
  self =>
}

case class ProblemDefinitionFiles(cfgFileName: String,
                                  envModelLocation: String,
                                  robotModelLocation: String,
                                  problemProperties: Properties)

object ProblemDefinitionFiles extends LazyLogging {
  def apply(cfgFile: String): Option[ProblemDefinitionFiles] = {
    val probFolder = PropertyFiles.problemsProperties.getProperty("org.combinators.ctp.problemFolder")

    val cfgProperties = new Properties()
    try {
      logger.debug(s"Loading cfg file: ${probFolder + cfgFile}")
      cfgProperties.load(new FileInputStream(probFolder + cfgFile))

      val eFile = probFolder + cfgProperties.getProperty("world").split("dae").head + "obj"
      val robotFile = probFolder + cfgProperties.getProperty("robot").split("dae").head + "obj"
      Some(ProblemDefinitionFiles(cfgFile, eFile, robotFile, cfgProperties))
    }
    catch {
      case e: Exception =>
        e.printStackTrace()
        None
    }
  }
}


case class PolygonScene(vertices: List[List[Float]], obstacles: List[List[Int]], boundaries: List[Float]) {
  self =>
  def withVertices(v: List[List[Float]]): PolygonScene = {
    PolygonScene(v, self.obstacles, self.boundaries)
  }

  def withFreeCells(l: List[List[Int]]): PolySceneCellSegmentation = {
    PolySceneCellSegmentation(self.vertices, self.obstacles, self.boundaries, l)
  }
}

case class PolySceneLineSegmentation(vertices: List[List[Float]],
                                     obstacles: List[List[Int]],
                                     boundaries: List[Float],
                                     topVertices: List[Int],
                                     bottomVertices: List[Int],
                                     lines: List[List[Int]]) {}

case class PolySceneCellSegmentation(vertices: List[List[Float]],
                                     obstacles: List[List[Int]],
                                     boundaries: List[Float],
                                     freeCells: List[List[Int]]) {
  self =>
  def withRoadmap(g: Graph[List[Float], WUnDiEdge]): PolySceneSegmentationRoadmap =
    PolySceneSegmentationRoadmap(self.vertices, self.obstacles, self.boundaries, self.freeCells, List.empty, g)
}

object PolySceneCellSegmentation {
  def apply(): PolySceneCellSegmentation = PolySceneCellSegmentation(
    List.empty[List[Float]], List.empty[List[Int]], List.empty[Float], List.empty[List[Int]])
}

case class PolySceneSegmentationRoadmap(vertices: List[List[Float]],
                                        obstacles: List[List[Int]],
                                        boundaries: List[Float],
                                        freeCells: List[List[Int]],
                                        centroids: List[List[Float]],
                                        roadmap: Graph[List[Float], WUnDiEdge]) {
  self =>
  def withRoadmap(g: Graph[List[Float], WUnDiEdge]): PolySceneSegmentationRoadmap =
    PolySceneSegmentationRoadmap(self.vertices, self.obstacles, self.boundaries, self.freeCells, self.centroids, g)

  def withPath(p: Seq[List[Float]]): PolySceneSegmentationRoadmapPath =
    PolySceneSegmentationRoadmapPath(self.vertices, self.obstacles, self.boundaries, self.freeCells, self.roadmap, p)
}

case class PolySceneSegmentationRoadmapPath(
                                             vertices: List[List[Float]],
                                             obstacles: List[List[Int]],
                                             boundaries: List[Float],
                                             freeCells: List[List[Int]],
                                             roadmap: Graph[List[Float], WUnDiEdge],
                                             gpath: Seq[List[Float]]) {
  self =>

  def withPath(p: Seq[List[Float]]): PolySceneSegmentationRoadmapPath =
    PolySceneSegmentationRoadmapPath(self.vertices, self.obstacles, self.boundaries, self.freeCells, self.roadmap, p)

  def withRoadmap(g: Graph[List[Float], WUnDiEdge]): PolySceneSegmentationRoadmapPath =
    PolySceneSegmentationRoadmapPath(self.vertices, self.obstacles, self.boundaries, self.freeCells, g, self.gpath)

  def empty: PolySceneSegmentationRoadmapPath = PolySceneSegmentationRoadmapPath(List.empty, List.empty, List.empty,
    List.empty, Graph.empty, Seq.empty[List[Float]])
}

case class PolyLineSegmentation(vertices: List[List[Float]], lines: List[List[Int]]) {}

case class CellSegmentation(vertices: List[List[Float]], cells: List[List[Int]])

case class SegmentationLines2d(lines: List[List[List[Float]]])

case class SceneCfreePolygons2d(vertices: List[List[Float]], cfreePolygons: List[PpPolyhedronMesh])

case class SceneMesh(vertices: vertexArrayType2, faces: facesArrayType, voxels: voxelArrayType) {}

case class MqttCubeData(tMatrix: List[List[Float]], cubeSize: List[Float])

case class MqttTransform(transformMatrixList: List[List[Float]])

case class MqttObstacleSRT(primitive: Int, srt: MqttTransformSRT)

case class MqttTransformSRT(localScale: List[Float], localRot: List[Float], localTranslate: List[Float])

case class PathPreds(nodes: List[Int], preds: List[Int])

case class RmInput(centroids: List[List[Float]], adjacency: List[List[Int]])

case class ExploredStates(exploredStates: List[List[Float]])

trait Transformable {
  val tMatrix: List[List[Float]]
}

case class MpTaskStartGoal(startPosition: List[Float], endPosition: List[Float]) {}

case class MpTaskStartGoalPose(startPoint: List[Float], startPose: List[Float], endPoint: List[Float], endPose: List[Float]) {}


