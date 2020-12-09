package org.combinators.ctp.repositories.toplevel

import java.io.FileInputStream
import java.util
import java.util.Properties

import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.math3.util.{FastMath, MathUtils}
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.geometry.{PpPolyhedronMesh, PpSurfaceMesh}
import org.combinators.ctp.repositories.pathcoverage.{CncTool, JtsUtils}
import org.locationtech.jts.algorithm.Angle
import org.locationtech.jts.dissolve.LineDissolver
import org.locationtech.jts.geom.{Coordinate, Geometry, Polygon}
import org.locationtech.jts.operation.overlay.snap.SnapIfNeededOverlayOp
import org.locationtech.jts.operation.union.{CascadedPolygonUnion, UnaryUnionOp}
import org.locationtech.jts.simplify.DouglasPeuckerSimplifier
import scalax.collection.Graph
import scalax.collection.edge.WUnDiEdge

import scala.annotation.tailrec
import scala.jdk.CollectionConverters._
import scala.language.implicitConversions


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

trait PathCoverageStepConfig extends JtsUtils with MachineAccelerationModel{
  val minPointClearanceOnPath: Double
  val min_ae: Double

  def bufferFct(g: Geometry, d: Double): Geometry = paraBuffer(minPointClearanceOnPath)(g, d)
}

object PathCoverageStepConfig {
  def apply(): PathCoverageStepConfig = new PathCoverageStepConfig{
    override val minPointClearanceOnPath: Double = 0.01
    override val min_ae: Double = 0.01

    override def machineModelAccX(velocity: Double): Double = {
      velocity match {
        case v if (0.0 <= v && v < 1.0) => -0.0624 * Math.pow(v, 2) + 0.2603 * v + 0.0216
        case v if (1.0 <= v && v <= 10.0) => -0.0027 * Math.pow(v, 2) + 0.0880 * v + 0.1536
        case v if (v > 10.0) => machineModelAccX(10.0)
        case _ => machineModelAccX(0.0)
      }
    }

    override def machineModelAccY(velocity: Double): Double = {
      velocity match {
        case v if (0.0 <= v && v < 1.0) => -0.1979 * Math.pow(v, 2) + 0.3970 * v + 0.0184
        case v if (1.0 <= v && v <= 10.0) => -0.0036 * Math.pow(v, 2) + 0.0967 * v + 0.1427
        case v if (v > 10.0) => machineModelAccY(10.0)
        case _ => machineModelAccY(0.0)
      }
    }

    //mm/min

    override def vfMaxByAngle(angle: Double): Double = {
      val normalizedAngle  = Math.PI - Math.abs(MathUtils.normalizeAngle(angle, 0.0))
      val normalizedDegree = FastMath.toDegrees(normalizedAngle)
      if(normalizedDegree < 1)
        Double.PositiveInfinity
      else {
        val vf = (-14.8311 / (Math.pow(normalizedDegree, 2)) + 28.4664 / normalizedDegree - 0.1496)
        if(vf < 0)
          logger.info(s"vf lt zero: angle: $angle, normalizedDegree: $normalizedDegree, vf: $vf")
        vf
      }
    }

    // v_0: Winkel, in richtige Richtung
    //v in angle direction
    override def newVf(angle: Double, startV: Double): (Double) = {
      logger.info(s"newVf: angle: $angle")
      val angleModPi = Math.abs(MathUtils.normalizeAngle(angle, 0.0))

      logger.info(s"newVf: angle mod pi: $angleModPi")
      val (vx, vy) = (Math.abs(Math.cos(angleModPi)) * startV, Math.sin(angleModPi) * startV)

      logger.info(s"newVf: vx: $vx, vy: $vy")
      val maxPossibleVx = machineModelAccX(vx) * deltaT * 60
      val maxPossibleVy = machineModelAccY(vy) * deltaT * 60

      logger.info(s"maxPossibleVx: $maxPossibleVx, maxPossibleVy: $maxPossibleVy")
      val tanAngle = Math.tan(angleModPi)
      val maxRelativeVx = if (tanAngle == 0.0) Double.PositiveInfinity else maxPossibleVy / tanAngle
      val maxRelativeVy = tanAngle * maxPossibleVx

      val achievableVx = List(maxPossibleVx, maxRelativeVx).map(Math.abs).min
      val achievableVy = List(maxPossibleVy, maxRelativeVy).map(Math.abs).min
      logger.info(s"max pos: achievableVx: $achievableVx, achievableVy: $achievableVy")

      val vNew = startV + math.sqrt(math.pow(achievableVx, 2) + math.pow(achievableVy, 2))
      val vNewMetersPerMinute = vNew

      //      logger.info(s"vOld: $startV, vNew: $vNew, vNew in m/min: $vNewMetersPerMinute")
      //      logger.info(s"angle: $angle, angleModPi: $angleModPi")
      //      logger.info(s"old vx: $vx, old vy: $vy")
      //      logger.info(s"maxPossibleVx: $maxPossibleVx, maxPossibleVy: $maxPossibleVy")
      //      logger.info(s"maxRelativeVx: $maxRelativeVx, maxRelativeVy: $maxRelativeVy")
      //      logger.info(s"achievable vx: $achievableVx, achievable vy: $achievableVy")
      //      logger.info(s"vNew: $vNew")

      vNew
    }

    override val deltaT: Double = 0.05d // Paper example 0 to 5 m/min in 0.2 secs taken from Example
  }
}

/**
 * @param s      scene applied to pc steps
 * @param config Config object that will be applied to all path coverage steps
 * @param l      unfolded pc step list
 */
case class PathCoverageResult(s: Cnc2DModel, config: PathCoverageStepConfig, l: List[PathCoverageStep]) extends JtsUtils {
  def unfoldPathCoverageStepsDepthFirst(l: List[PathCoverageStep]): List[(cncPathFct, CncTool)] =
    l match {
      case Nil => Nil
      case (a: PathCoverageStep) :: tail =>
        val currentCncStep: Option[cncPathFct] = a.pcFct
        val currentCncTool: Option[CncTool] = a.tool
        val currentCncStepsSubtree: List[(cncPathFct, CncTool)] =
          if (a.pcrList.nonEmpty)
            unfoldPathCoverageStepsDepthFirst(a.pcrList)
          else
            List.empty[(cncPathFct, CncTool)]
        val restCncSteps: List[(cncPathFct, CncTool)] = unfoldPathCoverageStepsDepthFirst(tail) // other part of decomposition
        val list1: List[(cncPathFct, CncTool)] =
          (currentCncStep, currentCncTool) match {
            case (Some(a), Some(tool)) => (a, tool) +: currentCncStepsSubtree
            case _ => currentCncStepsSubtree
          }
        list1 ++ restCncSteps
    }


  lazy val unfoldedPcFunctionList: List[(cncPathFct, CncTool)] = unfoldPathCoverageStepsDepthFirst(l)

  /**
   * quatruple: EndModel, Aux List Path Functions (empty when done),
   * model history (first element is initial model),
   * path history (first element is empty path)
   */
  lazy val computeModelHistory: (List[Cnc2DModel], List[List[List[Float]]]) ={
    lazy val (modelList, pathList) = unfoldedPcFunctionList.map(_._1).foldLeft((List(s), List.empty[List[List[Float]]])) {
      case (((modelList: List[Cnc2DModel],
      pathList: List[List[List[Float]]]), currentFct)) => {
        val (pathResult, modelResult) = currentFct(modelList.last, config)
        (modelList :+ modelResult, pathList ++ pathResult)
      }
    }
    (modelList, pathList)
  }

  lazy val (modelList, pathList) = computeModelHistory
  lazy val endScene = modelList.last
  lazy val toolList = unfoldedPcFunctionList.map(_._2)

  def vfMaxSegment(c1: Coordinate, c2: Coordinate, c3: Coordinate): Float = {
    val ang = Angle.angleBetween(c1, c2, c3)
    val asd = config.vfMaxByAngle(ang)
    logger.info(s"angleVelocity: $asd ${asd.toFloat}")
    asd.toFloat
  }


  lazy val withMaxVfByAngle: List[List[List[Float]]] = {
    (pathList).map {
      case (singlePath: List[List[Float]]) =>
        if (singlePath.length > 2) {
          val singlePathCoords = singlePath.map(asCoordinate)
          val constrPath = singlePathCoords.zip(singlePathCoords.tail).zip(singlePathCoords.tail.tail).map { case ((a, b), c) => (a, b, c) }
          val headEntry: List[Float] = List(
            singlePathCoords.head.x,
            singlePathCoords.head.y,
            singlePathCoords.head.getZ,
            Double.PositiveInfinity).map(_.toFloat)
          val listResults: List[List[Float]] = constrPath.map {
            case (c1, c2, c3) => List(c2.x.toFloat, c2.y.toFloat, c2.getZ.toFloat, vfMaxSegment(c1, c2, c3).toFloat)
          }
          val lastEntry: List[Float] = List(
            singlePathCoords.last.x,
            singlePathCoords.last.y,
            singlePathCoords.last.getZ,
            Double.PositiveInfinity).map(_.toFloat)
          headEntry +: listResults :+ lastEntry
        }
        else
          singlePath.map(coord => coord :+ 0.0f)
    }
  }


  lazy val maxVfByToolAndAngle: List[List[List[Float]]] = (withMaxVfByAngle.zip(toolList)).map {
    case (singlePath: List[List[Float]], tool: CncTool) => singlePath.map(l => l.dropRight(1) :+ List(l.last, tool.vf).min)
  } //Tool.vf for every coordinate

  lazy val withMaxVfByAcc: List[List[List[Float]]] = {
    def buildList(l: List[List[Float]], lastCoord: List[Float], nextCoord: List[Float]): List[List[Float]] = {
      @tailrec
      def calcVend(angle: Double, targetDistance: Double, accumulatedDistance: Double,
                   currentVelocity: Double): Double = {
        if (accumulatedDistance < targetDistance) {
          println(s"currentVelocity: $currentVelocity, accumulatedDistance: $accumulatedDistance")
          val newVelocity = config.newVf(angle, currentVelocity)
          val newDistance = accumulatedDistance + newVelocity * 1000 / 60 * config.deltaT
          println(s"currentVelocity: $currentVelocity, accumulatedDistance: $accumulatedDistance, newVelocity: $newVelocity,newDistance:$newDistance")
          calcVend(angle, targetDistance, newDistance, newVelocity)
        }
        else {
          logger.info(s"targetVelocity: $currentVelocity")
          currentVelocity
        }
      }

      val startV = lastCoord.last
      val angle = Angle.angle(asCoordinate(lastCoord), asCoordinate(nextCoord))
      val distanceBetweenPoits = asCoordinate(lastCoord).distance(asCoordinate(nextCoord))
      logger.info(s"startPoint: $lastCoord, nextCooord: $nextCoord, distance: $distanceBetweenPoits")

      logger.info(s"startV $startV")
      logger.info(s"comparing velocities for $nextCoord vs ${calcVend(angle, distanceBetweenPoits, 0.0d, startV).toFloat}")


      lazy val nextCoordWithVelocity: List[Float] =
        (nextCoord.dropRight(1) :+
          List(nextCoord.last, calcVend(angle, distanceBetweenPoits, 0.0d, startV).toFloat).min)
      l :+ nextCoordWithVelocity
    }

    maxVfByToolAndAngle.map {
      case (singlePath: List[List[Float]]) =>
        if (singlePath.size > 2) {
          singlePath.tail.foldLeft(List(singlePath.head.take(3) :+ 0.0f)) { case (a, b) => buildList(a, a.last, b) }
        } else List.empty[List[Float]]
    }
  }

  def getString(l: List[List[Float]], withFMAX: Boolean = false) =
    l.filter(_.nonEmpty).map(singleCoord => {
      val zCoordString = if (!singleCoord(2).isNaN) s" Z${singleCoord(2)}" else ""
      val fString = s"${if (withFMAX) "MAX" else singleCoord.last * 1000.0}"
      s"""L X${singleCoord.head} Y${singleCoord(1)}$zCoordString F$fString""".stripMargin
    })

  def printAll() = withMaxVfByAcc.zip(toolList).map {
    case (currentPath, currentTool: CncTool) =>
      if(currentPath.nonEmpty){
      val linearDive = if ((currentPath.head) (2).isNaN || (currentPath.head) (2) == 0.0)
        s"${getString(List(currentPath.head.dropRight(2) :+ 0.0f :+ 0.050f)).mkString("\r\n")}"
      else ""
      val outStr =
        s"""BEGIN PGM Single MM
           |TOOL CALL ${currentTool.idString} ; ${currentTool.description}
           |${getString(List(currentPath.head.dropRight(2) :+ 20.0f :+ currentTool.vf), true).mkString("\r\n")}
           |$linearDive
           |${getString(currentPath.tail).mkString("\r\n")}
           |${getString(List(currentPath.last.dropRight(2) :+ 20.0f), true).mkString("\r\n")}
           |END PGM Single MM
           |""".stripMargin
      logger.info(s"Teilpfad: \r\n$outStr")
      } else{
        logger.info(s"Empty path, no output")

      }
  }
}

// e.g. for fixed compositions and motion primitives (leafs)
//Pathfct can be empty, ie. for generic decomposition combinator
case class PathCoverageStep(pcFct: Option[cncPathFct],
                            tool: Option[CncTool],
                            pcrList: List[PathCoverageStep],
                            description_param: String = "no description"
                           ) {
  def getResultPathAndModel(cncModel: Cnc2DModel, config: PathCoverageStepConfig):
  (List[List[List[Float]]], Cnc2DModel) = {
    pcFct match {
      case Some(a) => a(cncModel, config)
      case None => (List.empty[List[List[Float]]], cncModel)
    }
  }

  lazy val getDescription = description_param + tool.map(i => s" using tool ${i.description}").getOrElse("")

  def getSubResults(cncModel: Cnc2DModel, config: PathCoverageStepConfig):
  List[(List[List[List[Float]]], Cnc2DModel)] =
    pcrList.map(i => i.getCompleteResultPathAndModel(cncModel, config))

  def getCompleteResultPathAndModel(cncModel: Cnc2DModel, config: PathCoverageStepConfig):
  (List[List[List[Float]]], Cnc2DModel) = {
    val currentResult = getResultPathAndModel(cncModel, config)
    if (pcrList.isEmpty) {
      currentResult
    } else {
      getSubResults(currentResult._2, config).foldLeft(currentResult)((t, m) => (t._1 ++ m._1, m._2))
    }
  }
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
                      machined: List[Geometry]) extends LazyLogging with JtsUtils {
  self =>
  def empty = Scene(List.empty, List.empty)

  /**
   * Boundary rectangle - targetGeometry, area that must not be machined
   */
  lazy val targetWorkpiece: Geometry = {
    lazy val boundRect = getBoundaryRectangle(boundaries)
    pGeo("boundRect", boundRect)
    pGeo("targetGeometry", targetGeometry)
    lazy val diff = boundRect.difference(targetGeometry)
    pGeo("diff", diff)
    diff
  }
  lazy val getMachinedMultiGeo: Geometry = {
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

    machined.map(_.getGeometryType).filterNot(_.equals("Polygon")).foreach (i => logger.info(s"notPolygon: $i"))
    gf.createMultiPolygon(machinedTyped)
    //unary.union()
  }

  lazy val getRestMultiGeo: Geometry = {
    //val cpu = new CascadedPolygonUnion(rest.asJava)
    //cpu.union()
    lazy val multiPoly = gf.createMultiPolygon(rest.map(_.asInstanceOf[Polygon]).toArray)
    multiPoly
  }
  //rest.reduce(_.union(_))

  def withMachinedGeo(g0: Geometry): Cnc2DModel = {
    logger.info(s"with machinedGeo start: \r\n${getMachinedMultiGeo}")

    pGeo("g0", g0)
    logger.info(s"g0.getFactory ${g0.getFactory}")
//    val g1 = if (!g0.isEmpty){
//      val dissolved = LineDissolver.dissolve(g0).getCoordinates
//      gf.createPolygon(gf.createLinearRing(dissolved :+ dissolved.head))
//    }else
//      g0
//    pGeo("g1", g1)

    val g = DouglasPeuckerSimplifier.simplify(g0, 0.0001)
    pGeo("g", g)

    val restGeos1 = self.rest.map(i => {
     // pGeo("i", i)
      i.buffer(-0.0001).difference(g.buffer(0.0001)).buffer(0.0001)
    })
    //Simplification

    val restGeos2 = restGeos1.map(g => multiGeoToGeoList(g)).reduceOption(_ ++ _).filter(_.nonEmpty)
    logger.info("with machinedGeo: Got restgeos, attempting to sort")

    val restGeos = restGeos2.getOrElse(List.empty[Geometry]).sortBy(_.getArea)(Ordering[Double].reverse)
    logger.info("with machinedGeo after restGeos")
    Cnc2DModel(self.boundaries, self.targetGeometry, restGeos, self.machined :+ g)
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


