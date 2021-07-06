package org.combinators.ctp.repositories.pathcoverage

import com.typesafe.scalalogging.LazyLogging
import org.combinators.ctp.repositories.toplevel.{Cnc2DModel, PathCoverageStep, PathCoverageStepConfig}
import org.combinators.ctp.repositories._
import org.combinators.cls.interpreter.combinator
import org.combinators.cls.types.{Constructor, Kinding, Type, Variable}
import org.combinators.cls.types.syntax._
import org.locationtech.jts.algorithm.construct.MaximumInscribedCircle
import org.locationtech.jts.geom.impl.CoordinateArraySequence
import org.locationtech.jts.geom.{Coordinate, Envelope, Geometry, GeometryFactory, LineString, LinearRing, MultiLineString, MultiPolygon, Polygon}
import org.locationtech.jts.io.WKTReader
import org.locationtech.jts.math.Vector2D
import org.locationtech.jts.util.GeometricShapeFactory

import scala.io.Source

case class CncTool(d: Float, ae: Float, ap: Float, vf: Float, n: Int, description: String, idString: String)

trait CamMpTopRepository extends LazyLogging with JtsUtils {

  val pathCoverageFctRoot = Constructor("pathCoverageFctRoot")
  val pFct = Constructor("pFct")

  val coveragePrimitive = Constructor("coveragePrimitive")
  val roughing = Constructor("roughing")
  val finishing = Constructor("finishing")
  val steel:Type = Constructor("steel")
  val alu:Type = Constructor("alu")
  val pFctResult:Type = Constructor("pFctResult")
  val pFctResultRoot:Type = Constructor("pFctResultRoot")

  val root:Type = Constructor("root")
  val alpha = Variable("alpha")
  lazy val kindingComplete: Kinding = buildKinding(Map(alpha -> Seq(steel, alu)))
  lazy val aluKinding: Kinding = buildKinding(Map(alpha -> Seq(alu)))
  lazy val steelKinding: Kinding = buildKinding(Map(alpha -> Seq(steel)))

  @combinator object SpiralRoughing {
    def apply(t: CncTool,
              restMp: PathCoverageStep): PathCoverageStep = {
      val pcFct: cncPathFct = { (initialScene, pcConfig) =>
        logger.info(s"spiral roughing: ${initialScene.rest.head}")
        val largestEmptyCircle = new MaximumInscribedCircle(initialScene.rest.head, 1.0)
        val c1 = largestEmptyCircle.getCenter.getCoordinate
        val c11 = new Coordinate(c1.x, c1.y, 20.0f)
        val c2 = largestEmptyCircle.getCenter.getCoordinate
        val c22 = new Coordinate(c2.x, c2.y, 0.0f)

        logger.info(s"c11: $c11, c22: $c22")
        val diveIn = new DiveIn {
          override val tool: CncTool = t
          override val config: PathCoverageStepConfig = pcConfig
          override val lineString: LineString = getNewLineString(c11, c22)
        }

        val newCncModel = initialScene.withMachinedGeo(diveIn.getGeometry)
        pGeo("diveIn.getGeometry", diveIn.getGeometry)

        val spiral = new Spiral {
          override val minRadius: Double = t.d
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
        val returnModel = newCncModel.withMachinedGeo(spiralGeo)
        pGeo("spiralGeo", spiralGeo)
        (List(diveIn.getPath, spiral.discretePath), returnModel)
      }

      /**
       * Wähle zu bearbeitende Restgeometrie
       * Erstelle größten leeren Kreis c
       * Eintauchschritt in c.radius
       * Erstellung maximal möglicher Spiralpfad um c.radius
       * Update des geometrischen Modells
       */
      PathCoverageStep(Some(pcFct), Some(t), List(restMp))
    }

    val semanticType = alpha =>: pFct :&: alpha =>: pathCoverageFctRoot :&: alpha
  }

  // Moat first, rest later
  @combinator object MoatEntry {
    def apply(t: CncTool,
              restMp: PathCoverageStep):
    PathCoverageStep = {
      val combinatorPcFct: cncPathFct = { (initialScene, pcConfig) =>
        val moatPrimitive = new Moat {
          val tool: CncTool = t
          val restCoords = initialScene.rest.head.getCoordinates
          val pArray = if (restCoords.nonEmpty) restCoords :+ restCoords.head else Array.empty[Coordinate]
          val p1: Polygon = new Polygon(new LinearRing(new CoordinateArraySequence(pArray), gf), Array.empty[LinearRing], gf)
          override val config: PathCoverageStepConfig = pcConfig
        }

        logger.info(s"tool: d: ${t.d}")
        logger.info(s"computung diffgeo Moat")
        logger.info(s"initialScene.rest.head ${initialScene.rest.head}")
        val selectLines = moatPrimitive.selectLines
        if (selectLines.nonEmpty) {
          logger.info(s"asd.selectLines: $selectLines")
          val ml = new MultiLineString(selectLines, new GeometryFactory())
          logger.info(s"asd.selectLines: $ml")
          // logger.info(s"asd.selectLines.buffer: ${ml.buffer(t.d)}")
        }
        else
          logger.info(s"asd.selectLines: empty")
        logger.info(s"asd.returnPath Length: ${moatPrimitive.returnPath.length}")

        val diffGeo = moatPrimitive.getPolygon
        logger.info(s"diffgeo: $diffGeo")
        val newScene = initialScene.withMachinedGeo(diffGeo)
        logger.info(s"After rPath")
        (List(moatPrimitive.returnPath), newScene)
      }
      PathCoverageStep(Some(combinatorPcFct), Some(t), List(restMp), "moat composition")
    }

    val semanticType = steel =>: steel  =>: pathCoverageFctRoot :&: steel
  }

//  @combinator object IsleHull{
//    def apply(pcFct1: PathCoverageStep,
//              pcFct2: PathCoverageStep):
//    PathCoverageStep = {
//      val pcFct: Option[cncPathFct] = None
//      val modelAllocation: Option[Cnc2DModel => List[Cnc2DModel]] = Some(
//        (model) =>
//      {
//        val convexHull = model.rest.head.asInstanceOf[MultiPolygon]
//        val m1 = model //block hole with targetWorkpiece, remove from rest1
//        val m2 = model // restore set ch as rest1
//        List(m1,m2)
//      }
//      )
//      val l = List(pcFct2)
//      PathCoverageStep(pcFct, None, l, "generic composition")
//    }
//
//    val semanticType = pFct :&: alpha =>:
//      pFct :&: alpha =>:
//      pFct :&: alpha
//  }

  @combinator object GenericCompositionPcStep {
    def apply(pcFct1: PathCoverageStep,
              pcFct2: PathCoverageStep):
    PathCoverageStep = {
      val pcFct: Option[cncPathFct] = None
      val l = List(pcFct1, pcFct2)
      PathCoverageStep(pcFct, None, l, "generic composition")
    }

    val semanticType = pFct :&: alpha =>:
      pFct :&: alpha =>:
      pFct :&: alpha
  }

//
//  @combinator object ConvexHullDecomposition {
//    def apply(pcFct1: PathCoverageStep,
//              pcFct2: PathCoverageStep):
//    PathCoverageStep = {
//      val pcFct: Option[cncPathFct] = None
//      val l = List(pcFct1, pcFct2)
//      PathCoverageStep(pcFct, None, l, "generic composition")
//    }
//
//    val semanticType = pathCoverageFct =>:
//      pathCoverageFct =>:
//      pathCoverageFct
//  }

  //
  //  //org.locationtech.jts.precision.MinimumClearance Decomposition
  @combinator object ZigZagStep {
    def apply(t: CncTool): PathCoverageStep = {
      val fct = { (initialScene: Cnc2DModel, config: PathCoverageStepConfig) =>
        initialScene.rest.foreach(a => logger.info(s"initialScene Rest: $a"))
        val zigZag = new ZigZag {
          override val toolDia = t.d
          override val a_e: Float = t.ae
          override val restGeo: Geometry = initialScene.rest.head
          override val machinedGeo: Geometry = initialScene.machined.reduceOption[Geometry] {
            case (a: Geometry, b: Geometry) => a.union(b)
          }.getOrElse(gf.toGeometry(new Envelope()))
          override val stepDirection: Vector2D = new Vector2D(1.0f, 0.0f)
          override val targetWorkpiece: Geometry = initialScene.targetWorkpiece
          override val targetGeometry: Geometry = initialScene.targetGeometry
        }
        logger.info(s"ZiGZag after init")
        logger.info(s"ZigZag starting for geometry: ${zigZag.restGeo}")
        val boundingPolygon = zigZag.multiLines.buffer(t.d.toDouble / 2.0d)
        val path = zigZag.findPoints
        logger.info(s"zigzag boundingPolygon: $boundingPolygon")
        val newScene = initialScene.withMachinedGeo(boundingPolygon)
        logger.info(s"zigzag after machinedGeo")

        (List(path), newScene)
      }
      PathCoverageStep(Some(fct), Some(t), List.empty[PathCoverageStep])
    }


    val semanticType = alu =>: pFct :&: alu
//    val semanticType = alpha =>: pFct :&: alpha
  }


  /**
   * Selects a polygon (possibly using decomposition techniques), selects a line to traverse
   * (simple case: straight line)
   * Selects area that is nonmachinable, connects with machined, area
   * on and traverses until the area is selected. For every step: build a path as a function of current base point,
   * (initial) geometry, follow the contour, select start end point on linestring
   * if multipolygon remains, decide, what to do, either keep following contour along borders or work on rest parts.
   */
//    @combinator object MultiContourStep extends Contour {
//      def apply(t: CncTool): PathCoverageStep =
//        createMultiContourStep(t)
//
//    val semanticType = alpha =>: pFct :&: alpha
//    }

  @combinator object MultiCountourMultiTool extends Contour {
    def apply(t: CncTool, t2: CncTool): PathCoverageStep = {
      val pc1 = createMultiContourStep(t)
      val pcStepList = pc1.pcrList :+ createFinishContourStep(t2)
      /**
       * mit Tool 1:
       * Loop bis Abbruchbedingung erreicht
       *   Selektion Restgeometrie
       *   Bildung Konturpfad
       *   Update Model
       *
       * mit Tool 2 und neuem Model:
       * Loop bis Abbruchbedingung erreicht
       *   Selektion Restgeometrie
       *   Bildung Konturpfad
       *   Update Model
       */
      PathCoverageStep(pc1.pcFct, pc1.tool, pcStepList, description_param = pc1.description_param)
    }

    val semanticType = alu :&: roughing =>: alu :&: finishing =>: pFct :&: alu
  }

  @combinator object MultiCountourFinishing extends Contour {
    def apply(t: CncTool): PathCoverageStep = {
      val pc1 = createMultiContourStep(t)
      // val pcStepList = pc1.pcrList
      /**
       * mit Tool 1:
       * Loop bis Abbruchbedingung erreicht
       *   Selektion Restgeometrie
       *   Bildung Konturpfad
       *   Update Model
       *
       * mit Tool 2 und neuem Model:
       * Loop bis Abbruchbedingung erreicht
       *   Selektion Restgeometrie
       *   Bildung Konturpfad
       *   Update Model
       */
      // PathCoverageStep(pc1.pcFct, pc1.tool, pcStepList, description_param = pc1.description_param)
      pc1
    }

    val semanticType = alu :&: finishing =>: pFct :&: alu
  }

  //TODO Bugfix
  @combinator object MultiCountourMultiToolSteel extends Contour {
    def apply(t: CncTool, t2: CncTool): PathCoverageStep = {
      val newTool = CncTool(t.d, t.ae / 2.0f,t.ap, t.vf, t.n, t.description + ". Steel ae/2.0", t.idString)
      //case class CncTool(d: Float, ae: Float, ap: Float, vf: Float, n: Int, description: String, idString: String)
      val pc1 = createMultiContourStep(newTool)
      val pcStepList = pc1.pcrList :+ createFinishContourStep(t2)
      /**
       * mit Tool 1:
       * Loop bis Abbruchbedingung erreicht
       *   Selektion Restgeometrie
       *   Bildung Konturpfad
       *   Update Model
       *
       * mit Tool 2 und neuem Model:
       * Loop bis Abbruchbedingung erreicht
       *   Selektion Restgeometrie
       *   Bildung Konturpfad
       *   Update Model
       */
      PathCoverageStep(pc1.pcFct, pc1.tool, pcStepList, description_param = pc1.description_param)
    }

    val semanticType = steel :&: roughing =>: steel :&: finishing =>: pFct :&: steel
  }


  @combinator object SteelRadial extends Contour {
    def apply(t: CncTool): PathCoverageStep = {
      val pathFct :cncPathFct =
        {
          case (m: Cnc2DModel, c: PathCoverageStepConfig) =>
            val primitive = new DirectedRadial {
              override val model: Cnc2DModel = m
              override val tool: CncTool = t
              override val config: PathCoverageStepConfig = c
            }

            logger.info("Combinator step")
            val newModel = m.withMachinedGeo(primitive.machinedGeo)
            logger.info("after newModel")
            (List(primitive.getSteps), newModel)
      }

      PathCoverageStep(Some(pathFct), Some(t), List.empty, description_param = "Steel directed radial progression")
    }

    val semanticType = steel =>: pFct :&: steel
  }


  @combinator object ApplyScene extends JtsUtils {
    def apply(pcs: PathCoverageStep): PathCoverageResult = {
      val bounds = List[Float](0.0f, 50.0f, -15.0f, 40.0f)
      val machinedGeo: Geometry = wktRead("""POLYGON ((0 -15, 0 0, 50 0, 50 -15, 0 -15))""")

      val scene: Cnc2DModel = Cnc2DModel("models/machiningUc1.wkt", bounds).withInitialMachinedGeo(machinedGeo)
      val config = PathCoverageStepConfig()

      PathCoverageResult(scene, config, List(pcs))
    }

    val semanticType = (pFct :&: alpha =>: pFctResult :&: alpha) :&:
      (pathCoverageFctRoot :&: alpha =>: pFctResultRoot :&: alpha)
  }

  //  @combinator object SingleContourStep extends Contour {
  //    def apply(t: CncTool): PathCoverageStep = {
  //      val combinatorPcFunction: cncPathFct = singleContourStep(t)
  //      PathCoverageStep(Some(combinatorPcFunction), Some(t), List.empty[PathCoverageStep])
  //    }
  //
  //    val semanticType = millingTool =>: coveragePrimitive :&: pathCoverageFct
  //  }

// Datenblatt
//  @combinator object AluRoughing {
//    def apply: CncTool = CncTool(12.0f, 12.0f, 6.0f, 1.2750f, 7960,
//      "Alu Roughing, 12mm, Stirnfräsen, Werte aktualisieren", "1 Z S2000")
//
//    val semanticType = alu :&: roughing
//  }


  @combinator object AluRoughing {
    def apply: CncTool = CncTool(12.0f, 3.0f, 6.0f, 3990f, 13300,
      "Alu Roughing, d 12mm, ae 3mm, vf 3990 mm/min, n 13300", "1 Z S13300")

    val semanticType = alu :&: roughing
  }

//  Datenblatt
//  @combinator object AluFinish {
//    def apply: CncTool = CncTool(8.0f, 8.0f, 4.0f, 1.4300f, 11935,
//      "Alu finishing, 8mm, Stirnfraesen, radiale Zustellung 4mm, vf 1430mm/min, n 11935", "2 Z S2000")
//
//    val semanticType = alu :&: finishing
//  }

  @combinator object AluFinish {
    def apply: CncTool = CncTool(8.0f, 4.0f, 4.0f, 1280.0f, 8000,
      "Alu finishing, d 8mm, ae 4mm, vf 1280 mm/min, n 8000", "2 Z S8000")

    val semanticType = alu :&: finishing
  }

  @combinator object SteelRoughing {
    def apply: CncTool = CncTool(d = 10.0f, ae = 6.0f, ap = 10.0f,
      vf = 1948.0f, n = 4775, "Steel Roughing, d: 10mm, radiale Zustellung 6mm, vf 1948mm/min, n 4775", "3 Z S2000")
// korrekt wäre 1625
    val semanticType = steel :&: roughing
  }

  @combinator object SteelFinishing {
    def apply: CncTool = CncTool(d = 5.0f, ae = 0.5f, ap = 7.5f, vf = 380.0f, n = 6365,
      "Steel Finishing, d: 5mm, radiale Zustellung 0.5mm, vf 380mm/min, n 6365", "4 Z S2000")

    val semanticType = steel :&: finishing
  }
}
