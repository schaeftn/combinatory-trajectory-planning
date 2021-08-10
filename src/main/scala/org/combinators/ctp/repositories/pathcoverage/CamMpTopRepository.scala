package org.combinators.ctp.repositories.pathcoverage

import com.typesafe.scalalogging.LazyLogging
import org.combinators.cls.interpreter.combinator
import org.combinators.cls.types.syntax._
import org.combinators.cls.types.{Constructor, Kinding, Type, Variable}
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.toplevel.{Cnc2DModel, PathCoverageStep, PathCoverageStepConfig}
import org.locationtech.jts.algorithm.ConvexHull
import org.locationtech.jts.algorithm.construct.MaximumInscribedCircle
import org.locationtech.jts.geom.impl.CoordinateArraySequence
import org.locationtech.jts.geom._
import org.locationtech.jts.math.Vector2D
import org.locationtech.jts.operation.distance.DistanceOp
import org.locationtech.jts.util.GeometricShapeFactory

case class CncTool(d: Float, ae: Float, ap: Float, vf: Float, n: Int, description: String, idString: String) {
  lazy val r: Double = d / 2.0d
}

trait CamMpTopRepository extends LazyLogging with JtsUtils {

  val pcFctRoot = Constructor("pcFctRoot")
  val pcFct = Constructor("pcFct")

  val roughing = Constructor("roughing")
  val finishing = Constructor("finishing")
  val steel: Type = Constructor("steel")
  val alu: Type = Constructor("alu")
  val alpha = Variable("alpha")

  val pFctResult: Type = Constructor("pFctResult")
  val pFctResultRoot: Type = Constructor("pFctResultRoot")

  val atomicStep: Type = Constructor("atomicStep")
  lazy val kindingComplete: Kinding = buildKinding(Map(alpha -> Seq(steel, alu)))
  lazy val aluKinding: Kinding = buildKinding(Map(alpha -> Seq(alu)))
  lazy val steelKinding: Kinding = buildKinding(Map(alpha -> Seq(steel)))

  @combinator object SpiralRoughing {
    def apply(t: CncTool,
              restMp: PathCoverageStep): PathCoverageStep = {
      val pcFct: cncPathFct = { (initialScene, pcConfig) =>
        logger.debug(s"spiral roughing: ${initialScene.rest.head}")
        val largestEmptyCircle = new MaximumInscribedCircle(initialScene.rest.head, 1.0)
        val c1 = largestEmptyCircle.getCenter.getCoordinate
        val c11 = new Coordinate(c1.x, c1.y, 20.0f)
        val c2 = largestEmptyCircle.getCenter.getCoordinate
        val c22 = new Coordinate(c2.x, c2.y, 0.0f)

        logger.debug(s"c11: $c11, c22: $c22")
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
      PathCoverageStep(Some(pcFct), Some(t), List(restMp), "SpiralRoughing")
    }

    val semanticType = alpha :&: roughing =>: pcFct :&: alpha =>: pcFctRoot :&: alpha
  }

/**
  @combinator object MoatIntermediateAreaIsles{
    def apply(t: CncTool): PathCoverageStep = {
// Build envelope
      // select poly in between polygons
      // Moat contour workpiece
      // find biggest contour that has any leftover material in its proximity
      val pFct: cncPathFct = {
        case (model, pcConfig) =>
          logger.debug(s"MoatIntermediateAreaIsles start")
          val holesMultiGeo = findHoles(model.targetWorkpiece)
          pGeo(s"MoatIntermediateAreaIsles holes:", holesMultiGeo.getOrElse(emptyGeometry))

          val chulls: List[Geometry] = holesMultiGeo.map(g => getGeoListFromGeo(g).map {
            new ConvexHull(_)
          }).map { chList => chList.map(_.getConvexHull) }.getOrElse(List.empty[Geometry])
          pGeo(s"MoatIntermediateAreaIsles cHulls:", getGeoCollection(chulls))

          val selectedChull = chulls.maxBy{g => g.buffer(t.d).intersection(model.getRestMultiGeo).getArea}
          pGeo("selectedChull", selectedChull)
          val validPositions = model.getMachinedMultiGeo.buffer(- t.d/2.0d)
          pGeo("validPositions", validPositions)
          val distanceOp = new DistanceOp(selectedChull, validPositions)
          val nPoints = distanceOp.nearestPoints()
          pGeo("nPoints", getGeoCollection(nPoints.toList.map(gf.createPoint)))
          val origTargetGeo = model.targetGeometry
          (List.empty[List[List[Float]]], model)
      }
      PathCoverageStep(Some(pFct), Some(t), List(), "Moat MoatIntermediateAreaIsles")

      // find free area in next proximity or smallest distance to machined area

    }
    val semanticType = alpha =>: pcFct :&: alpha :&: atomicStep
  }
**/

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

        logger.debug(s"tool: d: ${t.d}")
        logger.debug(s"computung diffgeo Moat")
        logger.debug(s"initialScene.rest.head ${initialScene.rest.head}")
        val selectLines = moatPrimitive.selectLines
        if (selectLines.nonEmpty) {
          logger.debug(s"asd.selectLines: $selectLines")
          val ml = new MultiLineString(selectLines, new GeometryFactory())
          logger.debug(s"asd.selectLines: $ml")
          // logger.info(s"asd.selectLines.buffer: ${ml.buffer(t.d)}")
        }
        else
          logger.debug(s"asd.selectLines: empty")
        logger.debug(s"asd.returnPath Length: ${moatPrimitive.returnPath.length}")

        val diffGeo = moatPrimitive.getPolygon
        logger.debug(s"diffgeo: $diffGeo")
        val newScene = initialScene.withMachinedGeo(diffGeo)
        logger.debug(s"After rPath")
        (List(moatPrimitive.returnPath), newScene)
      }
      PathCoverageStep(Some(combinatorPcFct), Some(t), List(restMp), "Moat Composition")
    }

    val semanticType = alpha =>: pcFct :&: alpha  =>: pcFctRoot :&: alpha
  }

  @combinator object GenericCompositionPcStep {
    def apply(pcFct1: PathCoverageStep,
              pcFct2: PathCoverageStep):
    PathCoverageStep = {
      val pcFct: Option[cncPathFct] = None
      val l = List(pcFct1, pcFct2)
      PathCoverageStep(pcFct, None, l, "Generic Composition")
    }

    val semanticType = pcFct :&: alpha =>:
      pcFct :&: alpha :&: atomicStep =>:
      pcFct :&: alpha
  }

  @combinator object ConvexHullDecomposition {
    def apply(pcStep: PathCoverageStep): PathCoverageStep = {
      val pathCoverageFunctionBlock: (Cnc2DModel, PathCoverageStepConfig) => (List[List[List[Float]]], Cnc2DModel) = {
        case (model, config) =>
          val holesMultiGeo = findHoles(model.getRestMultiGeo)
          pGeo(s"CHull Combinator holes:", holesMultiGeo.getOrElse(emptyGeometry))

          val chulls: List[Geometry] = holesMultiGeo.map(g => getGeoListFromGeo(g).map {
            new ConvexHull(_)
          }).map { chList => chList.map(_.getConvexHull) }.getOrElse(List.empty[Geometry])
          pGeo(s"CHull Combinator cHulls:", getGeoCollection(chulls))

          val origTargetGeo = model.targetGeometry
          val gMultiGeo = chulls.reduceOption(_ union _).getOrElse(emptyGeometry)
          val updatedModel = model.blockGeometry(gMultiGeo)

          (List.empty[List[List[Float]]], updatedModel)
      }

      val pathCoverageFunctionRelease: (Cnc2DModel, PathCoverageStepConfig) => (List[List[List[Float]]], Cnc2DModel) = {
        case (model, config) =>
          (List.empty[List[List[Float]]],  model.releaseGeometry())
      }



      val pc1 = PathCoverageStep(Some(pathCoverageFunctionBlock), None, List(), "convex hull block operation")
      val pc3 = PathCoverageStep(Some(pathCoverageFunctionRelease), None, List(), "convex hull release operation")
      PathCoverageStep(None, None, List(pc1, pcStep, pc3), "convex hull wrapper")
    }
    val semanticType = pcFct :&: alpha =>:
      pcFct :&: alpha
  }

  @combinator object SpecimenContour {
    def apply(t: CncTool): PathCoverageStep = {
      val pcFunction: cncPathFct = {
        case (model, config) =>
          val holesMultiGeo = findHoles(model.targetGeometry)
          pGeo(s"CHull Combinator holes:", holesMultiGeo.getOrElse(emptyGeometry))
          val islesList = getGeoListFromGeo(holesMultiGeo.getOrElse(emptyGeometry))
          val bufferedIsles = islesList.map { g => robustDifference(g.buffer(t.d), model.machinedMultiPolygon) }
          val selectedIsleTuple = (islesList zip bufferedIsles).maxBy { case (g1, g2) => robustDifference(g2, g1).getArea }
          val selectedIsle = smartCastToPolygon(selectedIsleTuple._1)

          val toolPath = selectedIsle.buffer(t.r).asInstanceOf[Polygon].getExteriorRing
          val tpBuffer = toolPath.buffer(t.r)

          pGeo("SpecimenContour tpBuffer", tpBuffer)
          //Connect from machined area tool posis
          //Update model
          val updatedRestList: List[Geometry] =
          model.rest.map {
            _.buffer(-0.00001).buffer(0.00001).
              difference(tpBuffer.buffer(-0.00001).buffer(0.00001))
          }.map(g => getGeoListFromGeo(g)).reduce(_ ++ _)
          val updatedRestRemovedA = updatedRestList.partition { restGeo =>
            restGeo.isWithinDistance(toolPath, t.r + 0.01d) && restGeo.getArea < 0.1d
          }

          val geoCollection = gf.createGeometryCollection((List( model.machinedMultiPolygon,tpBuffer ) ++ updatedRestRemovedA._1).toArray)
          val newMachinedMultiPolygon: Geometry =geoCollection.buffer(0)

//            updatedRestRemovedA._1.foldLeft(
//              model.machinedMultiPolygon.buffer(0).union(tpBuffer.buffer(0))) {
//              case (accGeo, newGeo) => accGeo.buffer(0).union(newGeo.buffer(0)) }
          val newMachinedList: List[Geometry] = (model.machined :+ tpBuffer) ++ updatedRestRemovedA._1

          //        remove areas that are in vicinity of the workpiece and are under area theshhold
          //        difference(toolPath.buffer(t.r))
          val newModel = Cnc2DModel(boundaries = model.boundaries,
            targetGeometry = model.targetGeometry,
            rest = updatedRestRemovedA._2,
            machined = newMachinedList,
            machinedMultiPolygon = newMachinedMultiPolygon,
            initialMachined = model.initialMachined,
            transformStack = model.transformStack)

          (List(asFloatList(toolPath.getCoordinates)), newModel)
      }
      PathCoverageStep(Some(pcFunction), Some(t), List.empty[PathCoverageStep], """Specimen Contour""")
    }

    val semanticType = alpha =>: pcFct :&: alpha :&: atomicStep
  }


  //  //org.locationtech.jts.precision.MinimumClearance Decomposition
  @combinator object ZigZagStep {
    def apply(t: CncTool): PathCoverageStep = {
      val fct = { (initialScene: Cnc2DModel, config: PathCoverageStepConfig) =>
        pGeo("initialScene Rest", getGeoCollection(initialScene.rest))
        val zigZag = new ZigZag {
          override val toolDia = t.d
          override val a_e: Float = t.ae
          override val restGeo: Geometry = initialScene.rest.head
          override val machinedGeo: Geometry = initialScene.machined.reduceOption[Geometry] {
            case (a: Geometry, b: Geometry) => robustUnion(a,b)
          }.getOrElse(gf.toGeometry(new Envelope()))
          override val stepDirection: Vector2D = new Vector2D(1.0f, 0.0f)
          override val targetWorkpiece: Geometry = initialScene.targetWorkpiece
          override val targetGeometry: Geometry = initialScene.targetGeometry
        }
        logger.debug(s"ZiGZag after init")
        logger.debug(s"ZigZag starting for geometry: ${zigZag.restGeo}")
        val path = zigZag.findPoints
        val bufferedPoly = getNewLineString(path.map{l => asCoordinate(l)}.toArray).buffer(t.d.toDouble / 2.0d)
        logger.debug(s"zigzag bufferedPoly: $bufferedPoly")
        val newScene = initialScene.withMachinedGeo(bufferedPoly)
        logger.debug(s"zigzag after machinedGeo")

        (List(path), newScene)
      }
      PathCoverageStep(Some(fct), Some(t), List.empty[PathCoverageStep], """Zig-Zag""")
    }


    val semanticType = alu :&: roughing =>: pcFct :&: alu :&: atomicStep
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

  object RotateModelPcsObj {
    def apply(angle: Double): PathCoverageStep = {
      val pathCoverageFunction: cncPathFct = {
        case (model, _) =>
          val rotatedModel = model.rotateModel(angle)
          (List.empty[List[List[Float]]], rotatedModel)
      }

      val resultTransformFct: List[List[List[Float]]] => List[List[List[Float]]] =
        (l: List[List[List[Float]]]) => rotateFloatList(l, angle)

      PathCoverageStep(Some(pathCoverageFunction), None, List.empty[PathCoverageStep],
        description_param = s"RotationOperation $angle",
        resultTransform = Some(resultTransformFct))
    }
  }

  @combinator object RotateModelPcs {
    def apply(p1: PathCoverageStep): PathCoverageStep = {
      val rotateSequence = List(RotateModelPcsObj(math.Pi / 2), p1, RotateModelPcsObj(-math.Pi / 2))
      PathCoverageStep(None, None, rotateSequence, description_param = s"RotationCombinator for: ${p1.description_param}")
    }

    val semanticType = pcFct :&: alpha =>: pcFct :&: alpha :&: atomicStep
  }

  @combinator object MultiContourMultiTool extends Contour {
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

    val semanticType = alu :&: roughing =>: alu :&: finishing =>: pcFct :&: alu :&: atomicStep
  }

  /**
  @combinator object MultiContourFinishing extends Contour {
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

    val semanticType = alu :&: finishing =>: pcFct :&: alu :&: atomicStep
  }
**/

  //TODO Bugfix
  @combinator object MultiContourMultiToolSteel extends Contour {
    def apply(t: CncTool, t2: CncTool): PathCoverageStep = {
      val newTool = CncTool(t.d, t.ae / 2.0f, t.ap, t.vf, t.n, t.description + ". Steel ae/2.0", t.idString)
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

    val semanticType = steel :&: roughing =>: steel :&: finishing =>: pcFct :&: steel
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

            logger.debug("Combinator step")
            val newModel = m.withMachinedGeo(primitive.machinedGeo)
            logger.debug("after newModel")
            (List(primitive.getSteps), newModel)
      }

      PathCoverageStep(Some(pathFct), Some(t), List.empty, description_param = "Steel directed radial progression")
    }

    val semanticType = steel =>: pcFct :&: steel
  }

  val bounds1 = List[Float](0.0f, 50.0f, -15.0f, 40.0f)
  val machinedGeo1: Geometry = wktRead("""POLYGON ((0 -15, 0 0, 50 0, 50 -15, 0 -15))""")
  val scene1: Cnc2DModel = Cnc2DModel("models/machiningUc1.wkt", bounds1).withInitialMachinedGeo(machinedGeo1)
  val config = PathCoverageStepConfig()

  val bounds6 = List[Float](-40.0f, 60.0f, -20.0f, 70.0f)
  val scene6: Cnc2DModel = Cnc2DModel("models/machining6.wkt", bounds6)

  //  @combinator object ApplyScene1 extends JtsUtils {
  //    def apply(pcs: PathCoverageStep): PathCoverageResult = {
  //      PathCoverageResult(scene1, config, List(pcs))
  //    }
  //
  //    val semanticType = (pFct :&: alpha =>: pFctResult :&: alpha) :&:
  //      (pathCoverageFctRoot :&: alpha =>: pFctResultRoot :&: alpha)
  //  }

  @combinator object ApplyScene6 extends JtsUtils {
    def apply(pcs: PathCoverageStep): PathCoverageResult = {
      PathCoverageResult(scene6, config, pcs)
    }

    val semanticType = (pcFct :&: alpha =>: pFctResult :&: alpha) :&:
      (pcFctRoot :&: alpha =>: pFctResultRoot :&: alpha)
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
    def apply: CncTool = CncTool(5.0f, 2.0f, 6.0f, 3990f, 13300,
      "Alu Roughing, d 5mm, ae 2mm, vf 3990 mm/min, n 13300", "1 Z S13300")
      // Orig "Alu Roughing, d 12mm, ae 3mm, vf 3990 mm/min, n 13300", "1 Z S13300")

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
