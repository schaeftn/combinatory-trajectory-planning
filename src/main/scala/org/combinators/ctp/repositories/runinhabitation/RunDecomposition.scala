package org.combinators.ctp.repositories.runinhabitation

import java.io.{File, FileWriter}

import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.math3.util.{FastMath, MathUtils}
import org.combinators.cls.interpreter.{InhabitationResult, ReflectedRepository}
import org.combinators.cls.types.Taxonomy
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.pathcoverage.{CamMpTopRepository, JtsUtils}
import org.combinators.ctp.repositories.toplevel._
import org.locationtech.jts.algorithm.Angle
import org.locationtech.jts.geom.{Coordinate, Geometry, GeometryFactory, LineString, LinearRing, MultiLineString, Polygon}
import org.locationtech.jts.geom.impl.CoordinateArraySequence
import org.locationtech.jts.io.WKTReader
import org.locationtech.jts.util.Stopwatch
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.parser.decode

import scala.io.Source


trait RunDecomposition extends LazyLogging with AkkaImplicits with JtsUtils {
  lazy val repository = new CamMpTopRepository {}
  val aluUseCase: Boolean = false
  val printKlartext: Boolean = false
  val pRefinement: Boolean = true
  val openPocket: Boolean = true
  val acceptPercentage: Float = 0.005f // vorher: 0.005

  lazy val kinding = if (aluUseCase) repository.aluKinding else repository.steelKinding
  lazy val tgtType = {
    if (openPocket) repository.pFct else repository.pathCoverageFctRoot
  } :&: {
    if (aluUseCase) repository.alu else repository.steel
  }
  lazy val Gamma = ReflectedRepository(repository, Taxonomy.empty, kinding)

  logger.debug("kinding: " + Gamma.substitutionSpace.toString)
  logger.debug("Reflected Repository built, starting inhabitation")

  logger.debug(s"# of combinators: ${Gamma.combinators.size}")

  val watch: Stopwatch = new Stopwatch
  watch.start()

  val ihBatch = Gamma.InhabitationBatchJob[PathCoverageStep](tgtType)

  def getResultList(b: Gamma.InhabitationBatchJob) = {
    @scala.annotation.tailrec
    def getElements(l: List[InhabitationResult[Any]], bnew: b.ResultType): List[InhabitationResult[Any]] =
      bnew match {
        case (newJob: b.ResultType, result: InhabitationResult[Any]) => getElements(result +: l, newJob)
        case a: InhabitationResult[Any] => a +: l
      }

    getElements(List.empty, b.run())
  }

  val l = getResultList(ihBatch)

  watch.stop()
  logger.debug(s"elapsed time ${watch.getTimeString}")

  l.map(i => logger.debug((if (i.isEmpty) "inhabitant not found" else "inhabitant found") + "," +
    i.target.toString()))

  logger.info(s"${l.last.isInfinite}")
  //  (500 to 700).map(i => logger.info(s"${l.last.terms.index(i)}"))

  val wktReader = new WKTReader()

  //  val str: String = Source.fromResource("models/machining2.json").getLines.mkString("\r\n")
  // val tgtGeo = str2Wkt(str)
  val wktStr: String = Source.fromResource("models/machiningUc1.wkt").getLines.mkString("\r\n")
  val tgtGeo = wktReader.read(wktStr)
  logger.info(s"tgtGeo \r\n$tgtGeo")

  //    targetGeometry = tgtGeo, rest = List(tgtGeo), machined = List.empty[Geometry])
//  val bounds = List(tgtGeo.getEnvelopeInternal.getMinX - 5.0,
//    tgtGeo.getEnvelopeInternal.getMaxX+ 5.0,
//    tgtGeo.getEnvelopeInternal.getMinY - 5.0,
//    tgtGeo.getEnvelopeInternal.getMaxY + 5.0).map(_.toFloat)

  val bounds = List[Float](0.0f, 50.0f, -15.0f, 40.0f)

  val machinedGeo = wktReader.read("""POLYGON ((0 -15, 0 0, 50 0, 50 -15, 0 -15))""")
  //val machinedGeo = wktReader.read("""POLYGON ((10 0, 30 0, 30 -15, 10 -15, 10 0))""")
  val scene = Cnc2DModel(boundaries = bounds,
    targetGeometry = tgtGeo, rest = List(tgtGeo), machined = List(), machinedMultiPolygon = emptyGeometry, initialMachined = emptyGeometry).
    withInitialMachinedGeo(machinedGeo)

  def str2Wkt(s: String) = {
    val pSceneOpt = decode[PolygonScene](s) match {
      case Left(s) => logger.info(s"Could not decode String: $s")
        None
      case Right(s) => Some(s)
    }

    val vertices = pSceneOpt.map(pScene => pScene.obstacles.
      map(o => o.map(vid => pScene.vertices(vid)))).getOrElse(List.empty)
    val polyList = vertices.filter(v => v.map(point => point(2)).max == 0.0).map(poly => poly.map(asCoordinate)).
      map(getPolygonByCoordList)
    val multiPoly = gf.createMultiPolygon(polyList.toArray)
    logger.info(s"multiPoly: $multiPoly")
    multiPoly
  }

  val config = PathCoverageStepConfig(pRefinement)

  def printSummary(index: Int, pcr: PathCoverageResult): Unit = {
    val endScene = pcr.computeModelHistory._1.last
    val pathList = pcr.computeModelHistory._2
    logger.info(s"index: $index")
    logger.info(s"target geo: \r\n${scene.targetGeometry}")
    logger.info(s"endScene Rest head: \r\n${endScene.rest.head}")
    logger.info(s"endScene RestMultiGeo: \r\n${endScene.getRestMultiGeo}")
    logger.info(s"endScene Machined: \r\n${endScene.getMachinedMultiGeo}")
    logger.info(s"pathList length: ${pathList.length}")
    logger.info(s"Result pathList: \r\n${pathList}")
    logger.info(s"Corrupt paths : \r\n${pathList.filterNot(_.length > 1)}")
    //logger.info(s"pathAndVfMax: ${pcr.withMaxVfByAngle}")
    val rPath = new MultiLineString(pathList.filter(_.length > 1).map(p =>
      new LineString(
        new CoordinateArraySequence(
          p.map(i => new Coordinate(i(0), i(1))).toArray), new GeometryFactory())).toArray, gf)
    logger.info(s"Result path: \r\n$rPath")
    //    logger.info(s"pcr ${pcr.withMaxVfByAcc}")
    //    pcr.printAll()
  }
}

object RunCncPathCoverage extends App with RunDecomposition {
  lazy val lines = Iterator.continually(scala.io.StdIn.readLine()).takeWhile(_ != "exit")
  while (lines.hasNext) {
    lines.next() match {
      case inputString if inputString.contains("-") =>
        val values = inputString.split("-").map(i => i.toIntOption).filter {
          case Some(a) => true
          case _ => false
        }.map(_.get)
        val results = if (values.length > 1)
          (values.head to values.last) map { i: Int =>
            val fct = l.last.interpretedTerms.index(i).asInstanceOf[PathCoverageStep]
            val pcr = PathCoverageResult(scene, config, List(fct))
            (i, pcr)
          }
        else List.empty

        if (results.nonEmpty) {
          val (index, pcr) = results.minBy { case (_, p: PathCoverageResult) => p.computeModelHistory._1.last.getRestMultiGeo.getArea }
          printSummary(index, pcr)
        } else {
          logger.info("""Wrong format. Please use "startIndex-endIndex", eg. "100-200"""")
        }

        logger.info("Done")
      case inputString if inputString.equals("it") =>
        val d = new File(".")
        val newFile = if (d.exists && d.isDirectory)
          d.listFiles.filter(f => f.isFile && f.getName.startsWith("run_out")).toList.maxBy(_.getName).getName + "1"
        else "asdasd"

        def getResults(accList: List[(Int, PathCoverageResult)], i: Int): List[(Int, PathCoverageResult)] = {
          logger.info(s"inhabitant: $i")
          val tree = true // s"${l.last.terms.index(i)}".startsWith("Tree(GenericCompositionPcStep,org.combinators.ctp.repositories.toplevel.PathCoverageStep & pFct,ArraySeq(Tree(ZigZagStep")
          if (accList.size > 10) {
            accList
          } else {
            if (tree) {
              val fct = l.last.interpretedTerms.index(i).asInstanceOf[PathCoverageStep]
              val pcr = PathCoverageResult(scene, config, List(fct))
              val restarea = pcr.computeModelHistory._1.last.getRestMultiGeo.getArea
              val initialRest = scene.getRestMultiGeo.getArea
              val percentage = restarea / initialRest
              val fw = new FileWriter("run_out.txt", true);
              fw.write(s"$i: \r\n${pcr.computeModelHistory._1.last.machinedPolygonHistory}\r\n")
              fw.close()

              val newList = if (percentage < acceptPercentage && tree) {
                val fw = new FileWriter(newFile, true);
                fw.write(s"$i: \r\n${l.last.terms.index(i)}\r\n${pcr.computeModelHistory._1.last.machinedPolygonHistory}\r\n")
                val pathList = pcr.pathList
                val rPath = new MultiLineString(pathList.filter(_.length > 1).map(p =>
                  new LineString(
                    new CoordinateArraySequence(
                      p.map(i => new Coordinate(i(0), i(1))).toArray), new GeometryFactory())).toArray, gf)

                fw.write(s"${rPath}\r\n")
                fw.close()
                accList :+ (i, pcr)
              }
              else {
                accList
              }
              getResults(newList, i + 1)
            } else {
              getResults(accList, i + 1)
            }
          }
        }

        val selectedResults = getResults(List.empty, 0)
        logger.info(s"selected Indizes: ${selectedResults.map(_._1)}")
        logger.info(s"Times: ${selectedResults.map(_._2.pathTime)}")
        logger.info(s"with subpaths: ${selectedResults.map(_._2.pathTimesCalculated)}")
        logger.info("Done")

      case inputString =>
        inputString.toIntOption match {
          case Some(i) =>
            logger.info(s"${l.last.terms.index(i)}")
            val pcs =
              l.last.interpretedTerms.index(i).asInstanceOf[PathCoverageStep]
            //logger.info(s"info: $path")
            val pcr = PathCoverageResult(scene, config, List(pcs))
            val endScene = pcr.endScene
            val path = pcr.pathList
            logger.info(s"target geo: \r\n${scene.targetGeometry}")
            logger.info(s"endScene Rest head: \r\n${endScene.rest.head}")
            logger.info(s"endScene RestMultiGeo: \r\n${endScene.getRestMultiGeo}")
            logger.info(s"endScene Machined: \r\n${endScene.getMachinedMultiGeo}")
            logger.info(s"endScene Machined history: \r\n${endScene.machinedPolygonHistory}")
            logger.info(s"path length: ${path.length}")
            logger.info(s"Result path: \r\n${path}")
            logger.info(s"Corrupt paths : \r\n${path.filterNot(_.length > 1)}")

            def extractZZeroLines(p: List[List[Float]]): List[List[List[Float]]] = {
              if (p.isEmpty)
                List.empty[List[List[Float]]]
              else if (p.head.size == 3 && p.head(2) == 20.0f)
                extractZZeroLines(p.tail)
              else {
                val asd = p.takeWhile(i => i.size < 3 || i(2) == 0.0f)
                logger.info(s"asd size ${asd.size}")
                logger.info(s"p before ${p.size}")
                logger.info(s"p after ${p.drop(asd.size)}")
                if (asd.isEmpty)
                  extractZZeroLines(p.dropWhile(i => i.size > 2 && i(2) != 0.0f))
                else
                  asd +: extractZZeroLines(p.drop(asd.size))
              }
            }

            val newPath = path.filter(_.length > 1).map(extractZZeroLines).reduceOption(_ ++ _).getOrElse(List.empty)
            if (newPath.nonEmpty) {
              val toPaths = newPath.map { singleCompPathlist =>
                new LineString(
                  new CoordinateArraySequence(
                    singleCompPathlist.map(c => new Coordinate(c(0), c(1))).toArray)
                  , gf)
              }

              val mlString = new MultiLineString(toPaths.toArray, gf)

              logger.info(s"Result path: \r\n${mlString}")
              pcr.computeModelHistory._1.foreach(i => logger.info(s"\r\n${i.getMachinedMultiGeo}"))
              if (printKlartext) {
                pcr.printAll()
              }

              logger.info(s"Result path: \r\n${mlString}")
              logger.info(s"endScene Machined history: \r\n${endScene.machinedPolygonHistory}")

            }
            else {
              logger.info(s"empty path")
            }

          case None => logger.info("ignoring user input")
        }
    }
  }

  logger.info("Done")
}