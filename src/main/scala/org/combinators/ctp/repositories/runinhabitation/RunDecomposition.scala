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
  val aluUseCase: Boolean = true
  val printKlartext: Boolean = false
  val acceptPercentage: Float = 0.002f // vorher: 0.005

  lazy val kinding = if (aluUseCase) repository.aluKinding else repository.steelKinding
  lazy val tgtType = if (aluUseCase) repository.pathCoverageFctRoot :&: repository.alu
  else repository.pathCoverageFctRoot :&: repository.steel
  lazy val Gamma = ReflectedRepository(repository, Taxonomy.empty, kinding)

  logger.debug("kinding: " + Gamma.substitutionSpace.toString)
  logger.debug("Reflected Repository built, starting inhabitation")

  logger.debug(s"# of combinators: ${Gamma.combinators.size}")

  val watch: Stopwatch = new Stopwatch
  watch.start()

  val ihBatch = Gamma.InhabitationBatchJob[PathCoverageStep](repository.pFct)

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
  //  val tgtGeo = wktReader.read("""POLYGON ((0 0, 0 10, 50 50, 40 0, 50 -50, 0 0),
  //                               |  (18 10, 18 6, 18 -2, 24 -2, 24 6, 30 6, 30 10, 24 10, 18 10))""".stripMargin)
  //
  //  val tgtGeo = wktReader.read("""POLYGON ((0 0, 0 10, 70 70, 60 0, 70 -50, 0 0),
  //                                |  (18 10, 18 6, 18 -2, 24 -2, 24 6, 30 6, 30 10, 24 10, 18 10))""".stripMargin)
  //
  //  val tgtGeo = wktReader.read("""MULTIPOLYGON (((5 4, 5 60, 40 60, 40 4, 5 4)))""".stripMargin)
  //val tgtGeo = wktReader.read("""POLYGON ((20 20, 20 0, 0 0, 0 20, 20 20)))""".stripMargin)
  //  val tgtGeo = wktReader.read("""POLYGON ((0 5, 0 95, 0.0759610012173653 95.86824035644531, 0.3015370070934296 96.71009826660156, 0.6698729991912842 97.5, 1.1697779893875122 98.21393585205078, 1.7860620021820068 98.8302230834961, 2.5 99.33012390136719, 3.2898991107940674 99.6984634399414, 4.131759166717529 99.92404174804688, 5 100, 95 100, 95.86824035644531 99.92404174804688, 96.71009826660156 99.6984634399414, 97.5 99.33012390136719, 98.21393585205078 98.8302230834961, 98.8302230834961 98.21393585205078, 99.33012390136719 97.5, 99.6984634399414 96.71009826660156, 99.92404174804688 95.86824035644531, 100 95, 100 5, 99.92404174804688 4.131759166717529, 99.6984634399414 3.2898991107940674, 99.33012390136719 2.5, 98.8302230834961 1.7860620021820068, 98.21393585205078 1.1697779893875122, 97.5 0.6698729991912842, 96.71009826660156 0.3015370070934296, 95.86824035644531 0.0759610012173653, 95 0, 5 0, 4.131759166717529 0.0759610012173653, 3.2898991107940674 0.3015370070934296, 2.5 0.6698729991912842, 1.7860620021820068 1.1697779893875122, 1.1697779893875122 1.7860620021820068, 0.6698729991912842 2.5, 0.3015370070934296 3.2898991107940674, 0.0759610012173653 4.131759166717529, 0 5),
  //                                |  (50 15, 70 15, 70 20, 79.51886749267578 20, 79.71178436279297 20, 79.92462921142578 20, 80.15074157714844 20, 80.38346862792969 20, 80.61628723144531 20, 80.84294128417969 20, 81.05754852294922 20, 81.254638671875 20, 81.42925262451172 20, 85 20, 85 50, 60 50, 58.263519287109375 49.848079681396484, 56.57979965209961 49.39692687988281, 55 48.660255432128906, 54.42972183227539 48.26094055175781, 54.16383361816406 48.074764251708984, 53.86625289916992 47.866397857666016, 53.57212448120117 47.66044235229492, 53.54927062988281 47.63759231567383, 53.263980865478516 47.35230255126953, 52.974609375 47.062931060791016, 52.689002990722656 46.77732467651367, 52.41493606567383 46.503257751464844, 52.33955764770508 46.42787551879883, 52.192283630371094 46.217552185058594, 52.00510025024414 45.950225830078125, 51.339744567871094 45, 50.60307312011719 43.42020034790039, 50.151920318603516 41.736480712890625, 50 40, 50 15))""".stripMargin)

  val str: String = Source.fromResource("models/machining2.json").getLines.mkString("\r\n")
  //val tgtGeo = str2Wkt(str)
  val wktStr: String = Source.fromResource("models/machining5.wkt").getLines.mkString("\r\n")
  val tgtGeo = wktReader.read(wktStr)
  // logger.info(s"tgtGeo \r\n$tgtGeo")
  //  val tgtGeo = wktReader.read("""POLYGON ((-10 42, 15 42, 15 35, 17 35, 17 42, 62 42, 62 -33, -10 -33, -10 42))""".stripMargin)
  //val tgtGeo = wktReader.read("""POLYGON ((0 0, 0 40, 50 40, 50 0, 30 -0, 30 10, 36 10, 36.6945915222168 10.060769081115723, 37.368080139160156 10.241230010986328, 38 10.535898208618164, 38.57115173339844 10.935821533203125, 39.064178466796875 11.428850173950195, 39.46410369873047 12, 39.75876998901367 12.631918907165527, 39.939231872558594 13.30540657043457, 40 14, 40 24, 39.90884780883789 25.041889190673828, 39.63815689086914 26.052120208740234, 39.19615173339844 27, 38.59626770019531 27.856725692749023, 37.856727600097656 28.596267700195312, 37 29.196151733398438, 36.68108367919922 29.344865798950195, 36.052120208740234 29.63815689086914, 35.04188919067383 29.90884780883789, 34 30, 20 30, 18.263517379760742 29.84807777404785, 16.57979965209961 29.396926879882812, 15 28.660253524780273, 13.572123527526855 27.660444259643555, 12.339555740356445 26.427875518798828, 11.339746475219727 25, 10.603074073791504 23.42020034790039, 10.151922225952148 21.736482620239258, 10 20, 10 8, 10 0, 0 0))""".stripMargin)
  // val tgtGeo = wktReader.read("""POLYGON ((10 0, 10 8, 10 20, 10.151922225952148 21.736482620239258, 10.603074073791504 23.42020034790039, 11.339746475219727 25, 12.339555740356445 26.427875518798828, 13.572123527526855 27.660444259643555, 15 28.660253524780273, 16.57979965209961 29.396926879882812, 18.263517379760742 29.84807777404785, 20 30, 34 30, 35.04188919067383 29.90884780883789, 36.052120208740234 29.63815689086914, 36.68108367919922 29.344865798950195, 37 29.196151733398438, 37.856727600097656 28.596267700195312, 38.59626770019531 27.856725692749023, 39.19615173339844 27, 39.63815689086914 26.052120208740234, 39.90884780883789 25.041889190673828, 40 24, 40 14, 39.939231872558594 13.30540657043457, 39.75876998901367 12.631918907165527, 39.46410369873047 12, 39.064178466796875 11.428850173950195, 38.57115173339844 10.935821533203125, 38 10.535898208618164, 37.368080139160156 10.241230010986328, 36.6945915222168 10.060769081115723, 36 10, 30 10, 30 -0, 10 0))""".stripMargin)
  //  val tgtGeo = wktReader.read("""POLYGON ((-55.5 77.5, 121 77.5, 121 -87, -55.5 -87, -55.5 77.5),
  //                                |  (10 10, 10 -30, 30 -30, 30 -10, 50 -10, 50 20, 20 20, 20 19.995327360502255, 19.470765458798372 19.985985720017784, 18.767682731028415 19.92377922711879, 18.070739345905032 19.812132965291546, 17.383407440315594 19.651603150596497, 16.7091112672399 19.44298953449556, 16.051210136294088 19.18733141953056, 15.412981677819097 18.88590248157027, 14.79760551389118 18.540204424421262, 14.208147417604668 18.151959498415124, 13.647544039544615 17.72310192024389, 13.118588277541312 17.255768236789827, 12.623915362593777 16.752286680956306, 12.165989730281474 16.215165572528594, 11.747092743070228 15.64708082185087, 11.36931132467916 15.050862598575474, 11.034527563131714 14.42948123190008, 10.74440933428779 13.786032412537036, 10.500401992570092 13.12372177013794, 10.303721170281118 12.445848903008002, 10.155346721384166 11.755790939673208, 10.05601783992018 11.05698571419593, 10.006229377380219 10.352914639058527, 10.006229377380219 10, 10 10))""".stripMargin)
  //  val scene = Cnc2DModel(boundaries = List(-20.0f, 80.0f, -60.0f, 60.0f),
  //    targetGeometry = tgtGeo, rest = List(tgtGeo), machined = List.empty[Geometry])
  val bounds = List(tgtGeo.getEnvelopeInternal.getMinX - 5.0,
    tgtGeo.getEnvelopeInternal.getMaxX+ 5.0,
    tgtGeo.getEnvelopeInternal.getMinY - 5.0,
    tgtGeo.getEnvelopeInternal.getMaxY + 5.0).map(_.toFloat)

  val machinedGeo = wktReader.read("""POLYGON ((0 -15, 0 0, 50 0, 50 -15, 0 -15))""")
  //val machinedGeo = wktReader.read("""POLYGON ((10 0, 30 0, 30 -15, 10 -15, 10 0))""")
  val scene = Cnc2DModel(boundaries = bounds,
    targetGeometry = tgtGeo, rest = List(tgtGeo), machined = List(), machinedMultiPolygon = emptyGeometry)

  def str2Wkt(s: String) = {
    val pSceneOpt = decode[PolygonScene](s) match {
      case Left(s) => logger.info(s"Could not decode String: $s")
        None
      case Right(s) => Some(s)
    }

    val vertices = pSceneOpt.map(pScene => pScene.obstacles.map(o => o.map(vid => pScene.vertices(vid)))).getOrElse(List.empty)
    val polyList = vertices.filter(v => v.map(point => point(2)).max == 0.0).map(poly => poly.map(asCoordinate)).map(getPolygonByCoordList)
    val multiPoly = gf.createMultiPolygon(polyList.toArray)
    logger.info(s"multiPoly: $multiPoly")
    multiPoly
  }

  val config = PathCoverageStepConfig()

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
              fw.write(s"${pcr.computeModelHistory._1.last.machinedPolygonHistory}\r\n")
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