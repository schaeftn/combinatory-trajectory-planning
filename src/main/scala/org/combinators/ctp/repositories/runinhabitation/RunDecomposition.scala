package org.combinators.ctp.repositories.runinhabitation

import com.typesafe.scalalogging.LazyLogging
import org.combinators.cls.interpreter.{InhabitationResult, ReflectedRepository}
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.pathcoverage.{CamMpTopRepository}
import org.combinators.ctp.repositories.toplevel._
import org.locationtech.jts.geom.{Coordinate, Geometry, GeometryFactory, LineString, LinearRing, MultiLineString, Polygon}
import org.locationtech.jts.geom.impl.CoordinateArraySequence
import org.locationtech.jts.util.Stopwatch


trait RunDecomposition extends LazyLogging with AkkaImplicits {
  lazy val repository = new CamMpTopRepository {}


  lazy val Gamma = ReflectedRepository(repository)

  logger.debug("kinding: " + Gamma.substitutionSpace.toString)
  logger.debug("Reflected Repository built, starting inhabitation")

  logger.debug(s"# of combinators: ${Gamma.combinators.size}")

  val watch: Stopwatch = new Stopwatch
  watch.start()

  val ihBatch = Gamma.InhabitationBatchJob[Cnc2DModel => (List[List[List[Float]]], Cnc2DModel)](repository.pathCoverageFct)

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
  (500 to 700).map(i => logger.info(s"${l.last.terms.index(i)}"))

  val gf = new GeometryFactory()
  val pArray = Array(new Coordinate(0.0d, 0.0d, 0.0d),
    new Coordinate(0.0d, 10.0d, 0.0d),
    new Coordinate(50.0d, 50.0d, 0.0d),
    new Coordinate(40.0d, 0.0d, 0.0d),
    new Coordinate(50.0d, -50.0d, 0.0d),
    new Coordinate(0.0d, 0.0d, 0.0d))
  val p1: Polygon = new Polygon(new LinearRing(new CoordinateArraySequence(pArray), gf), Array.empty[LinearRing], gf)

  val scene = Cnc2DModel(boundaries = List(-75.0f, 75.0f, -60.0f, 60.0f),
    targetGeometry = p1, rest = List(p1), machined = List.empty[Geometry])

  def printSummary(index: Int, path: List[List[List[Float]]], endScene: Cnc2DModel, fct: Cnc2DModel => (List[List[List[Float]]], Cnc2DModel)): Unit = {
    logger.info(s"index: $index")
    logger.info(s"target geo: \r\n${scene.targetGeometry}")
    logger.info(s"endScene Rest head: \r\n${endScene.rest.head}")
    logger.info(s"endScene RestMultiGeo: \r\n${endScene.getRestMultiGeo}")
    logger.info(s"endScene Machined: \r\n${endScene.getMachinedMultiGeo}")
    logger.info(s"path length: ${path.length}")
    logger.info(s"Result path: \r\n${path}")
    logger.info(s"Corrupt paths : \r\n${path.filterNot(_.length > 1)}")
    val rPath = new MultiLineString(path.filter(_.length > 1).map(p =>
      new LineString(
        new CoordinateArraySequence(
          p.map(i => new Coordinate(i(0), i(1))).toArray), new GeometryFactory())).toArray, gf)
    logger.info(s"Result path: \r\n$rPath")
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
            val fct = l.last.interpretedTerms.index(i).asInstanceOf[Cnc2DModel => (List[List[List[Float]]], Cnc2DModel)]
            val (path, endScene) = fct(scene)
            (i, path, endScene, fct)
          }
        else List.empty

        if(results.nonEmpty){
        val (index, bestPath, bestEndScene, bestfct) = results.minBy { case (_, _, c, _) => c.getRestMultiGeo.getArea }
        printSummary(index, bestPath, bestEndScene, bestfct)} else{logger.info("""Wrong format. Please use "startIndex-endIndex", eg. "100-200"""")}

        logger.info("Done")
      case inputString =>
        inputString.toIntOption match {
          case Some(i) =>
            val (path, endScene) =
              l.last.interpretedTerms.index(i).asInstanceOf[Cnc2DModel => (List[List[List[Float]]], Cnc2DModel)](scene)
            //logger.info(s"info: $path")
            logger.info(s"target geo: \r\n${scene.targetGeometry}")
            logger.info(s"endScene Rest head: \r\n${endScene.rest.head}")
            logger.info(s"endScene RestMultiGeo: \r\n${endScene.getRestMultiGeo}")
            logger.info(s"endScene Machined: \r\n${endScene.getMachinedMultiGeo}")
            logger.info(s"path length: ${path.length}")
            logger.info(s"Result path: \r\n${path}")
            logger.info(s"Corrupt paths : \r\n${path.filterNot(_.length > 1)}")
            logger.info(s"Result path: \r\n${
              new MultiLineString(path.filter(_.length > 1).map(p =>
                new LineString(
                  new CoordinateArraySequence(
                    p.map(i => new Coordinate(i(0), i(1))).toArray), new GeometryFactory())).toArray, gf)
            }")
          //          logger.info(s"Result path: \r\n${
          //          path.filter(_.length > 1).map(p =>
          //            new LineString(
          //              new CoordinateArraySequence(p.map(i => new Coordinate(i(0), i(1))).toArray), new GeometryFactory())).
          //            reduce[Geometry] { case (a: Geometry, b: Geometry) => a.union(b) }
          //        }")
          //  ToAkka(path)
          case None => logger.info("ignoring user input")
        }
    }
  }

  logger.info("Done")
}