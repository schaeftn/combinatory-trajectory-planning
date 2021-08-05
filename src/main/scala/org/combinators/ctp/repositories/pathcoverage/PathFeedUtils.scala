package org.combinators.ctp.repositories.pathcoverage

import java.io.File

import org.apache.commons.math3.util.FastMath
import org.locationtech.jts.algorithm.Angle
import org.locationtech.jts.geom.Coordinate

import scala.annotation.tailrec
import scala.math.min

trait PathFeedUtils extends JtsUtils with PathRefinement {
  val pcr: PathCoverageResult
  lazy val config = pcr.config
  lazy val toolList = pcr.toolList

  def vfMaxSegment(c1: Coordinate, c2: Coordinate, c3: Coordinate): Float = {
    val ang = Angle.angleBetween(c1, c2, c3)
    val asd = pcr.config.vfMaxByAngle(ang)
    // logger.info(s"angleVelocity: $asd ${asd.toFloat}")
    asd.toFloat
  }

  def checkMinPathClearanceViolation(singlePath: List[List[Float]]): List[Float] =
    singlePath.reduce[List[Float]] { case (a, b) =>
      if (asCoordinate(a).distance(asCoordinate(b)) < config.minPointClearanceOnPath)
        logger.info(s"Min Point clearance violation. $a, $b")
      a
    }

  lazy val withMaxVfByAngle: List[List[List[Float]]] = {
    val foo = pcr.pathList
    logger.debug(s"foo: $foo")
    foo.map((singlePath: List[List[Float]]) => {
      if (singlePath.length > 2) {
        val singlePathCoords = singlePath.map(asCoordinate)
        val constrPath = singlePathCoords.
          zip(singlePathCoords.tail).
          zip(singlePathCoords.tail.tail).
          map { case ((a, b), c) => (a, b, c) }
        val headEntry: List[Float] = List(
          singlePathCoords.head.x,
          singlePathCoords.head.y,
          singlePathCoords.head.getZ,
          0.0d).map(_.toFloat)
        val listResults: List[List[Float]] = constrPath.map {
          case (c1, c2, c3) => List(c2.x.toFloat, c2.y.toFloat, c2.getZ.toFloat, vfMaxSegment(c1, c2, c3))
        }
        val lastEntry: List[Float] = List(
          singlePathCoords.last.x,
          singlePathCoords.last.y,
          singlePathCoords.last.getZ).map(_.toFloat) :+ 0.0f
        headEntry +: listResults :+ lastEntry
      }
      else {
        singlePath.map(coords => coords :+ 0.0f)
      }
    })
  }

  //Only for tools
  lazy val maxVfByToolAndAngle = withMaxVfByAngle.zip(toolList).flatMap {
    case (singlePath: List[List[Float]], toolOption: Option[CncTool]) =>
      toolOption.map (tool => (singlePath.map { l =>
        logger.debug(s"last: ${l.last} tool.vf: ${tool.vf}")
        l.dropRight(1) :+ min(l.last, tool.vf)
      }, tool))
  } //Tool.vf for every coordinate

  lazy val withMaxVfByAcc: List[List[List[Float]]] = {
    val xLookup = config.xAccLookup
    val yLookup = config.yAccLookup

    def buildAccSegments(currentCoord: List[Float],
                         nextCoord: List[Float], tool: CncTool): List[(Float, Float, Float)] = {
      val vf_start = currentCoord.last
      val vf_end = nextCoord.last
      val distanceBetweenPoints = asCoordinate(currentCoord).distance(asCoordinate(nextCoord))

      if ((vf_start == vf_end && vf_start == tool.vf) || distanceBetweenPoints < 0.1d)
        return List.empty[(Float, Float, Float)]

      val angle = Angle.angle(asCoordinate(currentCoord), asCoordinate(nextCoord))

      def getAccSequence(lookupTable: List[(Float, Float, Float)], distanceForDimension: Float) = {
        val splitIndex = lookupTable.lastIndexWhere { case (velocity, time, position) => velocity < vf_start }
        val (_, second) = lookupTable.splitAt(splitIndex)
        // Element at lastIndex
        val (_, f_t, f_s) = second.head
        // Remove offset, drop lastIndex element
        val valuesWithoutOffset = second.map { case (v, t, s) => (v, t - f_t, s - f_s) }.drop(1)
        val accEntries = valuesWithoutOffset.takeWhile { i => i._2 < distanceForDimension.toFloat }
        val headEntry = accEntries.headOption.map { case (v, t, s) => (min(v, tool.vf), t, s) }
        val tailAccList = accEntries.drop(1).takeWhile { case (v, t, s) => v < tool.vf }
        val accList = headEntry.map {
          _ +: tailAccList
        }.getOrElse(List.empty[(Float, Float, Float)])

        // if first entry v > tool.vf: use tool.vf to reach max admissible vf // or append vf at end of acclist
        accList
      }

      val newAccSeqX = getAccSequence(xLookup, Math.abs(currentCoord.head - nextCoord.head))
      val newAccSeqY = getAccSequence(yLookup, Math.abs(currentCoord(1) - nextCoord(1)))

      // Find limiting dimension by projection of last Element
      val projXFactor = FastMath.abs(FastMath.cos(angle).toFloat)
      val projYFactor = FastMath.abs(FastMath.sin(angle).toFloat)

      val projX = projXFactor * newAccSeqX.lastOption.map {
        _._3
      }.getOrElse(0.0f)
      val projY = projYFactor * newAccSeqY.lastOption.map {
        _._3
      }.getOrElse(0.0f)
      val xIsLimitingDimension = projX > projY

      if (xIsLimitingDimension)
        newAccSeqX.map { case (f_v, f_t, f_s) => (f_v * projXFactor, f_t, f_s * projXFactor) } // project Acc to Vector
      else
        newAccSeqY.map { case (f_v, f_t, f_s) => (f_v * projYFactor, f_t, f_s * projYFactor) } // project Acc to Vector
    }

    def getAccListsForStrategy(pathList: List[(List[List[Float]],
      CncTool)]): List[List[(List[Float], List[(Float, Float, Float)])]] = {
      @scala.annotation.tailrec
      def traverseList(pathList: List[List[Float]],
                       accList: List[(List[Float], List[(Float, Float, Float)])],
                       tool: CncTool): List[(List[Float], List[(Float, Float, Float)])] = {
        if (pathList.size == 1 || pathList.isEmpty) {
          pathList.headOption match {
            case Some(a) => accList :+ (a, List.empty[(Float, Float, Float)])
            case None => accList
          }
        } else {
          logger.debug(s"Building Acc Segments for ${pathList.head} to ${pathList(1)}")
          val accelerationSequence = buildAccSegments(pathList.head, pathList(1), tool)
          logger.debug(s"Got Acc Segments for ${pathList.head} to ${pathList(1)}: $accelerationSequence")

          val newAccList: List[(List[Float], List[(Float, Float, Float)])] =
            accList :+ (pathList.head, accelerationSequence)
          val updatedElement: List[Float] = pathList(1).dropRight(1) :+
            min(pathList(1).last, accelerationSequence.lastOption.map(_._1).getOrElse(Float.PositiveInfinity))
          val newList: List[List[Float]] =
            updatedElement +: pathList.drop(2) // remove first element from List, use updated second element
          traverseList(newList, newAccList, tool)
        }
      }

      val enrichedMachiningStrategy: List[List[(List[Float], List[(Float, Float, Float)])]] = pathList.map {
        case (singlePath: List[List[Float]], tool: CncTool) =>
          if (singlePath.size > 2) {
            val accLists = traverseList(singlePath, List.empty[(List[Float], List[(Float, Float, Float)])], tool)
            accLists
          } else List.empty[(List[Float], List[(Float, Float, Float)])]
      }
      enrichedMachiningStrategy
    }


    def resolveAccLists(p1: List[Float], p2: List[Float],
                        acc: List[(Float, Float, Float)],
                        dec: List[(Float, Float, Float)]): List[List[Float]] = {
      val distanceBetweenPoints = asCoordinate(p1).distance(asCoordinate(p2)).toFloat
      if (distanceBetweenPoints >
        (acc.lastOption.getOrElse((0.0f, 0.0f, 0.0f))._3 + dec.lastOption.getOrElse((0.0f, 0.0f, 0.0f))._3)) {
        // distance between points allows for both Acc and Dec
        val accPoints = acc.map { case (v, t, s) => pointAlongByDistance(p1, p2, s) :+ v }
        val decPoints = dec.map { case (v, t, s) => pointAlongByDistance(p2, p1, s) :+ v }.reverse

        p1 +: (accPoints ++ decPoints) :+ p2
      }
      else {
        val decUpdatedS = dec.map { case (v, t, s) => (v, t, distanceBetweenPoints - s) }.reverse
        val accPoints = acc.takeWhile { case (v, t, s) => decUpdatedS.find { case (vd, td, sd) => vd < v && sd < s } match {
          case Some(_) => false
          case None => true
        }
        }

        val decPoints = decUpdatedS.dropWhile {
          case (v, t, s) =>
            s < accPoints.lastOption.map {
              _._3
            }.getOrElse(0.0f)
        }

        p1 +: (accPoints ++ decPoints).map { case (v, t, s) => pointAlongByDistance(p1, p2, s) :+ v } :+ p2
      }
    }

    lazy val forwardAccs = getAccListsForStrategy(maxVfByToolAndAngle)
    lazy val updatedPathList = forwardAccs.map(i => i.map {
      _._1
    }).zip(toolList.flatten).map { case (a, b) => (a, b) }

    lazy val updatedReversedPathList = updatedPathList.map(singlePath => (singlePath._1.reverse, singlePath._2))
    lazy val withDecelleration: List[List[(List[Float], List[(Float, Float, Float)])]] =
      getAccListsForStrategy(updatedReversedPathList).map {
        _.reverse
      }.map(_.drop(1)) //for every path: skip first element which does not contain a deceleration tuple.
    // This way, pairs can be formed via zip

    // Liste von KnotenPaaren mit jeweils 2 Accfunctions
    val coordPairsWithAccelDecel = (forwardAccs.map { nodesWithAcc => nodesWithAcc.map {
      _._1
    }
    } zip
      withDecelleration.map(nodesWithDecel => nodesWithDecel.map {
        _._1
      })) zip
      forwardAccs.map { nodesWithAcc => nodesWithAcc.map {
        _._2
      }
      } zip
      withDecelleration.map { nodesWithDecel => nodesWithDecel.map {
        _._2
      }
      }

    logger.debug(s"$coordPairsWithAccelDecel")
    coordPairsWithAccelDecel.map {
      case ((pairList, accList), decList) =>
        ((pairList._1 zip pairList._2) zip accList zip decList).map {
          case ((pair, acc), dec) => resolveAccLists(pair._1, pair._2, acc, dec)
        }.reduceLeftOption(_ ++ _.tail).getOrElse {
          List.empty[List[Float]]
        }
    }
  }

  lazy val withMaxVfByAcc2: List[List[List[Float]]] = {
    def buildList(lastCoord: List[Float], nextCoord: List[Float]): List[Float] = {
      @tailrec
      def calcVend(angle: Double, targetDistance: Double, accumulatedDistance: Double,
                   currentVelocity: Double): Double = {
        if (accumulatedDistance < targetDistance) {
          // logger.debug(s"currentVelocity: $currentVelocity, accumulatedDistance: $accumulatedDistance")
          val newVelocity = config.newVf(angle, currentVelocity)
          val newDistance = accumulatedDistance + newVelocity * 1000 / 60 * config.deltaT
          //logger.debug(s"currentVelocity: $currentVelocity, accumulatedDistance: $accumulatedDistance, newVelocity: $newVelocity,newDistance:$newDistance")
          calcVend(angle, targetDistance, newDistance, newVelocity)
        }
        else {
          //  logger.info(s"targetVelocity: $currentVelocity")
          currentVelocity
        }
      }

      val startV = lastCoord.last
      val angle = Angle.angle(asCoordinate(lastCoord), asCoordinate(nextCoord))
      val distanceBetweenPoits = asCoordinate(lastCoord).distance(asCoordinate(nextCoord))
      //logger.info(s"startPoint: $lastCoord, nextCooord: $nextCoord, distance: $distanceBetweenPoits")
      //logger.info(s"startV $startV")
      //logger.info(s"comparing velocities for $nextCoord vs ${calcVend(angle, distanceBetweenPoits, 0.0d, startV).toFloat}")

      lazy val nextCoordWithVelocity: List[Float] =
        nextCoord.dropRight(1) :+
          min(nextCoord.last, calcVend(angle, distanceBetweenPoits, 0.0d, startV).toFloat)
      nextCoordWithVelocity
    }


    maxVfByToolAndAngle.map {
      case (singlePath: List[List[Float]], tool: CncTool) =>
        if (singlePath.size > 2) {
          singlePath.tail.foldLeft(List(singlePath.head.take(3) :+ 0.0f)) { case (a, b) => a :+ buildList(a.last, b) }
        } else List.empty[List[Float]]
    }
  }

  def getKlartextLineString(l: List[List[Float]], withFMAX: Boolean = false): List[String] =
    l.filter(_.nonEmpty).map(singleCoord => {
      val zCoordString = if (!singleCoord(2).isNaN) s" Z${singleCoord(2)}" else ""
      val fString = s"${if (withFMAX) "MAX" else if (singleCoord.last <= 50.0f) "50.0" else singleCoord.last}"
      s"""L X${singleCoord.head} Y${singleCoord(1)}$zCoordString F$fString""".stripMargin
    })


    //  lazy val completePathTime = completeTime(withMaxVfByAcc)
    //  lazy val pathTimesCalculated = {
    //    logger.info(s"withMaxVfByAcc $withMaxVfByAcc")
    //    calcTime(withMaxVfByAcc)
    //  }

  lazy val klartextFileContents: List[String] = {
    logger.debug(s"maxVfByToolAndAngle.size ${maxVfByToolAndAngle.size}")
    logger.debug(s"toolList.size ${toolList.size}")
    logger.debug(s"toolList ${toolList}")
    logger.debug(s"toolList.flatten.size ${toolList.flatten.size}")
    logger.debug(s"withMaxVfByAcc.size ${withMaxVfByAcc.size}")
    withMaxVfByAcc.zip(toolList.flatten).
    zipWithIndex.filter(_._1._1.nonEmpty).map {
    case ((currentPath, currentTool: CncTool), index) =>
      val linearDive = if (currentPath.head(2).isNaN || currentPath.head(2) == 0.0)
        s"${getKlartextLineString(List(currentPath.head.dropRight(2) :+ 0.0f :+ 0.500f)).mkString("\r\n")}"
      else ""
      s"""BEGIN PGM Single MM
         |TOOL CALL ${currentTool.idString} ; ${currentTool.description}
         |${
        getKlartextLineString(List(currentPath.head.dropRight(2) :+ 3.0f :+ currentTool.vf), withFMAX = true).
          mkString("\r\n")
      }
         |$linearDive
         |${getKlartextLineString(currentPath.tail).mkString("\r\n")}
         |${getKlartextLineString(List(currentPath.last.dropRight(2) :+ 3.0f), withFMAX = true).mkString("\r\n")}
         |END PGM Single MM
         |""".stripMargin
    // logger.info(s"Teilpfad: \r\n$outStr")
  }}

  def writeKlartextFiles(path: String = "."): Unit = {
    klartextFileContents.zipWithIndex.foreach {
      case (s, i) =>
        import java.io._
        val pw = new PrintWriter(new File(new File(path), s"${i + 1}.p"))
        pw.write(s"$s")
        pw.close()
    }
    //    logger.info(s"pathTimes $pathTimesCalculated")
    //    logger.info(s"complete Time: $completePathTime")
  }

  def buildZip(path: String, outFile: String): Unit = {
    import java.io.{BufferedInputStream, FileInputStream, FileOutputStream}
    import java.util.zip.{ZipEntry, ZipOutputStream}

    val zip = new ZipOutputStream(new FileOutputStream(outFile))
    val files = {
      val d = new File(path)
      if (d.exists && d.isDirectory) {
        d.listFiles.filter(_.isFile).filter(_.getName.endsWith(".p")).toList
      } else {
        List[File]()
      }
    }


    files.foreach { f =>
      zip.putNextEntry(new ZipEntry(f.getName))
      val in = new BufferedInputStream(new FileInputStream(f))
      var b = in.read()
      while (b > -1) {
        zip.write(b)
        b = in.read()
      }
      in.close()
      zip.closeEntry()
    }

    zip.close()
  }
}

object PathFeedUtils {
  def apply(p_pcr: PathCoverageResult) = new PathFeedUtils {
    override val pcr: PathCoverageResult = p_pcr
  }
}

/**
 * GenericCompositionPcStep(*,GenericCompositionPcStep(*,*))
 * GenericCompositionPcStep(GenericCompositionPcStep(SteelRadial,SteelRoughing),SteelRadial(*))
 */
