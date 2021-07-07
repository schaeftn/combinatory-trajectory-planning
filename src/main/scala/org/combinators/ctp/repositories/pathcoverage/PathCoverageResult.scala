package org.combinators.ctp.repositories.pathcoverage

import java.io.File

import org.combinators.ctp.repositories.cncPathFct
import org.combinators.ctp.repositories.toplevel.{Cnc2DModel, PathCoverageStep, PathCoverageStepConfig}
import org.locationtech.jts.algorithm.Angle
import org.locationtech.jts.geom.Coordinate

import scala.math.{max, min}


import scala.annotation.tailrec
import scala.math.min
import scala.xml.Elem

/**
 * @param s      scene applied to pc steps
 * @param config Config object that will be applied to all path coverage steps
 * @param l      unfolded pc step list
 */
case class PathCoverageResult(s: Cnc2DModel, config: PathCoverageStepConfig, l: List[PathCoverageStep])
  extends JtsUtils with PathRefinement {
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
  lazy val computeModelHistory: (List[Cnc2DModel], List[List[List[Float]]], List[CncTool], Boolean) = {
    lazy val (modelList, pathList, compToolList, runSuccessful) =
      unfoldedPcFunctionList.foldLeft(List(s), List.empty[List[List[Float]]], List.empty[CncTool], true: Boolean)({
        case ((modelList: List[Cnc2DModel],
        pathList: List[List[List[Float]]],
        toolList: List[CncTool],
        continueRun: Boolean),
        (currentFct, currentFoldTool)) =>
          logger.debug("starting fold step")
          if (continueRun) {
            logger.debug("continueRun: currentFct")
            val (pathResult, modelResult) = currentFct(modelList.last, config)
            logger.debug("continueRun: After fct")
            val newModelList: List[Cnc2DModel] = modelList :+ modelResult
            val newPathList: List[List[List[Float]]] = pathList ++ pathResult
            val newToolList: List[CncTool] = toolList ++ List.fill(pathResult.size)(currentFoldTool)
            val newContVal: Boolean = pathResult.nonEmpty

            val res = (newModelList, newPathList, newToolList, newContVal)
            logger.debug(s"res: $res")
            res
          } else {
            logger.debug("dontContinueRun: returning")
            (modelList, pathList, toolList, continueRun)
          }
      })

    logger.debug("Starting path refinement.")
    val refinedPathList = if (config.pathRefinement)
      pathList.map(sPath => refine(sPath, config.minPointClearanceOnPath)).
        map(sPath => refineMinClearance(sPath, config.maxPointClearanceOnPath))
    else
      pathList

    logger.debug("Path refinement done.")

    logger.debug(s"ModelList: ${modelList}")
    (modelList, refinedPathList, compToolList, runSuccessful)
  }

  lazy val (modelList, pathList, toolList, runSuccessful) = computeModelHistory
  lazy val endScene = modelList.last

  def writeXmlOut(fn: String) = {
    import java.io._
    val pw = new PrintWriter(new File(fn))
    pw.write(getXmlString(fn).mkString(s"\r\n"))
    pw.close()
  }

  def getXmlString(description: String): Elem = {
    val info = modelList.map { m => m.getMachinedMultiGeo } zipAll(
      pathList.map { p => asLineString(p) }, emptyGeometry, emptyGeometry)

    <run>
      <precisionModel type="FLOATING"/>{info.map { case (a, b) => <case>
      <desc>{description}</desc> <a>
        {a}
      </a> <b>
        {b}
      </b>
    </case>
    }}
    </run>
  }

  def writeXmlFile(): Unit = {
    import java.io._
    val pw = new PrintWriter(new File(s"out.xml"))
    pw.write(s"$s")
    pw.close()
  }
}
