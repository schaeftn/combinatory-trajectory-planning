package org.combinators.ctp.repositories.pathcoverage

import org.combinators.ctp.repositories.toplevel.{Cnc2DModel, PathCoverageStep, PathCoverageStepConfig}
import org.locationtech.jts.geom.Geometry

import scala.util.{Failure, Success, Try}
import scala.xml.Elem

/**
 * @param s      scene applied to pc steps
 * @param config Config object that will be applied to all path coverage steps
 * @param pcs    Top Level Path Coverage Step
 */
case class PathCoverageResult(s: Cnc2DModel, config: PathCoverageStepConfig, pcs: PathCoverageStep)
  extends JtsUtils with PathRefinement {
    def traverseTreePostOrder(pcs: PathCoverageStep,
                            accResultList:List[(Cnc2DModel, List[List[Float]], Option[CncTool],String)],
                            continueRun: Boolean):
  (List[(Cnc2DModel, List[List[Float]], Option[CncTool], String)], Boolean) = {
      // Run self
      logger.info(s"evaluating pcs: ${pcs.description_param}")

      logger.debug(s"accResultList.empty ${accResultList.isEmpty}")
      val thisResult: Option[(List[List[List[Float]]], Cnc2DModel)] =
        pcs.pcFct.map(f =>
          Try {
            f.apply(accResultList.lastOption.map(_._1).getOrElse(s), config)
          }
          match {
            case Success(v) => v
            case Failure(exception) =>
              logger.error(s"pcs: ${pcs.description_param}: Failure!")
              exception.printStackTrace()
              (List.empty[List[List[Float]]], accResultList.lastOption.map(_._1).getOrElse(s))
          })

      val thisResultAsListOption = thisResult.map(l => List(
        (l._2, l._1.reduceOption(_ ++ _).getOrElse(List.empty[List[Float]]), pcs.tool, pcs.description_param)))
      logger.debug(s"thisResultAsListOption $thisResultAsListOption")
      val lastCurrentResultEntry = thisResultAsListOption.getOrElse(accResultList.lastOption.map(List(_)).
        getOrElse(List((s, List.empty[List[Float]], None,pcs.description_param))))

      //Result was computed, but returned empty List
      val continueAfterCurrent = true // continueRun && thisResult.forall(_._1.isEmpty) //TODO Check combinator compositions

      // Run children for model
      val (retransformedChildrenPaths,continueRunAfterChildren) = {
        if (pcs.pcrList.nonEmpty) {
          val (lastAccResultWithChildren, lastAccChildrenContinue) =
            pcs.pcrList.foldLeft[(List[(Cnc2DModel, List[List[Float]], Option[CncTool], String)], Boolean)](
              (lastCurrentResultEntry, continueRun)) {
              case ((foldList, foldContinueRun), foldPcs) =>
                val (newResultList, newContinue) = traverseTreePostOrder(foldPcs, foldList, foldContinueRun)
                (newResultList, newContinue)
            }

          //TODO transform paths during current node evaluation
          val retransformedChildrenPathOption = pcs.resultTransform.map(f => f.apply(
            lastAccResultWithChildren.drop(accResultList.size).map(_._2)))

          val rcp = retransformedChildrenPathOption match {
            case None => lastAccResultWithChildren.drop(1)
            case Some(transformedPathList) => lastAccResultWithChildren.drop(1).zip(transformedPathList).map {
              case ((model, _, toolOpt, description), newPathList) => (model, newPathList, toolOpt, description)
            }
          }

          (rcp,lastAccChildrenContinue)
        } else {
          (List.empty[(Cnc2DModel, List[List[Float]], Option[CncTool], String)], true)
        }
      }

      logger.debug(s"pcs: ${pcs.description_param}, accResultList.size ${accResultList.size}")
      logger.debug(s"pcs: ${pcs.description_param}, thisResultAsListOption $thisResultAsListOption")
      thisResultAsListOption match {
        case Some(r) => r.foreach(t => {
          pGeo(s"pcs: ${pcs.description_param} machinedGeo", t._1.getMachinedMultiGeo)
          pGeo("path", asLineString(t._2))
        })
        case None => ()
      }
      logger.debug(s"pcs: ${pcs.description_param}, retransformedChildrenPaths ${retransformedChildrenPaths.size}")

      // Build result
      val rList = if (continueAfterCurrent) {
        thisResultAsListOption match {
          case None =>
            accResultList ++ retransformedChildrenPaths
          case Some(l) =>
            accResultList ++ l ++ retransformedChildrenPaths
        }
      } else {
        logger.warn(s"pcs: ${pcs.description_param}: Dropping children results")
        thisResultAsListOption.map(resultList => accResultList ++ resultList).getOrElse(accResultList)
      }

      logger.debug(s"pcs: ${pcs.description_param}, rList.size: ${rList.size}")
      (rList, continueRunAfterChildren)
      }

  /**
   * quatruple: EndModel, Aux List Path Functions (empty when done),
   * model history (first element is initial model),
   * path history (first element is empty path)
   */
  lazy val computeModelHistory: (List[(Cnc2DModel, List[List[Float]], Option[CncTool], String)], Boolean) = {
    logger.info(s"traverseTreePostOrder: ${pcs.description_param}")
    //traverseTreePostOrder(pcs, List.empty[(Cnc2DModel, List[List[Float]], Option[CncTool], String)], continueRun = true)
    traverseTreePostOrder(pcs, List((s, List.empty[List[Float]], None, """Initial model state.""")), continueRun = true)
  }

  lazy val modelList = computeModelHistory._1.map{case (cModel, _, _,_) => cModel}
  lazy val pathList = computeModelHistory._1.map{case (_, cPath, _,_) => cPath}
  lazy val toolList = computeModelHistory._1.map{case (_, _, cToolOption,_) => cToolOption}
  lazy val descrList = computeModelHistory._1.map{case (_, _, _,descr) => descr}
  lazy val runSuccessful = computeModelHistory._2

  lazy val endScene = modelList.last

  def writeXmlOut(fn: String, inhabitantIndex: Int) = {
    import java.io._
    val pw = new PrintWriter(new File(fn))
    pw.write(getXmlString(inhabitantIndex).mkString(s"\r\n"))
    pw.close()
  }

  def writeTreeOut(i: Int, fn: String) = {
    import java.io._
    val pw = new PrintWriter(new File(fn))
    pw.write(getXmlString(i).mkString(s"\r\n"))
    pw.close()
  }

  def getXmlString(inhabitantIndex: Int): Elem = {
    val modelTuple = modelList.map { m => (m, m.getMachinedMultiGeo) }
    val modelTupleWithPath: List[((Cnc2DModel, Geometry), Geometry)] = modelTuple.zip(
      pathList.map { p => asLineString(p) })

    <run>
      <precisionModel type="FLOATING"/>{modelTupleWithPath.zipWithIndex.map { case (((m, machined), path), index ) =>
      <case>
        <desc>
          {"inhabitant_" + "%03d".format(inhabitantIndex) + ", step " + descrList(index)}
        </desc> <a>
        {m.targetGeometry}
      </a> <b>
        {getGeoCollection(List(machined,path))}
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
