package org.combinators.ctp.repositories.samplebased

import org.combinators.cls.interpreter.combinator
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.python_interop.{PythonTemplateUtils, SubstitutionScheme}
import org.combinators.ctp.repositories.scene.SceneUtils
import org.combinators.ctp.repositories.toplevel._
import io.circe.parser.decode
import io.circe.generic.auto._
import io.circe.syntax._

trait SbmpPlannerTemplateRepositoryWithStates extends SceneUtils with PythonTemplateUtils with SbmpPlannerTemplateRepository {
  trait CombinatorPlannerTemplatePathWithStates extends CombinatorPlannerTemplate[(List[List[Float]], List[List[Float]])] {
    override val pf = {
      plannerOut: String => (parseOmplExploredStates(plannerOut).toList, parseDefaultOmplPath(plannerOut).toList)
    }
  }

  @combinator object PrmPlannerTemplateWithStates extends CombinatorPlannerTemplatePathWithStates {
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("PRM")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_PRM
  }

  @combinator object RrtPlannerTemplateWithStates extends CombinatorPlannerTemplatePathWithStates {
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("RRT")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_RRT
  }

  @combinator object PrmStarPlannerTemplateWithStates extends CombinatorPlannerTemplatePathWithStates {
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("PRMstar")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_PRMStar
  }


  @combinator object LazyPrmPlannerTemplateWithStates extends CombinatorPlannerTemplatePathWithStates {
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("LazyPRM")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_LazyPRM
  }

  @combinator object LazyPrmStarPlannerTemplateWithStates extends CombinatorPlannerTemplatePathWithStates {
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("LazyPRMstar")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_LazyPRMStar
  }

  @combinator object SSTPlannerTemplateWithStates extends CombinatorPlannerTemplatePathWithStates {
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("SST")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_SST
  }

  @combinator object RRTConnectPlannerTemplateWithStates extends CombinatorPlannerTemplatePathWithStates {
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("RRTConnect")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_RRTConnect
  }

  @combinator object RRTStarPlannerTemplateWithStates extends CombinatorPlannerTemplatePathWithStates {
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("RRTstar")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_RRTStar
  }

  @combinator object LBTRRTPlannerTemplateWithStates extends CombinatorPlannerTemplatePathWithStates {
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("LBTRRT")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_LBTRRT
  }

  @combinator object TRRTPlannerTemplateWithStates extends CombinatorPlannerTemplatePathWithStates {
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("TRRT")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_TRRT
  }

  @combinator object LazyRRTPlannerTemplateWithStates extends CombinatorPlannerTemplatePathWithStates {
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("LazyRRT")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_LazyRRT
  }

  @combinator object ESTPlannerTemplateWithStates extends CombinatorPlannerTemplatePathWithStates {
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("EST")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_EST
  }

  @combinator object SBLPlannerTemplateWithStates extends CombinatorPlannerTemplatePathWithStates {
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("SBL")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_SBL
  }

  @combinator object KPIECE1PlannerTemplateWithStates extends CombinatorPlannerTemplatePathWithStates {

    override val st: SubstitutionScheme = defaultPlannerSubstScheme("KPIECE1")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_KPIECE1
  }


  @combinator object BKPIECE1PlannerTemplateWithStates extends CombinatorPlannerTemplatePathWithStates {
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("BKPIECE1")
    override val startFile: String = sbmpMainStartFile
    val semanticType = sbmp_planner_BKPIECE1
  }

  @combinator object LBKPIECE1PlannerTemplateWithStates extends CombinatorPlannerTemplatePathWithStates {
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("LBKPIECE1")
    override val startFile: String = sbmpMainStartFile
    val semanticType = sbmp_planner_LBKPIECE1
  }

  @combinator object STRIDEPlannerTemplateWithStates extends CombinatorPlannerTemplatePathWithStates {
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("STRIDE")
    override val startFile: String = sbmpMainStartFile
    val semanticType = sbmp_planner_STRIDE
  }

  @combinator object PDSTPlannerTemplateWithStates extends CombinatorPlannerTemplatePathWithStates {
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("PDST")
    override val startFile: String = sbmpMainStartFile
    val semanticType = sbmp_planner_PDST
  }

  @combinator object FMTPlannerTemplateWithStates extends CombinatorPlannerTemplatePathWithStates {
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("FMT")
    override val startFile: String = sbmpMainStartFile
    val semanticType = sbmp_planner_FMT
  }

  @combinator object BFMTPlannerTemplateWithStates extends CombinatorPlannerTemplatePathWithStates {
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("BFMT")
    override val startFile: String = sbmpMainStartFile
    val semanticType = sbmp_planner_BFMT
  }

  @combinator object RRTsharpPlannerTemplateWithStates extends CombinatorPlannerTemplatePathWithStates {
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("RRTsharp")
    override val startFile: String = sbmpMainStartFile
    val semanticType = sbmp_planner_RRTsharp
  }

  @combinator object RRTXstaticPlannerTemplateWithStates extends CombinatorPlannerTemplatePathWithStates {
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("RRTXstatic")
    override val startFile: String = sbmpMainStartFile
    val semanticType = sbmp_planner_RRTXstatic
  }

  @combinator object InformedRRTstarPlannerTemplateWithStates extends CombinatorPlannerTemplatePathWithStates {
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("InformedRRTstar")
    override val startFile: String = sbmpMainStartFile
    val semanticType = sbmp_planner_InformedRRTstar
  }

  @combinator object BITstarPlannerTemplateWithStates extends CombinatorPlannerTemplatePathWithStates {
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("BITstar")
    override val startFile: String = sbmpMainStartFile
    val semanticType = sbmp_planner_BITstar
  }


  def parseOmplExploredStates(str: String): List[List[Float]] = {
    val lines = str.split("\r\n")
    val stateString = lines.filter(_.startsWith("""{"exploredStates" """)).head
    decode[ExploredStates](stateString) match {
      case Right(s) => s.exploredStates
      case Left(_) => print("Error while decoding")
        List.empty[List[Float]]
    }
  }
}