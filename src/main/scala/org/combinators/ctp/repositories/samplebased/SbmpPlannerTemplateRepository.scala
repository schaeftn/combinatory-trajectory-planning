package org.combinators.ctp.repositories.samplebased

import org.combinators.cls.interpreter.combinator
import org.combinators.cls.types.Constructor
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.python_interop.{PlannerScheme, PythonTemplateUtils, PythonWrapper, SubstitutionScheme}
import org.combinators.ctp.repositories.scene.SceneUtils
import org.combinators.ctp.repositories.toplevel._
import io.circe.parser.decode
import io.circe.generic.auto._
import io.circe.syntax._

trait SbmpPlannerTemplateRepository1 extends SbmpPlannerTemplateRepository[ProblemDefinitionFiles]{}


trait SbmpPlannerTemplateRepository[S] extends SceneUtils with PythonTemplateUtils {
  trait CombinatorPlannerTemplate[A, B] {
    def apply = new PlannerScheme[A, B](st, pf, startFile)

    val pf: (A, String) => B
    val st: SubstitutionScheme
    val startFile: String
  }

  @combinator object PrmPlannerTemplate
    extends CombinatorPlannerTemplate[S, List[List[Float]]] {
    override val pf: (S, String) => List[List[Float]] =
      (_: S, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("PRM")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_PRM
  }

  @combinator object RrtPlannerTemplate
    extends CombinatorPlannerTemplate[S, List[List[Float]]] {
    override val pf: (S, String) => List[List[Float]] =
      (_: S, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("RRT")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_RRT
  }

  @combinator object PrmStarPlannerTemplate
    extends CombinatorPlannerTemplate[S, List[List[Float]]] {
    override val pf: (S, String) => List[List[Float]] =
      (_: S, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("PRMstar")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_PRMStar
  }

  @combinator object LazyPrmPlannerTemplate
    extends CombinatorPlannerTemplate[S, List[List[Float]]] {
    override val pf: (S, String) => List[List[Float]] =
      (_: S, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("LazyPRM")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_LazyPRM
  }

  @combinator object LazyPrmStarPlannerTemplate
    extends CombinatorPlannerTemplate[S, List[List[Float]]] {
    override val pf: (S, String) => List[List[Float]] =
      (_: S, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("LazyPRMstar")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_LazyPRMStar
  }

  @combinator object SSTPlannerTemplate
    extends CombinatorPlannerTemplate[S, List[List[Float]]] {
    override val pf: (S, String) => List[List[Float]] =
      (_: S, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("SST")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_SST
  }

  @combinator object RRTConnectPlannerTemplate
    extends CombinatorPlannerTemplate[S, List[List[Float]]] {
    override val pf: (S, String) => List[List[Float]] =
      (_: S, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList

    override val st: SubstitutionScheme = defaultPlannerSubstScheme("RRTConnect")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_RRTConnect
  }

  @combinator object RRTStarPlannerTemplate
    extends CombinatorPlannerTemplate[S, List[List[Float]]] {
    override val pf: (S, String) => List[List[Float]] =
      (_: S, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("RRTstar")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_RRTStar
  }

  @combinator object LBTRRTPlannerTemplate
    extends CombinatorPlannerTemplate[S, List[List[Float]]] {
    override val pf: (S, String) => List[List[Float]] =
      (_: S, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("LBTRRT")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_LBTRRT
  }

  @combinator object TRRTPlannerTemplate
    extends CombinatorPlannerTemplate[S, List[List[Float]]] {
    override val pf: (S, String) => List[List[Float]] =
      (_: S, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("TRRT")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_TRRT
  }

  @combinator object LazyRRTPlannerTemplate
    extends CombinatorPlannerTemplate[S, List[List[Float]]] {
    override val pf: (S, String) => List[List[Float]] =
      (_: S, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("LazyRRT")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_LazyRRT
  }

  @combinator object ESTPlannerTemplate
    extends CombinatorPlannerTemplate[S, List[List[Float]]] {
    override val pf: (S, String) => List[List[Float]] =
      (_: S, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("EST")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_EST
  }

  @combinator object SBLPlannerTemplate
    extends CombinatorPlannerTemplate[S, List[List[Float]]] {
    override val pf: (S, String) => List[List[Float]] =
      (_: S, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("SBL")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_SBL
  }

  @combinator object KPIECE1PlannerTemplate
    extends CombinatorPlannerTemplate[S, List[List[Float]]] {
    override val pf: (S, String) => List[List[Float]] =
      (_: S, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("KPIECE1")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_KPIECE1
  }


  @combinator object BKPIECE1PlannerTemplate
    extends CombinatorPlannerTemplate[S, List[List[Float]]] {
    override val pf: (S, String) => List[List[Float]] =
      (_: S, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("BKPIECE1")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_BKPIECE1
  }

  @combinator object LBKPIECE1PlannerTemplate
    extends CombinatorPlannerTemplate[S, List[List[Float]]] {
    override val pf: (S, String) => List[List[Float]] =
      (_: S, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("LBKPIECE1")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_LBKPIECE1
  }


  @combinator object STRIDEPlannerTemplate
    extends CombinatorPlannerTemplate[S, List[List[Float]]] {
    override val pf: (S, String) => List[List[Float]] =
      (_: S, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("STRIDE")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_STRIDE
  }

  @combinator object PDSTPlannerTemplate
    extends CombinatorPlannerTemplate[S, List[List[Float]]] {
    override val pf: (S, String) => List[List[Float]] =
      (_: S, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("PDST")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_PDST
  }

  @combinator object FMTPlannerTemplate
    extends CombinatorPlannerTemplate[S, List[List[Float]]] {
    override val pf: (S, String) => List[List[Float]] =
      (_: S, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("FMT")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_FMT
  }

  @combinator object BFMTPlannerTemplate
    extends CombinatorPlannerTemplate[S, List[List[Float]]] {
    override val pf: (S, String) => List[List[Float]] =
      (_: S, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("BFMT")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_BFMT
  }

  @combinator object RRTsharpPlannerTemplate
    extends CombinatorPlannerTemplate[S, List[List[Float]]] {
    override val pf: (S, String) => List[List[Float]] =
      (_: S, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("RRTsharp")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_RRTsharp
  }


  @combinator object RRTXstaticPlannerTemplate
    extends CombinatorPlannerTemplate[S, List[List[Float]]] {
    override val pf: (S, String) => List[List[Float]] =
      (_: S, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("RRTXstatic")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_RRTXstatic
  }

  @combinator object InformedRRTstarPlannerTemplate
    extends CombinatorPlannerTemplate[S, List[List[Float]]] {
    override val pf: (S, String) => List[List[Float]] =
      (_: S, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("InformedRRTstar")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_InformedRRTstar
  }

  @combinator object BITstarPlannerTemplate
    extends CombinatorPlannerTemplate[S, List[List[Float]]] {
    override val pf: (S, String) => List[List[Float]] =
      (_: S, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("BITstar")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_BITstar
  }

  def defaultPlannerSubstScheme(s: String): SubstitutionScheme =
    SubstitutionScheme(
      Map(sbmpStartTemplate -> sbmpMainStartFile),
      Map("$plannerMainPlannerInst$" -> defaultPlannerString(s)))

  def parseDefaultOmplPath(str: String): Seq[List[Float]] = {
    val index = str.indexOf("solution path:")
    if (index > -1)
      str.substring(index).split("\r\n").
        withFilter((s: String) => s.startsWith("RealVectorState")).
        map(s => s.substring(s.indexOf("[") + 1, s.indexOf("]")).split(" ").map(_.toFloat).toList).toList
    else Seq.empty
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

  @combinator object PrmPlannerTemplatePathSmoothing
    extends CombinatorPlannerTemplate[(ProblemDefinitionFiles, List[List[Float]]), List[List[Float]]] {
    override val pf: ((ProblemDefinitionFiles, List[List[Float]]), String) => List[List[Float]] =
      (_: (ProblemDefinitionFiles, List[List[Float]]), plannerOut: String) =>
        parseDefaultOmplPath(plannerOut).toList

      override val st: SubstitutionScheme = defaultPlannerSubstScheme("PRM")
      override val startFile: String = sbmpMainStartFile
      val semanticType = sbmp_planner_PRM
  }

  @combinator object KPIECE1PlannerTemplatePathSmoothing
      extends CombinatorPlannerTemplate[(ProblemDefinitionFiles,List[List[Float]]), List[List[Float]]] {
    override val pf: ((ProblemDefinitionFiles,List[List[Float]]), String) => List[List[Float]] =
      (_: (ProblemDefinitionFiles,List[List[Float]]), plannerOut: String) =>
        parseDefaultOmplPath(plannerOut).toList

      override val st: SubstitutionScheme = SubstitutionScheme(
        Map(sbmpStartTemplate->sbmpMainStartFile),
        Map("$plannerMainPlannerInst$" -> "p = og.KPIECE1(self.si)\n        p.setBorderFraction(0.9)\n        p.setGoalBias(0.05)\n        p.setRange(0)\n        self.ss.setPlanner(p)"))
      override val startFile: String = sbmpMainStartFile
      val semanticType = sbmp_planner_KPIECE1
  }

  @combinator object KPIECE1PlannerTemplatePathSmoothingWithStates
      extends CombinatorPlannerTemplate[(ProblemDefinitionFiles,List[List[Float]]), (List[List[Float]],List[List[Float]])] {
    override val pf: ((ProblemDefinitionFiles,List[List[Float]]), String) => (List[List[Float]],List[List[Float]]) =
      (_: (ProblemDefinitionFiles,List[List[Float]]), plannerOut: String) =>
       (parseDefaultOmplPath(plannerOut).toList, parseOmplExploredStates(plannerOut))

      override val st: SubstitutionScheme = SubstitutionScheme(
        Map(sbmpStartTemplate->sbmpMainStartFile),
        Map("$plannerMainPlannerInst$" -> "p = og.KPIECE1(self.si)\n        p.setBorderFraction(0.9)\n        p.setGoalBias(0.05)\n        p.setRange(0)\n        self.ss.setPlanner(p)"))
      override val startFile: String = sbmpMainStartFile
      val semanticType = sbmp_planner_KPIECE1
  }


  def defaultPlannerString(s: String): String =
    s"p = og.$s(self.si)\n        self.ss.setPlanner(p)"

}