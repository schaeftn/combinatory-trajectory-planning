package org.combinators.ctp.repositories.samplebased

import org.combinators.cls.interpreter.combinator
import org.combinators.cls.types.Constructor
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.python_interop.{PlannerScheme, PythonTemplateUtils, PythonWrapper, SubstitutionScheme}
import org.combinators.ctp.repositories.scene.SceneUtils
import org.combinators.ctp.repositories.toplevel._

trait SbmpPlannerTemplateRepository extends SceneUtils with PythonTemplateUtils {
  trait CombinatorPlannerTemplate[A, B] {
    def apply = new PlannerScheme[A, B](st, pf, startFile)

    val pf: (A, String) => B
    val st: SubstitutionScheme
    val startFile: String
  }

  @combinator object PrmPlannerTemplate
    extends CombinatorPlannerTemplate[SceneSRT, List[List[Float]]] {
    override val pf: (SceneSRT, String) => List[List[Float]] =
      (_: SceneSRT, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("self.ss.setPlanner(og.PRM(self.si))")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_PRM
  }

  @combinator object RrtPlannerTemplate
    extends CombinatorPlannerTemplate[SceneSRT, List[List[Float]]] {
    override val pf: (SceneSRT, String) => List[List[Float]] =
      (_: SceneSRT, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("self.ss.setPlanner(og.RRT(self.si))")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_RRT
  }

  @combinator object RrtPlannerTemplateFb
    extends CombinatorPlannerTemplate[ProblemDefinitionFiles, List[List[Float]]] {
    override val pf: (ProblemDefinitionFiles, String) => List[List[Float]] =
      (_: ProblemDefinitionFiles, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("self.ss.setPlanner(og.RRT(self.si))")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_RRT
  }

  @combinator object PrmStarPlannerTemplate
    extends CombinatorPlannerTemplate[SceneSRT, List[List[Float]]] {
    override val pf: (SceneSRT, String) => List[List[Float]] =
      (_: SceneSRT, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("self.ss.setPlanner(og.PRMstar(self.si))")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_PRMStar
  }

  @combinator object LazyPrmPlannerTemplate
    extends CombinatorPlannerTemplate[SceneSRT, List[List[Float]]] {
    override val pf: (SceneSRT, String) => List[List[Float]] =
      (_: SceneSRT, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("self.ss.setPlanner(og.LazyPRM(self.si))")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_LazyPRM
  }

  @combinator object LazyPrmStarPlannerTemplate
    extends CombinatorPlannerTemplate[SceneSRT, List[List[Float]]] {
    override val pf: (SceneSRT, String) => List[List[Float]] =
      (_: SceneSRT, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("self.ss.setPlanner(og.LazyPRMstar(self.si))")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_LazyPRMStar
  }

  @combinator object SSTPlannerTemplate
    extends CombinatorPlannerTemplate[SceneSRT, List[List[Float]]] {
    override val pf: (SceneSRT, String) => List[List[Float]] =
      (_: SceneSRT, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("self.ss.setPlanner(og.SST(self.si))")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_SST
  }

  @combinator object RRTConnectPlannerTemplate
    extends CombinatorPlannerTemplate[SceneSRT, List[List[Float]]] {
    override val pf: (SceneSRT, String) => List[List[Float]] =
      (_: SceneSRT, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList

    override val st: SubstitutionScheme = defaultPlannerSubstScheme("self.ss.setPlanner(og.RRTConnect(self.si))")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_RRTConnect
  }

  @combinator object RRTConnectPlannerTemplate1
    extends CombinatorPlannerTemplate[ProblemDefinitionFiles, List[List[Float]]] {
    override val pf: (ProblemDefinitionFiles, String) => List[List[Float]] =
      (_: ProblemDefinitionFiles, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList

    override val st: SubstitutionScheme = defaultPlannerSubstScheme("self.ss.setPlanner(og.RRTConnect(self.si))")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_RRTConnect
  }

  @combinator object RRTStarPlannerTemplate
    extends CombinatorPlannerTemplate[SceneSRT, List[List[Float]]] {
    override val pf: (SceneSRT, String) => List[List[Float]] =
      (_: SceneSRT, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("self.ss.setPlanner(og.RRTstar(si))")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_RRTStar
  }

  @combinator object LBTRRTPlannerTemplate
    extends CombinatorPlannerTemplate[SceneSRT, List[List[Float]]] {
    override val pf: (SceneSRT, String) => List[List[Float]] =
      (_: SceneSRT, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("self.ss.setPlanner(og.LBTRRT(si))")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_LBTRRT
  }

  @combinator object TRRTPlannerTemplate
    extends CombinatorPlannerTemplate[SceneSRT, List[List[Float]]] {
    override val pf: (SceneSRT, String) => List[List[Float]] =
      (_: SceneSRT, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("self.ss.setPlanner(og.TRRT(si))")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_TRRT
  }

  @combinator object LazyRRTPlannerTemplate
    extends CombinatorPlannerTemplate[SceneSRT, List[List[Float]]] {
    override val pf: (SceneSRT, String) => List[List[Float]] =
      (_: SceneSRT, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("self.ss.setPlanner(og.LazyRRT(si))")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_LazyRRT
  }

  @combinator object ESTPlannerTemplate
    extends CombinatorPlannerTemplate[SceneSRT, List[List[Float]]] {
    override val pf: (SceneSRT, String) => List[List[Float]] =
      (_: SceneSRT, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("self.ss.setPlanner(og.EST(si))")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_EST
  }

  @combinator object SBLPlannerTemplate
    extends CombinatorPlannerTemplate[SceneSRT, List[List[Float]]] {
    override val pf: (SceneSRT, String) => List[List[Float]] =
      (_: SceneSRT, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("self.ss.setPlanner(og.SBL(si))")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_SBL
  }

  @combinator object KPIECE1PlannerTemplate
    extends CombinatorPlannerTemplate[SceneSRT, List[List[Float]]] {
    override val pf: (SceneSRT, String) => List[List[Float]] =
      (_: SceneSRT, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("self.ss.setPlanner(og.KPIECE1(si))")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_KPIECE1
  }

  @combinator object BKPIECE1PlannerTemplateProb
    extends CombinatorPlannerTemplate[ProblemDefinitionFiles, List[List[Float]]] {
    override val pf: (ProblemDefinitionFiles, String) => List[List[Float]] =
      (_: ProblemDefinitionFiles, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList

    override val st: SubstitutionScheme = defaultPlannerSubstScheme("self.ss.setPlanner(og.BKPIECE1(self.si))")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_BKPIECE1
  }

  @combinator object BKPIECE1PlannerTemplate
    extends CombinatorPlannerTemplate[SceneSRT, List[List[Float]]] {
    override val pf: (SceneSRT, String) => List[List[Float]] =
      (_: SceneSRT, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("self.ss.setPlanner(og.BKPIECE1(si))")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_BKPIECE1
  }

  @combinator object LBKPIECE1PlannerTemplate
    extends CombinatorPlannerTemplate[SceneSRT, List[List[Float]]] {
    override val pf: (SceneSRT, String) => List[List[Float]] =
      (_: SceneSRT, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("self.ss.setPlanner(og.LBKPIECE1(si))")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_LBKPIECE1
  }


  @combinator object STRIDEPlannerTemplate
    extends CombinatorPlannerTemplate[SceneSRT, List[List[Float]]] {
    override val pf: (SceneSRT, String) => List[List[Float]] =
      (_: SceneSRT, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("self.ss.setPlanner(og.STRIDE(si))")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_STRIDE
  }

  @combinator object PDSTPlannerTemplate
    extends CombinatorPlannerTemplate[SceneSRT, List[List[Float]]] {
    override val pf: (SceneSRT, String) => List[List[Float]] =
      (_: SceneSRT, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("self.ss.setPlanner(og.PDST(si))")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_PDST
  }

  @combinator object FMTPlannerTemplate
    extends CombinatorPlannerTemplate[SceneSRT, List[List[Float]]] {
    override val pf: (SceneSRT, String) => List[List[Float]] =
      (_: SceneSRT, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("self.ss.setPlanner(og.FMT(si))")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_FMT
  }

  @combinator object BFMTPlannerTemplate
    extends CombinatorPlannerTemplate[SceneSRT, List[List[Float]]] {
    override val pf: (SceneSRT, String) => List[List[Float]] =
      (_: SceneSRT, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("self.ss.setPlanner(og.BFMT(si))")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_BFMT
  }

  @combinator object RRTsharpPlannerTemplate
    extends CombinatorPlannerTemplate[SceneSRT, List[List[Float]]] {
    override val pf: (SceneSRT, String) => List[List[Float]] =
      (_: SceneSRT, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("self.ss.setPlanner(og.RRTsharp(si))")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_RRTsharp
  }


  @combinator object RRTXstaticPlannerTemplate
    extends CombinatorPlannerTemplate[SceneSRT, List[List[Float]]] {
    override val pf: (SceneSRT, String) => List[List[Float]] =
      (_: SceneSRT, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("self.ss.setPlanner(og.RRTXstatic(si))")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_RRTXstatic
  }

  @combinator object InformedRRTstarPlannerTemplate
    extends CombinatorPlannerTemplate[SceneSRT, List[List[Float]]] {
    override val pf: (SceneSRT, String) => List[List[Float]] =
      (_: SceneSRT, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("self.ss.setPlanner(og.InformedRRTstar(si))")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_InformedRRTstar
  }

  @combinator object BITstarPlannerTemplate
    extends CombinatorPlannerTemplate[SceneSRT, List[List[Float]]] {
    override val pf: (SceneSRT, String) => List[List[Float]] =
      (_: SceneSRT, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("self.ss.setPlanner(og.BITstar(si))")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_BITstar
  }

  def defaultPlannerSubstScheme(s: String): SubstitutionScheme =
    SubstitutionScheme(
      Map(sbmpStartTemplate -> sbmpMainStartFile),
      Map("$plannerMainPlannerInst$" -> s))

  def parseDefaultOmplPath(str: String): Seq[List[Float]] = {
    val index = str.indexOf("solution path:")
    if (index > -1)
      str.substring(index).split("\r\n").
        withFilter((s: String) => s.startsWith("RealVectorState")).
        map(s => s.substring(s.indexOf("[") + 1, s.indexOf("]")).split(" ").map(_.toFloat).toList).toList
    else Seq.empty
  }

  @combinator object PrmPlannerTemplatePathSmoothing
      extends CombinatorPlannerTemplate[(ProblemDefinitionFiles,List[List[Float]]), List[List[Float]]] {
    override val pf: ((ProblemDefinitionFiles,List[List[Float]]), String) => List[List[Float]] =
      (_: (ProblemDefinitionFiles,List[List[Float]]), plannerOut: String) =>
        parseDefaultOmplPath(plannerOut).toList

      override val st: SubstitutionScheme = SubstitutionScheme(
        Map(sbmpStartTemplate->sbmpMainStartFile),
        Map("$plannerMainPlannerInst$" -> "self.ss.setPlanner(og.PRM(self.si))"))
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


}