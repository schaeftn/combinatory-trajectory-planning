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
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("og.PRM(self.si)")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_PRM
  }

  @combinator object RrtPlannerTemplate
    extends CombinatorPlannerTemplate[SceneSRT, List[List[Float]]] {
    override val pf: (SceneSRT, String) => List[List[Float]] =
      (_: SceneSRT, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("og.RRT(self.si)")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_RRT
  }

  @combinator object PrmStarPlannerTemplate
    extends CombinatorPlannerTemplate[SceneSRT, List[List[Float]]] {
    override val pf: (SceneSRT, String) => List[List[Float]] =
      (_: SceneSRT, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("og.PRMstar(self.si)")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_PRMStar
  }

  @combinator object LazyPrmPlannerTemplate
    extends CombinatorPlannerTemplate[SceneSRT, List[List[Float]]] {
    override val pf: (SceneSRT, String) => List[List[Float]] =
      (_: SceneSRT, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("og.LazyPRM(self.si)")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_LazyPRM
  }

  @combinator object LazyPrmStarPlannerTemplate
    extends CombinatorPlannerTemplate[SceneSRT, List[List[Float]]] {
    override val pf: (SceneSRT, String) => List[List[Float]] =
      (_: SceneSRT, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("og.LazyPRMstar(self.si)")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_LazyPRMStar
  }

  @combinator object SSTPlannerTemplate
    extends CombinatorPlannerTemplate[SceneSRT, List[List[Float]]] {
    override val pf: (SceneSRT, String) => List[List[Float]] =
      (_: SceneSRT, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("og.SST(self.si)")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_SST
  }

  @combinator object RRTConnectPlannerTemplate
    extends CombinatorPlannerTemplate[SceneSRT, List[List[Float]]] {
    override val pf: (SceneSRT, String) => List[List[Float]] =
      (_: SceneSRT, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList

    override val st: SubstitutionScheme = defaultPlannerSubstScheme("og.PRMstar(si)")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_RRTConnect
  }

  @combinator object RRTStarPlannerTemplate
    extends CombinatorPlannerTemplate[SceneSRT, List[List[Float]]] {
    override val pf: (SceneSRT, String) => List[List[Float]] =
      (_: SceneSRT, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("og.RRTstar(si)")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_RRTStar
  }

  @combinator object LBTRRTPlannerTemplate
    extends CombinatorPlannerTemplate[SceneSRT, List[List[Float]]] {
    override val pf: (SceneSRT, String) => List[List[Float]] =
      (_: SceneSRT, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("og.LBTRRT(si)")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_LBTRRT
  }

  @combinator object TRRTPlannerTemplate
    extends CombinatorPlannerTemplate[SceneSRT, List[List[Float]]] {
    override val pf: (SceneSRT, String) => List[List[Float]] =
      (_: SceneSRT, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("og.TRRT(si)")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_TRRT
  }

  @combinator object LazyRRTPlannerTemplate
    extends CombinatorPlannerTemplate[SceneSRT, List[List[Float]]] {
    override val pf: (SceneSRT, String) => List[List[Float]] =
      (_: SceneSRT, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("og.LazyRRT(si)")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_LazyRRT
  }

  @combinator object ESTPlannerTemplate
    extends CombinatorPlannerTemplate[SceneSRT, List[List[Float]]] {
    override val pf: (SceneSRT, String) => List[List[Float]] =
      (_: SceneSRT, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("og.EST(si)")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_EST
  }

  @combinator object SBLPlannerTemplate
    extends CombinatorPlannerTemplate[SceneSRT, List[List[Float]]] {
    override val pf: (SceneSRT, String) => List[List[Float]] =
      (_: SceneSRT, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("og.SBL(si)")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_SBL
  }

  @combinator object KPIECE1PlannerTemplate
    extends CombinatorPlannerTemplate[SceneSRT, List[List[Float]]] {
    override val pf: (SceneSRT, String) => List[List[Float]] =
      (_: SceneSRT, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("og.KPIECE1(si)")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_KPIECE1
  }

  @combinator object BKPIECE1PlannerTemplate
    extends CombinatorPlannerTemplate[SceneSRT, List[List[Float]]] {
    override val pf: (SceneSRT, String) => List[List[Float]] =
      (_: SceneSRT, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("og.BKPIECE1(si)")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_BKPIECE1
  }

  @combinator object LBKPIECE1PlannerTemplate
    extends CombinatorPlannerTemplate[SceneSRT, List[List[Float]]] {
    override val pf: (SceneSRT, String) => List[List[Float]] =
      (_: SceneSRT, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("og.LBKPIECE1(si)")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_LBKPIECE1
  }


  @combinator object STRIDEPlannerTemplate
    extends CombinatorPlannerTemplate[SceneSRT, List[List[Float]]] {
    override val pf: (SceneSRT, String) => List[List[Float]] =
      (_: SceneSRT, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("og.STRIDE(si)")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_STRIDE
  }

  @combinator object PDSTPlannerTemplate
    extends CombinatorPlannerTemplate[SceneSRT, List[List[Float]]] {
    override val pf: (SceneSRT, String) => List[List[Float]] =
      (_: SceneSRT, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("og.PDST(si)")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_PDST
  }

  @combinator object FMTPlannerTemplate
    extends CombinatorPlannerTemplate[SceneSRT, List[List[Float]]] {
    override val pf: (SceneSRT, String) => List[List[Float]] =
      (_: SceneSRT, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("og.FMT(si)")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_FMT
  }

  @combinator object BFMTPlannerTemplate
    extends CombinatorPlannerTemplate[SceneSRT, List[List[Float]]] {
    override val pf: (SceneSRT, String) => List[List[Float]] =
      (_: SceneSRT, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("og.BFMT(si)")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_BFMT
  }

  @combinator object RRTsharpPlannerTemplate
    extends CombinatorPlannerTemplate[SceneSRT, List[List[Float]]] {
    override val pf: (SceneSRT, String) => List[List[Float]] =
      (_: SceneSRT, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("og.RRTsharp(si)")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_RRTsharp
  }


  @combinator object RRTXstaticPlannerTemplate
    extends CombinatorPlannerTemplate[SceneSRT, List[List[Float]]] {
    override val pf: (SceneSRT, String) => List[List[Float]] =
      (_: SceneSRT, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("og.RRTXstatic(si)")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_RRTXstatic
  }

  @combinator object InformedRRTstarPlannerTemplate
    extends CombinatorPlannerTemplate[SceneSRT, List[List[Float]]] {
    override val pf: (SceneSRT, String) => List[List[Float]] =
      (_: SceneSRT, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("og.InformedRRTstar(si)")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_InformedRRTstar
  }

  @combinator object BITstarPlannerTemplate
    extends CombinatorPlannerTemplate[SceneSRT, List[List[Float]]] {
    override val pf: (SceneSRT, String) => List[List[Float]] =
      (_: SceneSRT, plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("og.BITstar(si)")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_planner_BITstar
  }

  def defaultPlannerSubstScheme(s: String): SubstitutionScheme =
    SubstitutionScheme(
      Map(sbmpStartTemplate -> sbmpMainStartFile),
      Map("$plannerMainPlannerInst$" -> s))

  def parseDefaultOmplPath(str: String): Seq[List[Float]] = str.substring(str.indexOf("solution path:")).split("\r\n").
    filter((s: String) => s.startsWith("RealVectorState")).
    map(s => s.substring(s.indexOf("[") + 1, s.indexOf("]")).split(" ").map(_.toFloat).toList).toList


/*
  @combinator object PrmPlannerTemplatePathSmoothing
    extends CombinatorPlannerTemplate[PolySceneSegmentationRoadmapPath, PolySceneSegmentationRoadmapPath] {
    override val pf: (PolySceneSegmentationRoadmapPath, String) => PolySceneSegmentationRoadmapPath =
      (inputData: PolySceneSegmentationRoadmapPath, plannerOut: String) =>
        inputData.withPath(parsePath(plannerOut))

    def parsePath(str: String): Seq[List[Float]] = str.substring(str.indexOf("Solution path:")).split("\r\n").
      filter((s: String) => s.startsWith("RealVectorState")).
      map(s => s.substring(s.indexOf("["), s.indexOf("]")).split(" ").map(_.toFloat).toList).toList

    override val st: SubstitutionScheme = SubstitutionScheme(Map(sbmpStartTemplate->sbmpMainStartFile), Map("$plannerMainPlannerInst$" -> "og.PRM(self.si)"))
    override val startFile: String = sbmpMainStartFile
    val semanticType = sbmp_planner_PRM
  }
*/

}