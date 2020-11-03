package org.combinators.ctp.repositories.samplebased

import org.combinators.cls.interpreter.combinator
import org.combinators.cls.types.{Constructor, Type}
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.python_interop.{ModifierPlannerScheme, PlannerScheme, PythonTemplateUtils, PythonWrapper, SubstitutionScheme}
import org.combinators.ctp.repositories.scene.SceneUtils
import org.combinators.ctp.repositories.toplevel._
import io.circe.parser.decode
import io.circe.generic.auto._
import io.circe.syntax._

trait SbmpPlannerTemplateRepository extends SceneUtils with PythonTemplateUtils {

  trait CombinatorPlannerTemplate[B] {
    def apply(optObjectice: SubstitutionScheme, sampler: SubstitutionScheme) = new PlannerScheme[B](optObjectice.merge(sampler.merge(st)), pf, startFile)

    val pf: (String) => B
    val st: SubstitutionScheme
    val startFile: String
  }

  trait CombinatorPlannerTemplateStdPath extends CombinatorPlannerTemplate[List[List[Float]]] {
    override val pf = {
      plannerOut: String => parseDefaultOmplPath(plannerOut).toList
    }
  }

  trait ModifierCombinatorPlannerTemplate[A, B] {
    def apply = new ModifierPlannerScheme[A, B](st, pf, startFile)

    val pf: (A, String) => B
    val st: SubstitutionScheme
    val startFile: String
  }

  @combinator object PrmPlannerTemplate
    extends CombinatorPlannerTemplate[List[List[Float]]] {
    override val pf: (String) => List[List[Float]] =
      (plannerOut: String) => parseDefaultOmplPath(plannerOut).toList
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("PRM")
    override val startFile: String = sbmpMainStartFile

    val semanticType =
      (any_sbmp_optimization_objective_type=>:sbmp_spaceSampler_type=>:sbmp_planner_PRM):&:
        (any_sbmp_optimization_objective_type=>:sbmp_validStateSampler_type=>:sbmp_planner_PRM):&:
        (any_sbmp_optimization_objective_type=>:sbmp_informedSampler_type=>:sbmp_planner_PRM)
  }

  @combinator object RrtPlannerTemplate extends CombinatorPlannerTemplateStdPath {
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("RRT")
    override val startFile: String = sbmpMainStartFile

    val semanticType = (sbmp_opt_path_length=>:sbmp_spaceSampler_type=>:sbmp_planner_RRT):&:
      (sbmp_opt_path_length=>:sbmp_informedSampler_type=>:sbmp_planner_RRT)
  }

  @combinator object RRTConnectPlannerTemplate extends CombinatorPlannerTemplateStdPath {
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("RRTConnect")
    override val startFile: String = sbmpMainStartFile

    val semanticType = (sbmp_opt_path_length=>:sbmp_spaceSampler_type=>:sbmp_planner_RRTConnect):&:
      (sbmp_opt_path_length=>:sbmp_informedSampler_type=>:sbmp_planner_RRTConnect)
  }

  @combinator object PrmStarPlannerTemplate extends CombinatorPlannerTemplateStdPath {
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("PRMstar")
    override val startFile: String = sbmpMainStartFile

    val semanticType = (any_sbmp_optimization_objective_type=>:sbmp_spaceSampler_type=>:sbmp_planner_PRMStar):&:
      (any_sbmp_optimization_objective_type=>:sbmp_validStateSampler_type=>:sbmp_planner_PRMStar):&:
      (any_sbmp_optimization_objective_type=>:sbmp_informedSampler_type=>:sbmp_planner_PRMStar)

  }

  @combinator object LazyPrmPlannerTemplate extends CombinatorPlannerTemplateStdPath {
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("LazyPRM")
    override val startFile: String = sbmpMainStartFile

    val semanticType = (any_sbmp_optimization_objective_type=>:sbmp_spaceSampler_type=>:sbmp_planner_LazyPRM):&:
      (any_sbmp_optimization_objective_type=>:sbmp_informedSampler_type=>:sbmp_planner_LazyPRM)
  }

  @combinator object LazyPrmStarPlannerTemplate extends CombinatorPlannerTemplateStdPath {
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("LazyPRMstar")
    override val startFile: String = sbmpMainStartFile

    val semanticType = (any_sbmp_optimization_objective_type=>:sbmp_spaceSampler_type=>:sbmp_planner_PRMStar):&:
      (any_sbmp_optimization_objective_type=>:sbmp_validStateSampler_type=>:sbmp_planner_PRMStar):&:
      (any_sbmp_optimization_objective_type=>:sbmp_informedSampler_type=>:sbmp_planner_PRMStar)
  }

  @combinator object SSTPlannerTemplate extends CombinatorPlannerTemplateStdPath {
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("SST")
    override val startFile: String = sbmpMainStartFile

    val semanticType = (any_sbmp_optimization_objective_type=>:sbmp_spaceSampler_type=>:sbmp_planner_SST):&:
      (any_sbmp_optimization_objective_type=>:sbmp_informedSampler_type=>:sbmp_planner_SST)
  }


  @combinator object RRTStarPlannerTemplate extends CombinatorPlannerTemplateStdPath {
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("RRTstar")
    override val startFile: String = sbmpMainStartFile

    val semanticType = (any_sbmp_optimization_objective_type=>:sbmp_spaceSampler_type=>:sbmp_planner_RRTStar):&:
      (any_sbmp_optimization_objective_type=>:sbmp_informedSampler_type=>:sbmp_planner_RRTStar)
  }

  @combinator object LBTRRTPlannerTemplate extends CombinatorPlannerTemplateStdPath {
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("LBTRRT")
    override val startFile: String = sbmpMainStartFile

    val semanticType = (sbmp_opt_path_length=>:sbmp_spaceSampler_type=>:sbmp_planner_LBTRRT):&:
      (sbmp_opt_path_length=>:sbmp_informedSampler_type=>:sbmp_planner_LBTRRT)
  }

  @combinator object TRRTPlannerTemplate extends CombinatorPlannerTemplateStdPath {
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("TRRT")
    override val startFile: String = sbmpMainStartFile

    val semanticType = (any_sbmp_optimization_objective_type=>:sbmp_spaceSampler_type=>:sbmp_planner_TRRT):&:
      (any_sbmp_optimization_objective_type=>:sbmp_informedSampler_type=>:sbmp_planner_TRRT)
  }

  @combinator object LazyRRTPlannerTemplate extends CombinatorPlannerTemplateStdPath {
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("LazyRRT")
    override val startFile: String = sbmpMainStartFile

    val semanticType =(sbmp_opt_path_length=>:sbmp_spaceSampler_type=>:sbmp_planner_LazyRRT):&:
      (sbmp_opt_path_length=>:sbmp_informedSampler_type=>:sbmp_planner_LazyRRT)
  }

  @combinator object ESTPlannerTemplate extends CombinatorPlannerTemplateStdPath {
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("EST")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_opt_path_length=>:sbmp_validStateSampler_type=>:sbmp_planner_EST
  }

  @combinator object SBLPlannerTemplate extends CombinatorPlannerTemplateStdPath {
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("SBL")
    override val startFile: String = sbmpMainStartFile

    val semanticType = sbmp_opt_path_length=>:sbmp_validStateSampler_type=>:sbmp_planner_SBL

  }

  @combinator object KPIECE1PlannerTemplate extends CombinatorPlannerTemplateStdPath {
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("KPIECE1")
    override val startFile: String = sbmpMainStartFile

    val semanticType = (sbmp_opt_path_length =>: sbmp_spaceSampler_type =>: sbmp_planner_KPIECE1) :&:
      (sbmp_opt_path_length =>: sbmp_informedSampler_type =>: sbmp_planner_KPIECE1)
  }


  @combinator object BKPIECE1PlannerTemplate extends CombinatorPlannerTemplateStdPath {
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("BKPIECE1")
    override val startFile: String = sbmpMainStartFile
    val semanticType = (sbmp_opt_path_length=>:sbmp_spaceSampler_type=>:sbmp_planner_BKPIECE1):&:
      (sbmp_opt_path_length=>:sbmp_informedSampler_type=>:sbmp_planner_BKPIECE1)
  }

  @combinator object LBKPIECE1PlannerTemplate extends CombinatorPlannerTemplateStdPath {
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("LBKPIECE1")
    override val startFile: String = sbmpMainStartFile
    val semanticType = (sbmp_opt_path_length=>:sbmp_spaceSampler_type=>:sbmp_planner_LBKPIECE1):&:
      (sbmp_opt_path_length=>:sbmp_informedSampler_type=>:sbmp_planner_LBKPIECE1)
  }

  @combinator object STRIDEPlannerTemplate extends CombinatorPlannerTemplateStdPath {
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("STRIDE")
    override val startFile: String = sbmpMainStartFile
    val semanticType = sbmp_opt_path_length=>:sbmp_validStateSampler_type=>:sbmp_planner_STRIDE

  }

  @combinator object PDSTPlannerTemplate extends CombinatorPlannerTemplateStdPath {
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("PDST")
    override val startFile: String = sbmpMainStartFile
    val semanticType = (sbmp_opt_path_length=>:sbmp_spaceSampler_type=>:sbmp_planner_PDST):&:
      (sbmp_opt_path_length=>:sbmp_informedSampler_type=>:sbmp_planner_PDST)
  }

  @combinator object FMTPlannerTemplate extends CombinatorPlannerTemplateStdPath {
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("FMT")
    override val startFile: String = sbmpMainStartFile
    val semanticType = (any_sbmp_optimization_objective_type=>:sbmp_spaceSampler_type=>:sbmp_planner_FMT):&:
      (any_sbmp_optimization_objective_type=>:sbmp_informedSampler_type=>:sbmp_planner_FMT)
  }

  @combinator object BFMTPlannerTemplate extends CombinatorPlannerTemplateStdPath {
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("BFMT")
    override val startFile: String = sbmpMainStartFile
    val semanticType = (any_sbmp_optimization_objective_type=>:sbmp_spaceSampler_type=>:sbmp_planner_BFMT):&:
      (any_sbmp_optimization_objective_type=>:sbmp_informedSampler_type=>:sbmp_planner_BFMT)
  }

  @combinator object RRTsharpPlannerTemplate extends CombinatorPlannerTemplateStdPath {
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("RRTsharp")
    override val startFile: String = sbmpMainStartFile
    val semanticType = (any_sbmp_optimization_objective_type=>:sbmp_spaceSampler_type=>:sbmp_planner_RRTsharp):&:
      (any_sbmp_optimization_objective_type=>:sbmp_informedSampler_type=>:sbmp_planner_RRTsharp)
  }

  @combinator object RRTXstaticPlannerTemplate extends CombinatorPlannerTemplateStdPath {
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("RRTXstatic")
    override val startFile: String = sbmpMainStartFile
    val semanticType = (any_sbmp_optimization_objective_type=>:sbmp_spaceSampler_type=>:sbmp_planner_RRTXstatic):&:
      (any_sbmp_optimization_objective_type=>:sbmp_informedSampler_type=>:sbmp_planner_RRTXstatic)
  }

  @combinator object InformedRRTstarPlannerTemplate extends CombinatorPlannerTemplateStdPath {
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("InformedRRTstar")
    override val startFile: String = sbmpMainStartFile
    val semanticType = (any_sbmp_optimization_objective_type=>:sbmp_spaceSampler_type=>:sbmp_planner_InformedRRTstar):&:
      (any_sbmp_optimization_objective_type=>:sbmp_informedSampler_type=>:sbmp_planner_InformedRRTstar)
  }

  @combinator object BITstarPlannerTemplate extends CombinatorPlannerTemplateStdPath {
    override val st: SubstitutionScheme = defaultPlannerSubstScheme("BITstar")
    override val startFile: String = sbmpMainStartFile
    val semanticType = (any_sbmp_optimization_objective_type=>:sbmp_spaceSampler_type=>:sbmp_planner_BITstar):&:
      (any_sbmp_optimization_objective_type=>:sbmp_informedSampler_type=>:sbmp_planner_BITstar)
  }

  def defaultPlannerSubstScheme(s: String): SubstitutionScheme =
    SubstitutionScheme(
      Map(sbmpStartTemplate -> sbmpMainStartFile),
      Map("$plannerMainPlannerInst$" -> defaultPlannerString(s)))

  //TODO Q&D, handle in a combinatorial way or during dyn repository build
  def parseDefaultOmplPath(str: String): Seq[List[Float]] = {
    parseSE3OmplPath(str)
  }

  /*
  def parseDefaultOmplPath(str: String): Seq[List[Float]] = {
    val index = str.indexOf("Found exact solution:")
    if (index > -1)
      str.substring(index).split("\r\n").
        withFilter((s: String) => s.startsWith("RealVectorState")).
        map(s => s.substring(s.indexOf("[") + 1, s.indexOf("]")).split(" ").map(_.toFloat).toList).toList
    else Seq.empty
  }*/

  def parseSE3OmplPath(str: String): Seq[List[Float]] = {
    val index = str.indexOf("Found exact solution:")

    @scala.annotation.tailrec
    def parseList(remainingStr: List[String], accList: List[List[Float]]): List[List[Float]] = {
      if (remainingStr.contains("Compound state [")) {
        val takeUntil = remainingStr.tail.indexOf("Compound state [")
        val rString = if (takeUntil == -1) {
          remainingStr
        }
        else {
          remainingStr.take(takeUntil + 1)
        }

        val rvState: List[Float] = rString.withFilter((s: String) => s.startsWith("RealVectorState")).
          map(s => s.substring(s.indexOf("[") + 1, s.indexOf("]")).split(" ").map(_.toFloat).toList).
          headOption.getOrElse(List.empty[Float])
        val so3State: List[Float] = rString.withFilter((s: String) => s.startsWith("SO3State")).
          map(s => s.substring(s.indexOf("[") + 1, s.indexOf("]")).split(" ").map(_.toFloat).toList).
          headOption.getOrElse(List.empty[Float])
        val currentFloatList = rvState ++ so3State
        val newAccList = accList :+ currentFloatList
        val newRemainingStr = remainingStr.takeRight(remainingStr.size - rString.size)
        parseList(newRemainingStr, newAccList)
      } else {
        accList
      }
    }

    if (index > -1)
      parseList(str.substring(index).split("\r\n").toList, List.empty[List[Float]]).tail
    else Seq.empty
  }

  @combinator object PrmPlannerTemplatePathSmoothing
    extends ModifierCombinatorPlannerTemplate[(ProblemDefinitionFiles, List[List[Float]]), List[List[Float]]] {
    override val pf: ((ProblemDefinitionFiles, List[List[Float]]), String) => List[List[Float]] =
      (_: (ProblemDefinitionFiles, List[List[Float]]), plannerOut: String) =>
        parseDefaultOmplPath(plannerOut).toList

    override val st: SubstitutionScheme = defaultPlannerSubstScheme("PRM")
    override val startFile: String = sbmpMainStartFile
    val semanticType = sbmp_planner_PRM
  }

  @combinator object KPIECE1PlannerTemplatePathSmoothing
    extends ModifierCombinatorPlannerTemplate[(ProblemDefinitionFiles, List[List[Float]]), List[List[Float]]] {
    override val pf: ((ProblemDefinitionFiles, List[List[Float]]), String) => List[List[Float]] =
      (_: (ProblemDefinitionFiles, List[List[Float]]), plannerOut: String) =>
        parseDefaultOmplPath(plannerOut).toList

    override val st: SubstitutionScheme = SubstitutionScheme(
      Map(sbmpStartTemplate -> sbmpMainStartFile),
      Map("$plannerMainPlannerInst$" -> "p = og.KPIECE1(self.si)\n        p.setBorderFraction(0.9)\n        p.setGoalBias(0.05)\n        p.setRange(0)\n        self.ss.setPlanner(p)"))
    override val startFile: String = sbmpMainStartFile
    val semanticType = sbmp_planner_KPIECE1
  }

  def defaultPlannerString(s: String): String =
    s"p = og.$s(self.si)\n        self.ss.setPlanner(p)"

}