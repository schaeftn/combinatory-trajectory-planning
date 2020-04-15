package org.combinators.ctp.repositories.samplebased

import org.combinators.cls.interpreter.combinator
import org.combinators.cls.types.Constructor
import org.combinators.ctp.repositories.scene.{PolySceneSegmentationRoadmapPath, SceneSRT, SceneUtils}
import org.combinators.ctp.repositories.python_interop.{PlannerScheme, PythonTemplateUtils, PythonWrapper, SubstitutionScheme}
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.mptasks.MpTaskStartGoal

trait SbmpTopLevelRepository extends SceneUtils with PythonTemplateUtils with SbmpInputDataRepository with SbmpOptimizeCostRepository with SbmpSamplerRepository with SbmpValidatorRepository {

  val sbmpDefaultKindingMap = Map(sbmp_planner_var -> Seq(sbmp_planner_PRM),
    sbmp_sampler_var -> Seq(sbmp_uniform_valid_state_sampler),
    sbmp_state_validator_var -> Seq(sbmp_fcl_validator),
    sbmp_motion_validator_var -> Seq(sbmp_fcl_motion_validator),
    sbmp_cost_var -> Seq(sbmp_default_cost_state),
    sbmp_optimization_objective_var -> Seq(sbmp_opt_path_length)
  )

  trait OmplPlannerTrait[A, B] {
    def apply(pScheme: PlannerScheme[A,B],
              samplerSubstScheme: SubstitutionScheme,
              stateValidatorSubstScheme: SubstitutionScheme,
              motionValidatorSubstScheme: SubstitutionScheme,
              optimizationCostSubstScheme: SubstitutionScheme,
              dataSubstScheme: (A, MpTaskStartGoal) => SubstitutionScheme
             ): (A, MpTaskStartGoal) => B = { (input: A, task: MpTaskStartGoal) =>
      println("ompltrait")
      val schemeList = List(pScheme.st, samplerSubstScheme, stateValidatorSubstScheme,
        motionValidatorSubstScheme, optimizationCostSubstScheme, dataSubstScheme(input, task))
      val newScheme = schemeList.reduce(_.merge(_))

      val pWrapper = PythonWrapper.apply(newScheme, pScheme.startFile, pScheme.pf)
      println("pWrapper starting")
      pWrapper.computeResultAndModifyInput(input)
    }

    val semanticType =
      sbmp_planner_var =>:
        sbmp_sampler_var =>:
        sbmp_state_validator_var =>:
        sbmp_motion_validator_var =>:
        sbmp_cost_var :&: sbmp_optimization_objective_var =>:
        sbmp_input_data =>: //ggf plus sampler, plus motion validator?
        sbmp_planning_algorithm :&: sbmp_planner_var :&: sbmp_sampler_var :&:
          sbmp_state_validator_var :&: sbmp_motion_validator_var :&: sbmp_optimization_objective_var :&:
          sbmp_cost_var
  }

  @combinator object OmplPlannerRefinement extends
    OmplPlannerTrait[PolySceneSegmentationRoadmapPath, PolySceneSegmentationRoadmapPath] {}

  @combinator object OmplPlannerPrmStandard extends
    OmplPlannerTrait[SceneSRT, List[List[Float]]] {}

  trait CombinatorPlannerTemplate[A, B] {
    def apply = new PlannerScheme[A, B](st, pf, startFile)

    val pf: (A, String) => B
    val st: SubstitutionScheme
    val startFile: String
  }

  @combinator object PrmPlannerTemplate
    extends CombinatorPlannerTemplate[SceneSRT, List[List[Float]]] {
    override val pf: (SceneSRT, String) => List[List[Float]] =
      (_: SceneSRT, plannerOut: String) =>
        parsePath(plannerOut).toList
        //inputData.withPath(parsePath(plannerOut))

    def parsePath(str: String): Seq[List[Float]] = str.substring(str.indexOf("solution path:")).split("\r\n").
      filter((s: String) => s.startsWith("RealVectorState")).
      map(s => s.substring(s.indexOf("[") + 1, s.indexOf("]")).split(" ").map(_.toFloat).toList).toList

    override val st: SubstitutionScheme = SubstitutionScheme(
      Map(sbmpStartTemplate -> sbmpMainStartFile),
      Map("$plannerMainPlannerInst$" -> "og.PRM(self.si)"))
    override val startFile: String = sbmpMainStartFile
    val semanticType = sbmp_planner_PRM
  }

  @combinator object RrtPlannerTemplate
    extends CombinatorPlannerTemplate[SceneSRT, List[List[Float]]] {
    override val pf: (SceneSRT, String) => List[List[Float]] =
      (_: SceneSRT, plannerOut: String) =>
        parsePath(plannerOut).toList

    def parsePath(str: String): Seq[List[Float]] = str.substring(str.indexOf("solution path:")).split("\r\n").
      filter((s: String) => s.startsWith("RealVectorState")).
      map(s => s.substring(s.indexOf("[") + 1, s.indexOf("]")).split(" ").map(_.toFloat).toList).toList

    override val st: SubstitutionScheme = SubstitutionScheme(
      Map(sbmpStartTemplate -> sbmpMainStartFile),
      Map("$plannerMainPlannerInst$" -> "og.RRT(self.si)"))
    override val startFile: String = sbmpMainStartFile
    val semanticType = sbmp_planner_RRT
  }

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


  trait EmptyTemplateScheme[A] {
    def apply: A => SubstitutionScheme = (_: A) => SubstitutionScheme(
      Map.empty[String, String], //no files
      Map.empty[String, String], //no substitutions
    )
  }

  @combinator object EmptyTemplateScheme extends EmptyTemplateScheme[String] {
    val semanticType = Constructor("emptyTemplate")
  }

  //
  //
  //  @combinator object SceneToPath {
  //    def apply(): (SceneSRT, MpTaskStartGoal) => List[List[Float]] = { case (s, t) =>
  //      def runCdFile(fclSceneString: String, startStopStateString: String):List[List[Float]] = {
  //        //TODO SubstTemplate instead
  //        substituteStringsInFile(fclSamplingTemplateFile, fclSamplingGenFile,
  //          Map("{substitute}" -> fclSceneString))
  //        substituteStringsInFile(samplingStartFileTemplate,  samplingStartFile,
  //          Map("{substitute}" -> startStopStateString))
  //        val foo = s"python3 $samplingStartFile"
  //        val resultString = foo.lineStream_!.takeWhile(_ => true)
  //        println(s"""Resultstring: ${resultString.toList.mkString("\r\n")}""")
  //        println("decoded")
  //        resultStringToVertexList(resultString.filter(_.startsWith("RealVectorState")))
  //      }
  //
  //      val pyString: String = sceneSRTtoFclPythonString(s)
  //      val startGoalStr: String =   taskGoalStartToPythonOMPLString(t)
  //      println(s"Python Scene String: ${pyString}")
  //      runCdFile(pyString, startGoalStr)
  //    }
  //
  //    val semanticType = Constructor("sampleCombinatorTop")
  //  }
  //
  //
  //
  //  @combinator object SceneToPath2D {
  //    def apply(): (SceneSRT, MpTaskStartGoal) => List[List[Float]] = { case (s, t) =>
  //      val pythonSettings = new Properties()
  //      pythonSettings.load(getClass.getClassLoader.getResourceAsStream("pythoninterop.properties"))
  //
  //      //TODO Anpassen...
  //
  //
  //      def runCdFile(cgalFileLocation: String, startStopStateString: String):List[List[Float]] = {
  //        substituteStringsInFile(fclSamplingTemplateFile, fclSamplingGenFile,
  //          Map("{substituteScene}" -> cgalFileLocation))
  //        substituteStringsInFile(samplingStartFileTemplate, samplingStartFile,
  //          Map("{substituteTask}" -> startStopStateString, "{substitutePath}"-> startStopStateString))
  //        val foo = s"python3 $samplingStartFile"
  //        val resultString = foo.lineStream_!.takeWhile(_ => true)
  //        println(s"""Resultstring: ${resultString.toList.mkString("\r\n")}""")
  //        println("decoded")
  //        resultStringToVertexList(resultString.filter(_.startsWith("RealVectorState")))
  //      }
  //
  //      val pyString: String = sceneSRTtoFclPythonString(s)
  //      val startGoalStr: String =   taskGoalStartToPythonOMPLString(t)
  //      println(s"Python Scene String: ${pyString}")
  //      runCdFile(pyString, startGoalStr)
  //    }
  //
  //    val semanticType = Constructor("2dTest")
  //  }
  //
  //  def resultStringToVertexList(lines: Stream[String]): List[List[Float]] = {
  //    lines.map(_.replace("RealVectorState ", "").replace("[", "")
  //      .replace("]", "").split(" "))
  //      .map(line => line.map(element => element.toFloat).toList).toList
  //  }
}