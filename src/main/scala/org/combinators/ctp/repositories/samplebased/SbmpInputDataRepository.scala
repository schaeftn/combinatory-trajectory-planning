package org.combinators.ctp.repositories.samplebased

import org.combinators.cls.interpreter.combinator
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.toplevel._
import org.combinators.ctp.repositories.python_interop.{PythonTemplateUtils, SubstitutionScheme}
import org.combinators.ctp.repositories.scene._


//Contains all combinators that produce substitution schemes for given scene representations
trait SbmpInputDataRepository extends SceneUtils with PythonTemplateUtils {
  @combinator object SceneSrtToFclSceneData{
    def apply: ((SceneSRT, MpTaskStartGoal)) => SubstitutionScheme = {
      case (scene: SceneSRT, task: MpTaskStartGoal) =>
        val fileMapping: Map[String, String] = Map(sbmpStartTemplate -> sbmpMainStartFile,
          fclSceneDataTemplate -> fclSceneDataFile)
        val substituteMap: Map[String, String] = {
          Map("$fcl_scene_data.data$" -> sceneSRTtoFclPythonString(scene),
          "$sbmp_main.startstop$"-> taskGoalStartToPythonOMPLString(task),
            "$sbmp_main.r3bounds$" -> getOmplBoundsFromScene(scene))}
        SubstitutionScheme(fileMapping, substituteMap)
    }
    val semanticType = sbmp_input_data :&: dimensionality_three_d_t
    //ggf plus sampler, plus motion validator?, ggf plus dimension
  }


  @combinator object ScenePolyToCgalSceneData{
    def apply: (PolySceneSegmentationRoadmapPath, MpTaskStartGoal) => SubstitutionScheme = {
      (scene: PolySceneSegmentationRoadmapPath, task: MpTaskStartGoal) =>
        val fileMapping: Map[String, String] = Map(sbmpStartTemplate -> sbmpMainStartFile,
          cgalSceneDataTemplate -> cgalSceneDataFile)
        val substituteMap: Map[String, String] = {
          Map("$cgal_scene_data.data$" -> cgalSceneStringPolyPath(scene),
            "$sbmp_main.startstop$"-> taskGoalStartToPythonOMPLString(task))}
        SubstitutionScheme(fileMapping, substituteMap)
    }
    val semanticType = sbmp_input_data :&: dimensionality_two_d_t
    //ggf plus sampler, plus motion validator?, ggf plus dimension
  }


  @combinator object SceneFileImportSceneData{
    def apply: ProblemDefinitionFiles => SubstitutionScheme = {
      scene: ProblemDefinitionFiles =>
        val fileMapping: Map[String, String] = Map(sbmpStartTemplate -> sbmpMainStartFile,
          fclSceneDataTemplate -> fclSceneDataFile)

        val substituteMap: Map[String, String] = {
          Map("$fcl_scene_data.data$" -> loadSceneFromProbDefinition(scene),
            "$sbmp_main.startstop$"-> readOmplStartStopFromCfg(scene.problemProperties),
            "$sbmp_main.r3bounds$" -> readOmplBoundsFromCfg(scene.problemProperties))}
        SubstitutionScheme(fileMapping, substituteMap)
    }
    val semanticType = sbmp_input_data :&: dimensionality_three_d_t
    //ggf plus sampler, plus motion validator?, ggf plus dimension
  }

  @combinator object SceneFileImportPathSceneData {
    def apply: ((ProblemDefinitionFiles, List[List[Float]])) => SubstitutionScheme = {
      case (scene: ProblemDefinitionFiles, pathNodes: List[List[Float]]) =>
        val fileMapping: Map[String, String] = Map(sbmpStartTemplate -> sbmpMainStartFile,
          fclSceneDataTemplate -> fclSceneDataFile,
          pathDataTemplate -> pathDataFile)

        val substituteMap: Map[String, String] = {
          Map("$fcl_scene_data.data$" -> loadSceneFromProbDefinition(scene),
            "$sbmp_main.startstop$" -> readOmplStartStopFromCfg(scene.problemProperties),
            "$sbmp_main.r3bounds$" -> readOmplBoundsFromCfg(scene.problemProperties),
            "$path_data.path$" -> writePyPathData(pathNodes)
          )
        }
        SubstitutionScheme(fileMapping, substituteMap)
    }

    val semanticType = sbmp_input_data :&: dimensionality_three_d_t
    //ggf plus sampler, plus motion validator?, ggf plus dimension
  }

  // Path refinement
  // scene rep
  // sampler path hinterlegen
  // Start Goal

}