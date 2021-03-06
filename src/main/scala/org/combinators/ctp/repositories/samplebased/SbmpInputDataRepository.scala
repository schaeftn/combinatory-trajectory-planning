package org.combinators.ctp.repositories.samplebased

import org.combinators.cls.interpreter.combinator
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.toplevel._
import org.combinators.ctp.repositories.python_interop.{PythonTemplateUtils, SubstitutionSchema}
import org.combinators.ctp.repositories.scene._


//Contains all combinators that produce substitution schemes for given scene representations
trait SbmpInputDataRepository extends SceneUtils with PythonTemplateUtils {

  @combinator object SceneSrtToFclSceneData {
    def apply: ((SceneSRT, MpTaskStartGoal)) => SubstitutionSchema = {
      case (scene: SceneSRT, task: MpTaskStartGoal) =>
        val fileMapping: Map[String, String] = Map(sbmpStartTemplate -> sbmpMainStartFile,
          fclSceneDataTemplate -> fclSceneDataFile)
        val substituteMap: Map[String, String] = {
          Map("$fcl_scene_data.data$" -> sceneSRTtoFclPythonString(scene),
            "$sbmp_main.startstop$" -> taskGoalStartToPythonOMPLString(task),
            "$sbmp_main.r3bounds$" -> getOmplBoundsFromScene(scene),
            "$path_data.path$" -> "path_list=np.array([])")
        }
        SubstitutionSchema(fileMapping, substituteMap)
    }

    val semanticType = sbmp_input_data :&: dimensionality_three_d_t
    //ggf plus sampler, plus motion validator?, ggf plus dimension
  }


  @combinator object ScenePolyToCgalSceneData {
    def apply: (PolySceneSegmentationRoadmapPath, MpTaskStartGoal) => SubstitutionSchema = {
      (scene: PolySceneSegmentationRoadmapPath, task: MpTaskStartGoal) =>
        val fileMapping: Map[String, String] = Map(sbmpStartTemplate -> sbmpMainStartFile,
          cgalSceneDataTemplate -> cgalSceneDataFile)
        val substituteMap: Map[String, String] = {
          Map("$cgal_scene_data.data$" -> cgalSceneStringPolyPath(scene),
            "$sbmp_main.startstop$" -> taskGoalStartToPythonOMPLString(task))
        }
        SubstitutionSchema(fileMapping, substituteMap)
    }

    val semanticType = sbmp_input_data :&: dimensionality_two_d_t
    //ggf plus sampler, plus motion validator?, ggf plus dimension
  }


  @combinator object SceneFileImportSceneData {
    def apply: ProblemDefinitionFiles => SubstitutionSchema = {
      scene: ProblemDefinitionFiles =>
        val fileMapping: Map[String, String] = Map(sbmpStartTemplate -> sbmpMainStartFile,
          fclSceneDataTemplate -> fclSceneDataFile)

        val substituteMap: Map[String, String] = {
          Map("$fcl_scene_data.data$" -> loadSceneFromProbDefinition(scene),
            "$sbmp_main.startstop$" -> readOmplStartStopFromCfg(scene.problemProperties),
            "$sbmp_main.r3bounds$" -> readOmplBoundsFromCfg(scene.problemProperties),
            "$path_data.path$" -> "path_list=np.array([])")
        }
        SubstitutionSchema(fileMapping, substituteMap)
    }

    val semanticType = sbmp_input_data :&: dimensionality_three_d_t
    //ggf plus sampler, plus motion validator?, ggf plus dimension
  }

  @combinator object SceneFileImportPathSceneData {
    def apply: ((ProblemDefinitionFiles, List[List[Float]])) => SubstitutionSchema = {
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
        SubstitutionSchema(fileMapping, substituteMap)
    }

    val semanticType = sbmp_input_data :&: dimensionality_three_d_t
    //ggf plus sampler, plus motion validator?, ggf plus dimension
  }

  @combinator object FileBasedWithConfigInputData {
    def apply: ((ProblemDefinitionFiles, String)) => SubstitutionSchema = {
      case (scene: ProblemDefinitionFiles, _: String) =>
        val fileMapping: Map[String, String] = Map(sbmpStartTemplate -> sbmpMainStartFile,
          fclSceneDataTemplate -> fclSceneDataFile,
          pathDataTemplate -> pathDataFile)

        val substituteMap: Map[String, String] = {
          Map("$fcl_scene_data.data$" -> loadSceneFromProbDefinition(scene),
            "$sbmp_main.startstop$" -> readOmplStartStopFromCfg(scene.problemProperties),
            "$sbmp_main.r3bounds$" -> readOmplBoundsFromCfg(scene.problemProperties),
            "$path_data.path$" -> "path_list=np.array([])")
        }
        SubstitutionSchema(fileMapping, substituteMap)
    }

    val semanticType = sbmp_input_data :&: dimensionality_three_d_t
  }


  // Path refinement
  // scene rep
  // sampler path hinterlegen
  // Start Goal

}