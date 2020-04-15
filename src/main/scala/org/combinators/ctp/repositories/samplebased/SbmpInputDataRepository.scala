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
    def apply: (SceneSRT, MpTaskStartGoal) => SubstitutionScheme = {
      (scene: SceneSRT, task: MpTaskStartGoal) =>
        val fileMapping: Map[String, String] = Map(sbmpStartTemplate -> sbmpMainStartFile,
          fclSceneDataTemplate -> fclSceneDataFile)
        val substituteMap: Map[String, String] = {
          Map("$fcl_scene_data.data$" -> sceneSRTtoFclPythonString(scene),
          "$sbmp_main.startstop$"-> taskGoalStartToPythonOMPLString(task))}
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

  // Path refinement
  // scene rep
  // sampler path hinterlegen
  // Start Goal

}