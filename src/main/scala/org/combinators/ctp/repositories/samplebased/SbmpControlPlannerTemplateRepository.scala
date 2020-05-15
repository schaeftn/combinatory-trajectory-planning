package org.combinators.ctp.repositories.samplebased

import org.combinators.cls.interpreter.combinator
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.python_interop.{PlannerScheme, PythonTemplateUtils, SubstitutionScheme}
import org.combinators.ctp.repositories.scene.SceneUtils
import org.combinators.ctp.repositories.toplevel._

trait SbmpControlPlannerTemplateRepository extends SceneUtils with PythonTemplateUtils with SbmpPlannerTemplateRepository {

  /*
  * different main template
  * Existing Plannersubstitutes: different namespace (oc instead of og)
  * ControlSpace => GridDecomposition oder default 2D, 3D grid decomposition
  * Robot => ControlSpace
  */
}