package org.combinators.ctp.repositories.samplebased

import org.combinators.cls.interpreter.combinator
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.python_interop.{PythonTemplateUtils, SubstitutionScheme}

trait SbmpValidatorRepository extends PythonTemplateUtils {

  @combinator object StateValidatorFcl {
    def apply: SubstitutionScheme = {
      val fileMapping: Map[String, String] = Map(samplingStartFileTemplate->samplingStartFile)
      val substMap = Map("$plannerValidatorInst$" -> "self.si.setStateValidityChecker(ob.StateValidityCheckerFn(self.isStateValid))",
        "$sbmp_main.sceneRep$"->"FclSceneRep()")
      SubstitutionScheme(fileMapping, substMap)
    }
    val semanticType = sbmp_fcl_validator
  }

  @combinator object MotionValidatorFcl {
    def apply: SubstitutionScheme = {
      val fileMapping: Map[String, String] = Map(samplingStartFileTemplate->samplingStartFile)
      val substMap = Map("$plannerValidatorInst$" -> "self.si.setStateValidityChecker(ob.StateValidityCheckerFn(self.isStateValid))",
        "$sbmp_main.motionValidator$"->"MyMotionValidator(self.si, self.scene)"
      )
      SubstitutionScheme(fileMapping, substMap)
    }
    val semanticType = sbmp_fcl_motion_validator
  }
}