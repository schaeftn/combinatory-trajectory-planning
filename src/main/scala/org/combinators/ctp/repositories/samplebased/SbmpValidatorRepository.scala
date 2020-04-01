package org.combinators.ctp.repositories.samplebased

import org.combinators.cls.interpreter.combinator
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.python_interop.{PythonTemplateUtils, SubstitutionScheme}

trait SbmpValidatorRepository extends PythonTemplateUtils {

  @combinator object StateValidatorFcl {
    def apply: SubstitutionScheme = {
      val fileMapping: Map[String, String] = Map(sbmpStartTemplate->sbmpMainStartFile)
      val substMap = Map("$sbmp_main.stateValidator$" -> "self.si.setStateValidityChecker(FclStateValidator(self.si))",
      "$sbmp_main.stateValidatorResolution$" -> "self.si.setStateValidityCheckingResolution(0.01)")
      SubstitutionScheme(fileMapping, substMap)
    }
    val semanticType = sbmp_fcl_validator
  }

  @combinator object MotionValidatorFcl {
    def apply: SubstitutionScheme = {
      val fileMapping: Map[String, String] = Map(sbmpStartTemplate->sbmpMainStartFile)
      val substMap = Map(
        "$sbmp_main.motionValidator$"->"self.si.setMotionValidator(FclMotionValidator(self.si))"
      )
      SubstitutionScheme(fileMapping, substMap)
    }
    val semanticType = sbmp_fcl_motion_validator
  }
}