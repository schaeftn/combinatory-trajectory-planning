package org.combinators.ctp.repositories.dynrepository

import org.combinators.cls.types.Constructor
import org.combinators.ctp.repositories.sbmp_opt_integral

object SbmpOptObjective extends Enumeration {
  type EnumType = Value
  val sbmp_opt_path_length, sbmp_opt_integral, not_specified = Value
}

object SbmpCosts extends Enumeration {
  type EnumType = Value
  val sbmp_default_cost_state, sbmp_cost_state_change_weighted, not_specified = Value
}