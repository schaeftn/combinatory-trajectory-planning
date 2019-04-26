package org.combinators.ctp.repositories.mptasks

import org.combinators.cls.interpreter.combinator
import org.combinators.cls.types.{Constructor, Type}


trait MpTasksRepository {
  val mpt_sp_type: Type = Constructor("ShortestPath")
  val mpt_pc_type: Type = Constructor("PathCoverage")
  val mpt_mc_type: Type = Constructor("MaxClearance")

  @combinator object ShortestPathCombinator {
    def apply = ???
    val semanticType = mpt_sp_type
  }

  @combinator object PathCoverageCombinator {
    def apply = ???
    val semanticType = mpt_pc_type
  }

  @combinator object MaxClearanceCombinator {
    def apply = ???
    val semanticType = mpt_mc_type
  }
}