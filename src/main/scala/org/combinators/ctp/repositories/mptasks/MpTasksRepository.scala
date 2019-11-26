package org.combinators.ctp.repositories.mptasks

import org.combinators.cls.interpreter.combinator
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories.taxkinding.{CombinatorialMotionPlanning, CtpTaxonomy, GeometricModelTypes, MpTask}


trait MpTasksRepository extends MpTask with CombinatorialMotionPlanning with GeometricModelTypes {

//  @combinator object ShortestPathCombinator {
//    def apply(start: vertexType, end: vertexType): MpTaskStartGoal =
//      new MpTaskSpTwoD(start, end) {}
//
//    val semanticType = mpt_start_position_type :&: dimensionality_two_d_t =>:
//      mpt_target_position_type :&: dimensionality_two_d_t =>: mpt_shortest_path_type :&: dimensionality_two_d_t
//  }

  //Add Dynamically new SynthesisTask ...
  // Oder Synth Task -> Scene -> SynthesisGoal


/*
  •	Path Finding
  •	Traveling Salesman
  •	Coverage
  •	max Clearance
  (•	Persuer Evader, weil reaktiv?!)
*/


  //Todo Propertyfile => type
/*

  @combinator object PathCoverageCombinator {
    def apply = ???
    val semanticType = mpt_pc_type
  }

  @combinator object MaxClearanceCombinator {
    def apply = ???
    val semanticType = mpt_mc_type
  }
*/


}