package org.combinators.ctp.repositories.taxkinding

import org.combinators.cls.types.{Constructor, Kinding, Variable}
import org.combinators.ctp.repositories._

trait SbmpSemanticTypes {
  val sbmp_planner_var = Variable("sbmp_planner_var")
  val sbmp_planner_PRM = Constructor("sbmp_planner_PRM")
  val sbmp_planner_PRMStar = Constructor("sbmp_planner_PRMStar")
  val sbmp_planner_LazyPRM = Constructor("sbmp_planner_LazyPRM")
  val sbmp_planner_LazyPRMStar = Constructor("sbmp_planner_LazyPRMStar")
  val sbmp_planner_SST = Constructor("sbmp_planner_SST")
  val sbmp_planner_RRT = Constructor("sbmp_planner_RRT")
  val sbmp_planner_RRTStar = Constructor("sbmp_planner_RRTStar")
  val sbmp_planner_LBTRRT = Constructor("sbmp_planner_LBTRRT")
  val sbmp_planner_TRRT = Constructor("sbmp_planner_TRRT")
//  val sbmp_planner_VFRRT = Constructor("sbmp_planner_VFRRT") // no py bindings available
//  val sbmp_planner_pRRT = Constructor("sbmp_planner_pRRT") // no py bindings available
  val sbmp_planner_LazyRRT = Constructor("sbmp_planner_LazyRRT")
  val sbmp_planner_cRRT = Constructor("sbmp_planner_cRRT")
  val sbmp_planner_RRTConnect = Constructor("sbmp_planner_RRTConnect")

  val sbmp_planner_EST = Constructor("sbmp_planner_EST")
  val sbmp_planner_SBL = Constructor("sbmp_planner_SBL")
//  val sbmp_planner_pSBL = Constructor("sbmp_planner_pSBL") // no py bindings available

  val sbmp_planner_LBKPIECE1 = Constructor("sbmp_planner_LBKPIECE1")
  val sbmp_planner_KPIECE1 = Constructor("sbmp_planner_KPIECE1")
  val sbmp_planner_BKPIECE1 = Constructor("sbmp_planner_BKPIECE1")

  val sbmp_planner_STRIDE = Constructor("sbmp_planner_STRIDE")
  val sbmp_planner_PDST = Constructor("sbmp_planner_PDST")
  val sbmp_planner_FMT = Constructor("sbmp_planner_FMT")
  val sbmp_planner_BFMT = Constructor("sbmp_planner_BFMT")

  val sbmp_planner_RRTsharp = Constructor("sbmp_planner_RRTsharp")
  val sbmp_planner_RRTXstatic = Constructor("sbmp_planner_RRTXstatic")
  val sbmp_planner_InformedRRTstar = Constructor("sbmp_planner_InformedRRTstar")
  val sbmp_planner_BITstar = Constructor("sbmp_planner_BITstar")
  //  val sbmp_planner_SPARS = Constructor("sbmp_planner_SPARS") // unstable for py
  //  val sbmp_planner_SPARStwo = Constructor("sbmp_planner_SPARStwo") // unstable for py
  //  val sbmp_planner_CForest = Constructor("sbmp_planner_CForest") // no py bindings available
  //  val sbmp_planner_AnytimePathShortening  = Constructor("sbmp_planner_AnytimePathShortening") // no py bindings available


  val sbmp_sampler_var = Variable("sbmp_sampler_var")
  val sbmp_roadmap_valid_state_sampler = Constructor("sbmp_valid_state_sampler")
  val sbmp_milling_valid_state_sampler = Constructor("sbmp_milling_valid_state_sampler")
  val sbmp_uniform_valid_state_sampler = Constructor("sbmp_uniform_valid_state_sampler")
  val sbmp_obstacle_valid_state_sampler = Constructor("sbmp_obstacle_valid_state_sampler")
  val sbmp_gaussian_valid_state_sampler = Constructor("sbmp_gaussian_valid_state_sampler")
  val sbmp_max_clearance_valid_state_sampler = Constructor("sbmp_max_clearance_valid_state_sampler")
  val sbmp_path_optimizer_sampler = Constructor("sbmp_path_optimizer_sampler")

  val sbmp_state_validator_var = Variable("sbmp_state_validator_var")
  val sbmp_fcl_validator = Constructor("sbmp_fcl_validator")
  val sbmp_cgal_validator = Constructor("sbmp_cgal_validator")
  val sbmp_m5a_validator = Constructor("sbmp_m5a_validator")

  val sbmp_motion_validator_var = Variable("sbmp_motion_validator_var")
  val sbmp_fcl_motion_validator = Constructor("sbmp_fcl_motion_validator")
  val sbmp_discrete_motion_validator = Constructor("sbmp_discrete_motion_validator")
  val sbmp_m5a_motion_validator = Constructor("sbmp_m5a_motion_validator") // TBD, Problemspezifisches Validieren möglich?

  val sbmp_optimization_objective_var = Variable("sbmp_optimization_objective_var")
  val sbmp_opt_path_length = Constructor("sbmp_path_length")
  val sbmp_opt_path_clearance = Constructor("sbmp_path_clearance")
  val sbmp_opt_path_smoothness = Constructor("sbmp_path_smoothness")
  val sbmp_opt_control_smoothness = Constructor("sbmp_control_smoothness")
  val sbmp_m5a_integral_fct = Constructor("sbmp_m5a_integral_fct")

  val sbmp_cost_var = Variable("sbmp_cost_var")
  val sbmp_default_cost_state = Constructor("sbmp_default_cost_state")
  val sbmp_cost_state_change = Constructor("sbmp_cost_state_change")
  val sbmp_cost_state_change_weighted = Constructor("sbmp_cost_state_change_weighted")
  val sbmp_cost_state_acceleration = Constructor("sbmp_cost_state_acceleration")


  //TODO Ompl Simplification
  // val sbmp_simplification_splines

  val sbmp_input_data = Constructor("sbmp_input_data")
  val sbmp_planning_algorithm = Constructor("sbmp_planning_algorithm")

//  val map = Map(
//    cmp_cell_decomposition_var -> Seq(
//      cmp_vertical_cell_decomposition_type, cmp_wavefront_cell_decomposition_type, cmp_robot_decomposition_type),
//    cmp_graph_shortest_path_var -> Seq(cmp_graph_dijkstra_type, cmp_graph_a_star_type, cmp_graph_vbi_type))
//  val kinding: Kinding = buildKinding(map)
}