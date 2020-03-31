package org.combinators.ctp.repositories.taxkinding

import org.combinators.cls.types.{Constructor, Kinding, Variable}
import org.combinators.ctp.repositories._

trait SbmpSemanticTypes {
  val sbmp_planner_var = Variable("sbmp_planner_var")
  val sbmp_planner_PRM = Constructor("sbmp_planner_PRM")
  val sbmp_planner_PRMStar = Constructor("sbmp_planner_PRMStar")
  val sbmp_planner_RRT = Constructor("sbmp_planner_RRT")
  val sbmp_planner_RRTStar = Constructor("sbmp_planner_RRTStar")
  val sbmp_planner_cRRT = Constructor("sbmp_planner_cRRT")
  val sbmp_planner_RRTConnect = Constructor("sbmp_planner_RRTConnect")

  val sbmp_sampler_var = Variable("sbmp_sampler_var")
  val sbmp_roadmap_valid_state_sampler = Constructor("sbmp_valid_state_sampler")
  val sbmp_milling_valid_state_sampler = Constructor("sbmp_milling_valid_state_sampler")
  val sbmp_uniform_valid_state_sampler = Constructor("sbmp_uniform_valid_state_sampler")
  val sbmp_obstacle_valid_state_sampler = Constructor("sbmp_obstacle_valid_state_sampler")
  val sbmp_gaussian_valid_state_sampler = Constructor("sbmp_gaussian_valid_state_sampler")
  val sbmp_max_clearance_valid_state_sampler = Constructor("sbmp_max_clearance_valid_state_sampler")

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