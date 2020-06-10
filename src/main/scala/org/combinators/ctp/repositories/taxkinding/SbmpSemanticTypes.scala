package org.combinators.ctp.repositories.taxkinding

import org.combinators.cls.types.{Constructor, Kinding, Taxonomy, Variable}
import org.combinators.ctp.repositories._

trait SbmpSemanticTypes {
  val any_sbmp_planner_type = Constructor("any_sbmp_planner_type")
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

  val any_sbmp_sampler_type = Constructor("any_sbmp_sampler_type")
  val sbmp_sampler_var = Variable("sbmp_sampler_var")
  // val sbmp_roadmap_valid_state_sampler = Constructor("sbmp_roadmap_valid_state_sampler") TODO
  // val sbmp_milling_valid_state_sampler = Constructor("sbmp_milling_valid_state_sampler") TODO
  val sbmp_uniform_valid_state_sampler = Constructor("sbmp_uniform_valid_state_sampler")
  val sbmp_obstacle_valid_state_sampler = Constructor("sbmp_obstacle_valid_state_sampler")
  val sbmp_gaussian_valid_state_sampler = Constructor("sbmp_gaussian_valid_state_sampler")
  val sbmp_max_clearance_valid_state_sampler = Constructor("sbmp_max_clearance_valid_state_sampler")
  val sbmp_valid_path_optimizer_sampler = Constructor("sbmp_path_optimizer_sampler")
  val sbmp_path_optimizer_sampler = Constructor("sbmp_path_optimizer_sampler")
  val sbmp_uniform_space_sampler = Constructor("sbmp_uniform_space_sampler")
  val sbmp_gaussian_space_sampler = Constructor("sbmp_gaussian_space_sampler")

  val any_sbmp_state_validator_type = Constructor("any_sbmp_state_validator_type")
  val sbmp_state_validator_var = Variable("sbmp_state_validator_var")
  val sbmp_fcl_validator = Constructor("sbmp_fcl_validator")
//  val sbmp_cgal_validator = Constructor("sbmp_cgal_validator") // TODO
//  val sbmp_m5a_validator = Constructor("sbmp_m5a_validator") // TODO

  val any_sbmp_motion_validator_type = Constructor("any_sbmp_motion_validator_type")
  val sbmp_motion_validator_var = Variable("sbmp_motion_validator_var")
  val sbmp_fcl_motion_validator = Constructor("sbmp_fcl_motion_validator")
  val sbmp_discrete_motion_validator = Constructor("sbmp_discrete_motion_validator")
  // val sbmp_m5a_motion_validator = Constructor("sbmp_m5a_motion_validator") // problem specific validation?

  val any_sbmp_optimization_objective_type = Constructor("any_sbmp_optimization_objective_type")
  val sbmp_optimization_objective_var = Variable("sbmp_optimization_objective_var")
  val sbmp_opt_path_length = Constructor("sbmp_path_length")
  val sbmp_opt_integral = Constructor("sbmp_opt_integral")
  // val sbmp_opt_path_clearance = Constructor("sbmp_path_clearance") // TODO
  // val sbmp_opt_path_smoothness = Constructor("sbmp_path_smoothness") // TODO
  // val sbmp_opt_control_smoothness = Constructor("sbmp_control_smoothness") // TODO
  // val sbmp_m5a_integral_fct = Constructor("sbmp_m5a_integral_fct") // TODO

  val any_sbmp_cost_type = Constructor("any_sbmp_cost_type")
  val sbmp_cost_var = Variable("sbmp_cost_var")
  val sbmp_default_cost_state = Constructor("sbmp_default_cost_state")
  val sbmp_cost_clearance_state_change_weighted = Constructor("sbmp_cost_state_change_weighted")
  //  val sbmp_cost_state_change = Constructor("sbmp_cost_state_change") //TODO
  // val sbmp_cost_state_acceleration = Constructor("sbmp_cost_state_acceleration") //TODO

  val sbmp_input_data = Constructor("sbmp_input_data")
  val sbmp_planning_algorithm = Constructor("sbmp_planning_algorithm")

  val any_sbmp_simplification_type = Constructor("any_sbmp_simplification_type")
  val sbmp_simplification_var = Variable("sbmp_simplification_var")
  val sbmp_use_simplification = Constructor("sbmp_use_simplification")
  val sbmp_no_simplification = Constructor("sbmp_no_simplification")

  val taxMapList = Map(
    "any_sbmp_planner_type" -> List("sbmp_planner_PRM", "sbmp_planner_PRMStar", "sbmp_planner_LazyPRM",
      "sbmp_planner_LazyPRMStar", "sbmp_planner_SST", "sbmp_planner_RRT", "sbmp_planner_RRTStar", "sbmp_planner_LBTRRT",
      "sbmp_planner_TRRT", "sbmp_planner_LazyRRT", "sbmp_planner_cRRT", "sbmp_planner_RRTConnect", "sbmp_planner_EST",
      "sbmp_planner_SBL", "sbmp_planner_LBKPIECE1", "sbmp_planner_KPIECE1", "sbmp_planner_BKPIECE1",
      "sbmp_planner_STRIDE", "sbmp_planner_PDST", "sbmp_planner_FMT", "sbmp_planner_BFMT", "sbmp_planner_RRTsharp",
      "sbmp_planner_RRTXstatic", "sbmp_planner_InformedRRTstar", "sbmp_planner_BITstar"),
    "any_sbmp_sampler_type" -> List("sbmp_uniform_valid_state_sampler", "sbmp_obstacle_valid_state_sampler",
      "sbmp_gaussian_valid_state_sampler", "sbmp_max_clearance_valid_state_sampler", "sbmp_path_optimizer_sampler",
      "sbmp_path_optimizer_sampler", "sbmp_uniform_space_sampler", "sbmp_gaussian_space_sampler"),
    "any_sbmp_state_validator_type" -> List("sbmp_fcl_validator"),
    "any_sbmp_motion_validator_type" -> List("sbmp_fcl_motion_validator", "sbmp_discrete_motion_validator"),
    "any_sbmp_optimization_objective_type" -> List("sbmp_path_length", "sbmp_opt_integral"),
    "any_sbmp_cost_type" -> List("sbmp_default_cost_state", "sbmp_cost_state_change_weighted"),
    "any_sbmp_simplification_type" -> List("sbmp_use_simplification", "sbmp_no_simplification"),
    "any_dimensionality_type" -> List("dimensionality_two_d_t", "dimensionality_three_d_t", "dimensionality_n_d_t")
  )

  val sbmp_taxonomy = taxMapList.map { case ((k, l)) => l.foldLeft(Taxonomy(k))((t, s) => t.addSubtype(s)) }.
    reduce(_ merge _)

  //  val map = Map(
//    cmp_cell_decomposition_var -> Seq(
//      cmp_vertical_cell_decomposition_type, cmp_wavefront_cell_decomposition_type, cmp_robot_decomposition_type),
//    cmp_graph_shortest_path_var -> Seq(cmp_graph_dijkstra_type, cmp_graph_a_star_type, cmp_graph_vbi_type))
//  val kinding: Kinding = buildKinding(map)
}