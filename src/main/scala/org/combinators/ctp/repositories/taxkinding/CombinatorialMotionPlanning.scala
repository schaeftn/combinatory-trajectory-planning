package org.combinators.ctp.repositories.taxkinding

import org.combinators.cls.types.{Constructor, Kinding, Variable}
import org.combinators.ctp.repositories._

trait CombinatorialMotionPlanning {
  val cmp_decomposition_var = Variable("cmp_decomposition_var")
  val cmp_vertical_cell_decomposition_type = Constructor("cmp_vertical_cell_decomposition")
  val cmp_tetrahedralization_cd_type = Constructor("cmp_tetrahedralization_cd_type")
  val cmp_wavefront_cell_decomposition_type = Constructor("cmp_wavefront_cell_decomposition")
  val cmp_robot_decomposition_type = Constructor("cmp_robot_decomposition_type")
  val cmp_voroni_diagram_type = Constructor("cmp_voroni_diagram_type")

  val cmp_graph_shortest_path_var = Variable("cmp_graph_shortest_path_var")
  val cmp_graph_dijkstra_type = Constructor("cmp_graph_dijkstra_type")
  val cmp_graph_a_star_type = Constructor("cmp_graph_a_star_type")
  val cmp_graph_vbi_type = Constructor("cmp_graph_vbi_type") // value based iteration

  val cmp_path_opt_var = Variable("cmp_path_opt_var")
  val cmp_teb_type = Constructor("cmp_teb_type") // time elastic band planner

  val cmp_result_var = Variable("cmp_path_opt_var")
  val cmp_result_unity_type = Constructor("cmp_result_unity_type") // Display in Unity
  val cmp_result_file_type = Constructor("cmp_result_file_type") // Produce output file in obj or Image?

  val cmp_ac_decomposition_type = Constructor("cmp_ac_decomposition_type") // decomposition identifier
  val cmp_ac_scene_type = Constructor("cmp_ac_scene_type") //scene source type identifier
  val cmp_ac_spgs_type = Constructor("cmp_ac_spgs_type") // shortest path graph search identifier
  val cmp_ac_path_opt_type = Constructor("cmp_ac_path_opt_type") // shortest path graph search identifier
  val cmp_ac_result_type = Constructor("cmp_ac_result_type") // output data format identifier

  val cmp_combinatorial_type = Constructor("cmp_combinatorial_type")
  val cmp_sampling_type = Constructor("cmp_sampling_type")
  val cmp_config_space_type = Constructor("cmp_config_space_type")

  val cmp_cell_graph = Constructor("cmp_cell_graph") // Decomposition result as cell graph
  val cmp_scene_graph = Constructor("cmp_scene_graph") // Decomposition result as scene graph (contains scene and graph)
  val cmp_scene_graph_path = Constructor("cmp_scene_graph_path") // Decomposition result as scene graph (contains scene, graph and path)
  val cmp_scene_graph_pathf = Constructor("cmp_scene_graph_pathf") // Decomposition result as scene graph (contains scene, graph and path)
  val cmp_cfree_polygons = Constructor("cmp_cfree_polygons") // Decomposition result as cfree polygons
  val cmp_cd_lines = Constructor("cmp_cd_lines") // Decomposition result as lines
  val cmp_cd_cells = Constructor("cmp_cd_cells") // Decomposition result as cells

  val dimensionality_var = Variable("dimensionality_var")
  val dimensionality_two_d_t = Constructor("dimensionality_two_d_t")
  val dimensionality_three_d_t = Constructor("dimensionality_three_d_t")
  val dimensionality_n_d_t = Constructor("dimensionality_n_d_t")

  val map = Map(
    cmp_decomposition_var -> Seq(
      cmp_vertical_cell_decomposition_type, cmp_wavefront_cell_decomposition_type, cmp_robot_decomposition_type),
    cmp_graph_shortest_path_var ->
      Seq(cmp_graph_dijkstra_type, cmp_graph_a_star_type, cmp_graph_vbi_type),
    dimensionality_var ->
      Seq(dimensionality_two_d_t, dimensionality_three_d_t, dimensionality_n_d_t))
  val kinding: Kinding = buildKinding(map)
}