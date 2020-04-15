package org.combinators.ctp.repositories.taxkinding

import org.combinators.cls.types.{Constructor, Kinding, Variable}
import org.combinators.ctp.repositories._
import scalax.collection.edge.WUnDiEdge

trait CombinatorialMotionPlanning {
  val cmp_algorithm_type = Constructor("cmp_algorithm_type")

  val cmp_graph_algorithm_var = Variable("cmp_graph_algorithm_var")
  val cmp_graph_dijkstra_type = Constructor("cmp_graph_dijkstra_type")
  val cmp_graph_a_star_type = Constructor("cmp_graph_a_star_type")
  val cmp_graph_vbi_type = Constructor("cmp_graph_vbi_type") // value based iteration
  val cmp_graph_mst_type = Constructor("cmp_graph_mst_type")
  val cmp_graph_tsp_type = Constructor("cmp_graph_tsp_type")

  val cmp_path_opt_var = Variable("cmp_path_opt_var")
  val cmp_teb_type = Constructor("cmp_teb_type") // time elastic band planner

  val cmp_result_var = Variable("cmp_path_opt_var")
  val cmp_result_unity_type = Constructor("cmp_result_unity_type") // Display in Unity
  val cmp_result_file_type = Constructor("cmp_result_file_type") // Produce output file in obj or Image?

  val cmp_combinatorial_type = Constructor("cmp_combinatorial_type")
  val cmp_sampling_type = Constructor("cmp_sampling_type")
  val cmp_config_space_type = Constructor("cmp_config_space_type")

  val cmp_cell_graph = Constructor("cmp_cell_graph") // Decomposition result as cell graph
  val cmp_scene_graph = Constructor("cmp_scene_graph") // Decomposition result as scene graph (contains scene and graph)
  val cmp_scene_graph_path = Constructor("cmp_scene_graph_path") // Decomposition result as scene graph (contains scene, graph and path)
  val cmp_path_only = Constructor("cmp_path_only") // List of vertices, describes path
  val cmp_scene_graph_pathf = Constructor("cmp_scene_graph_pathf") // Decomposition result as scene graph (contains scene, graph and path)
  val cmp_cfree_polygons = Constructor("cmp_cfree_polygons") // Decomposition result as cfree polygons
  val cmp_cd_lines = Constructor("cmp_cd_lines") // Decomposition result as lines
  val cmp_cd_cells = Constructor("cmp_cd_cells") // Decomposition result as cells
  val cmp_graphBuildFct_type = Constructor("cmp_graphBuildFct_type")

  val cmd_centroidFct_type = Constructor("triangle_centroidsFct_type")
  val rmc_centroidFct_var = Variable("cell_centroidFct_var")
  val cFct_centroids_naive_type = Constructor("cFct_centroids_naive_type")
  val cFct_jts_default_type = Constructor("cFct_jts_default_type")
  val cFct_jts_incentre_type = Constructor("cFct_jts_incentre_type")
  val cFct_avg_type = Constructor("cFct_avg_type")
  val triangle_centroidsFctNd_type = Constructor("triangle_centroidsFctNd_type")

  val rmc_usingCentroids_var = Variable("cmd_centroidFeature_var")
  val rm_withCentroids_type = Constructor("rm_withCentroids_type")
  val rm_withoutCentroids_type = Constructor("rm_withoutCentroids_type")


  val triangle_gbuildNd_type = Constructor("triangle_gbuildNd_type")
  val triangle_gbuildNdFast_type = Constructor("triangle_gbuildNdFast_type")
  val triangle_gRefine_type = Constructor("triangle_gRefine_type")

  val triangulation_path_prog_type = Constructor("triangulation_path_prog_type")

  val cmp_sd_tetrahedra_type = Constructor("cmp_sd_tetrahedra_type")

  val sd_cell_type_var = Variable("sd_cell_type_var")
  val sd_cell_triangle_type = Constructor("sd_cell_triangle_type")
  val sd_cell_vertical_type = Constructor("sd_cell_vertical_type")


  val dimensionality_var = Variable("dimensionality_var")
  val dimensionality_two_d_t = Constructor("dimensionality_two_d_t")
  val dimensionality_three_d_t = Constructor("dimensionality_three_d_t")
  val dimensionality_n_d_t = Constructor("dimensionality_n_d_t")

  val rmc_initFct_type = Constructor("rmc_initFct_type")
 // val rmc_connectorAddFct_type = Constructor("rmc_connectorAddFct_type")
  val rmc_startGoalAddFct_type = Constructor("rmc_startGoalAddFct_type")
  val rmc_startGoalFct_type = Constructor("rmc_startGoalFct_type")

  val rmc_edgeAdd_type = Constructor("rmc_edgeAdd_type")
  val rmc_cellGraph_var = Variable("rmc_cellGraph_var")
  val rmc_cg_centroidsOnly = Constructor("rmc_cg_centroidsOnly")
  val rmc_cg_allVertices = Constructor("rmc_cg_allVertices")
  val rmc_cg_centroidCellVertices = Constructor("rmc_cg_centroidCellVertices")

  val rmc_cellNodeAddFct_type = Constructor("rmc_cellNodeAddFct_type")
  val rmc_cellNodeAddFct_var = Variable("rmc_cellNodeAddFct_var")
  val rmc_cna_withCellNodes_type = Constructor("rmc_cna_withCellNodes_type")
  val rmc_cna_withoutCellNodes_type = Constructor("rmc_cna_withoutCellNodes_type")

  val rmc_connectorNodeFct_type = Constructor("rmc_connectorNodeFct_type")
  val rmc_connectorNodes_var = Variable("rmc_connectorNodes_var")
  val rmc_cn_withConnectorNodes = Constructor("rmc_cn_withConnectorNodes")
  val rmc_cn_withoutConnectorNodes = Constructor("rmc_cn_withoutConnectorNodes")

  val rmc_startGoalFct_var = Variable("rmc_startGoalFct_var")
  val rmc_startGoal_cellbased_type = Constructor("rmc_startGoal_cellbased_type")
  val rmc_startGoal_nn_type = Constructor("rmc_startGoal_nn_type")

  //  val map = Map(
//    cmp_decomposition_var -> Seq(
//      cmp_vertical_cell_decomposition_type, cmp_wavefront_cell_decomposition_type, cmp_robot_decomposition_type),
////    cmp_graph_shortest_path_var ->
////      Seq(cmp_graph_dijkstra_type, cmp_graph_a_star_type, cmp_graph_vbi_type),
//    dimensionality_var ->
//      Seq(dimensionality_two_d_t, dimensionality_three_d_t, dimensionality_n_d_t),
//    cmp_scene_triangulation_parameters ->
//      Seq(sd_seg_triangles_para, sd_seg_triangles_simple))

  type roadmapRedefineNativeType = (List[List[Float]], Map[Int, (List[List[Float]], List[WUnDiEdge[List[Float]]])])
  type neighbourCellsNativeType = IndexedSeq[(List[Int], Int, Int)] // Common vertices, id1, id2
  trait RmAuxDataNodes {
    self =>
    val cellId: Int
    val centroid: List[Float]
    val vertices: List[List[Float]]
    val connxPoints: List[List[Float]]
  }
}