package org.combinators.ctp.repositories.taxkinding

import org.combinators.cls.types.{Constructor, Kinding, Taxonomy, Variable}
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.toplevel.{MqttCubeData, PolygonScene, Scene}
import scalax.collection.edge.WUnDiEdge

trait CtpSemanticTypes {
  val cmp_any_algorithm_type = Constructor("cmp_any_algorithm_type")
  val cmp_algorithm_type = Constructor("cmp_algorithm_type")

  val cmp_any_graph_algorithm_type = Constructor("cmp_any_graph_algorithm_type")
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

//  val cmp_cell_graph = Constructor("cmp_cell_graph") // Decomposition result as cell graph
//  val cmp_scene_graph = Constructor("cmp_scene_graph") // Decomposition result as scene graph (contains scene and graph)
  val cmp_scene_graph_path = Constructor("cmp_scene_graph_path") // Decomposition result as scene graph (contains scene, graph and path)
  val cmp_path_only = Constructor("cmp_path_only") // List of vertices, describes path
  val cmp_scene_graph_pathf = Constructor("cmp_scene_graph_pathf") // Decomposition result as scene graph (contains scene, graph and path)
  val cmp_cfree_polygons = Constructor("cmp_cfree_polygons") // Decomposition result as cfree polygons
  val cmp_cd_lines = Constructor("cmp_cd_lines") // Decomposition result as lines
  val cmp_cd_cells = Constructor("cmp_cd_cells") // Decomposition result as cells
  val cmp_graphBuildFct_type = Constructor("cmp_graphBuildFct_type")

  val rmc_neighbourFct_type = Constructor("rmc_neighbourFct_type")

  val cmd_centroidFct_type = Constructor("cmd_centroidFct_type")
  val rmc_any_centroidFct_type = Constructor("rmc_any_centroidFct_type")
  val rmc_centroidFct_var = Variable("cell_centroidFct_var")
  val cFct_centroids_naive_type = Constructor("cFct_centroids_naive_type")
  val cFct_jts_default_type = Constructor("cFct_jts_default_type")
  val cFct_jts_incentre_type = Constructor("cFct_jts_incentre_type")
  val cFct_avg_type = Constructor("cFct_avg_type")
  val cFct_triangle_centroidsNd_type = Constructor("triangle_centroidsFctNd_type")


  val rmc_any_usingCentroids_type = Constructor("rmc_any_usingCentroids_type")
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

  val any_dimensionality_type = Constructor("any_dimensionality_type")
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


  val cmp_scene_source_var = Variable("cmp_scene_source_var")
  val sd_source_unity_type = Constructor("sd_source_unity_type")
  val sd_source_string_type = Constructor("sd_source_string_type")
  val sd_source_native_scala_type = Constructor("sd_source_native_scala_type")

  val sd_scene_boundaries_type = Constructor("sd_scene_boundaries_type")
  val sd_scene_cost_model_type = Constructor("sd_scene_cost_model_type")

  val sd_scene_descripton_obstacles = Constructor("sd_obstacles")

  val sd_polygon_scene_type = Constructor("sd_polygon_scene_type")
  val sd_unity_scene_type = Constructor("sd_unity_scene_type")
  val sd_unity_scene_srt_type = Constructor("sd_unity_scene_srt_type")
  val sd_scene_segmentation = Constructor("sd_scene_segmentation")


  val cmp_any_sceneSegFct_type = Constructor("cmp_any_sceneSegFct_type")
  val cmp_sceneSegFct_type = Constructor("cmp_sceneSegFct_type")
  val sd_poly_scene_cell_segmentation_var = Variable("sd_poly_scene_cell_segmentation_var")
  val sd_vertical_cell_decomposition_type = Constructor("sd_vertical_cell_decomposition_type")
  val sd_seg_triangles_simple_type = Constructor("sd_seg_triangles_simple_type")
  val sd_seg_triangles_para_type = Constructor("sd_seg_triangles_para_type")
  val sd_seg_grid_type = Constructor("sd_seg_grid_type")

  val util_file_list_type = Constructor("util_file_list_type")
  val util_file_reader_type = Constructor("util_file_reader_type")
  /*  val cmp_vertical_cell_decomposition_type = Constructor("cmp_vertical_cell_decomposition")
    val cmp_tetrahedralization_cd_type = Constructor("cmp_tetrahedralization_cd_type")
    val cmp_wavefront_cell_decomposition_type = Constructor("cmp_wavefront_cell_decomposition")
    val cmp_robot_decomposition_type = Constructor("cmp_robot_decomposition_type")
    val cmp_voroni_diagram_type = Constructor("cmp_voroni_diagram_type")
    val cmp_visibility_graph_type = Constructor("cmp_visibility_graph_type")
    */

  val cmd_obstacleSceneBoundingCutFct_type = Constructor("cmd_obstacleSceneBoundingCutFct_type")

  val cmp_any_cell_graph_fct_type = Constructor("cmp_any_cell_graph_fct_type")
  val cmp_cell_graph_fct_type = Constructor("cmp_cell_graph_fct_type")
  //  val sd_seg_lines = Constructor("sd_seg_lines")
  //  val sd_seg_cells = Constructor("sd_seg_cells")
  //  val sd_seg_centroid_cells = Constructor("sd_seg_centroid_cells")

  type scene_2d_boundaries_n = ((Int, Int), (Int, Int))
  type scene_3d_boundaries_n = List[Int]
  type scene_type_2d_n = Scene
  type scene_type_3d_n = Scene
  type scene_type_2d_poly_n = PolygonScene
  type scene_cube_2d_n = MqttCubeData
  type scene_cube_3d_n = MqttCubeData

  type roadmapRedefineNativeType = (List[List[Float]], Map[Int, (List[List[Float]], List[WUnDiEdge[List[Float]]])])
  type neighbourCellsNativeType = IndexedSeq[(List[Int], Int, Int)] // Common vertices, id1, id2
  trait RmAuxDataNodes {
    self =>
    val cellId: Int
    val centroid: List[Float]
    val vertices: List[List[Float]]
    val connxPoints: List[List[Float]]
  }


  val taxMapListCmp = Map(
    "cmp_any_graph_algorithm_type" -> List("cmp_graph_dijkstra_type", "cmp_graph_a_star_type", "cmp_graph_tsp_type",
      "cmp_graph_mst_type"))

  val cmp_taxonomy = taxMapListCmp.map { case ((k, l)) => l.foldLeft(Taxonomy(k))((t, s) => t.addSubtype(s)) }.
    reduce(_ merge _)

}