package org.combinators.ctp.repositories.taxkinding

import org.combinators.ctp.repositories._
import org.combinators.cls.types.{Constructor, Kinding, Type, Variable}
import org.combinators.ctp.repositories.toplevel.{MqttCubeData, PolygonScene, Scene}

trait SceneDescription {
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

  val cmp_sceneSegFct_type = Constructor("sd_poly_scene_segmentation")
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

  val cmp_cell_graph_fct = Constructor("cmp_cell_graph_fct")
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
}