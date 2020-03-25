package org.combinators.ctp.repositories.taxkinding

import org.combinators.ctp.repositories._
import org.combinators.cls.types.{Constructor, Kinding, Type, Variable}
import org.combinators.ctp.repositories.scene.{MqttCubeData, PolygonScene, Scene, SceneSegmentation3d}

trait SceneDescription {
  val cmp_scene_source_var = Variable("cmp_scene_source_var")
val cmp_scene_triangulation_parameters = Variable("cmp_scene_triangulation_parameters")
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
  val sd_poly_scene_segmentation = Constructor("sd_poly_scene_segmentation")
  val sd_seg_lines = Constructor("sd_seg_lines")
  val sd_seg_cells = Constructor("sd_seg_cells")
  val sd_seg_triangles_simple = Constructor("sd_seg_triangles_simple")
  val sd_seg_triangles_para = Constructor("sd_seg_triangles_para")
  val sd_seg_centroid_cells = Constructor("sd_seg_centroid_cells")

  type scene_2d_boundaries_n = ((Int, Int), (Int, Int))
  type scene_3d_boundaries_n = List[Int]
  type scene_type_2d_n = Scene
  type scene_type_3d_n = Scene
  type scene_type_2d_poly_n = PolygonScene
  type scene_segmentation_type_3d_n = SceneSegmentation3d
  type scene_cube_2d_n = MqttCubeData
  type scene_cube_3d_n = MqttCubeData
}