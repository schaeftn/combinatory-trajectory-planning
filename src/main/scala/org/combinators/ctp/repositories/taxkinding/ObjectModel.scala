package org.combinators.ctp.repositories.taxkinding

import org.combinators.ctp.repositories._
import org.combinators.cls.types.{Constructor, Kinding, Type, Variable}

trait ObjectModel {
  val var_om_dim = Variable("var_om_dim")
  val t_om_two_dim = Constructor("t_coverage_path")
  val t_om_n_dim = Constructor("t_clearance_path")

  def t_om_union(t: Type*) = Constructor("t_om_union", t:_*)
  def t_om_substraction(t1: Type, t2: Type) = Constructor("t_om_substraction", t1, t2)
  def t_intersection(t: Type*) = Constructor("t_om_intersection", t:_*)

  val var_om_primitive = Variable("var_om_primitive")
  val t_om_AABB = Constructor("t_om_AABB")
  val t_om_Cube = Constructor("t_om_Cube")
  val t_om_OBB = Constructor("t_om_OBB")
  val t_om_point = Constructor("t_om_point")
  val t_om_sphere = Constructor("t_om_sphere")

  val t_om_polygon = Constructor("t_om_polygon")
  val t_om_closedFigure = Constructor("t_om_closedFigure")
  val t_om_unity_mesh = Constructor("t_om_unity_mesh")
  val t_om_unity_mesh_distinct = Constructor("t_om_unity_mesh_distinct")
  val t_om_urdf_mesh = Constructor("t_om_polygon")

  val t_om_half_space = Constructor("t_om_half_space")

  val var_om_cforms = Variable("var_om_cforms ")
  val t_om_convex = Constructor("t_om_convex")
  val t_om_non_convex = Constructor("t_om_non_convex")

  val t_om_transformation = Constructor("t_om_transformation")
  val t_om_translation = Constructor("t_om_translation")
  val t_om_rotation = Constructor("t_om_rotation")
}
