package org.combinators.ctp.repositories.taxkinding

import org.combinators.ctp.repositories._
import org.combinators.cls.types.{Constructor, Kinding, Type, Variable}
import org.combinators.cls.types.Type
import org.combinators.cls.types.syntax._

trait GeometricModelTypes {
  val gm_dim = Variable("var_gm_dim")

  def gm_union(t: Type*) = Constructor("gm_union", t:_*)
  def gm_substraction(t1: Type, t2: Type) = Constructor("gm_substraction", t1, t2)
  def t_intersection(t: Type*) = Constructor("gm_intersection", t:_*)

  //  semantic type for list with procedural geometries
  val gm_procedural_type = Constructor("gm_procedural_type")

  //  semantic type for a single procedural geometry
  val var_gm_procedural = Variable("var_gm_procedural")
  val gm_AABB = Constructor("gm_AABB")
  val gm_rectangular = Constructor("gm_rectangular")
  val gm_OBB = Constructor("gm_OBB")
  val gm_point = Constructor("gm_point")
  val gm_sphere  = Constructor("gm_sphere")
  val gm_circle = Constructor("circle")

  val gm_polygon = Constructor("gm_polygon")
  val gm_closedFigure = Constructor("gm_closedFigure")
  val gm_unity_mesh = Constructor("gm_unity_mesh")
  val gm_unity_mesh_distinct = Constructor("gm_unity_mesh_distinct")
  val gm_urdf_mesh = Constructor("gm_urdf_mesh")

  val gm_half_space = Constructor("gm_half_space")

  val var_gm_cforms = Variable("var_gm_cforms ")
  val gm_convex = Constructor("gm_convex")
  val gm_non_convex = Constructor("gm_non_convex")

  val gm_AffTransformVertexFunction = Constructor("gm_AffTransformVertexFunction")
  val gm_AffTransformVertexListFunction = Constructor("gm_AffTransformVertexListFunction")
  val gm_translation = Constructor("gm_translation")
  val gm_rotation = Constructor("gm_rotation")

  val gm_aaBbGenFct = Constructor("gm_aaBbGenFct")
  val gm_CubeToPoly = Constructor("gm_CubeToPoly")

  val gf_centroid_selection = Constructor("gf_centroid_selection")
  // function applies affine transform matrix to vertex
  // function applies affine transform matrix to vertexList

  type vertexType = List[Float]
  type glNativeType = Vector[Float]
  type unityNativeType = (Seq[Int],vertexArrayType)

  type sphereRadius = Float
  type sphereData = (vertexType,Float)
  type boxData = (vertexType,vertexType)

  type intListType = List[Int]
  type vertexArrayType = List[Float]

  type vertexArrayType2 = Array[Array[Float]]
  type facesArrayType = Array[Array[Int]]
  type voxelArrayType = Array[Array[Int]]

  type vertexPairType = (vertexType, vertexType)

  type tMatrixType = List[List[Float]]

  trait CellProperties{
    val cp_convexCells: Type = 'convexCells
    val cp_cellsWithHoles: Type = 'cellsWHoles
    val cp_cellsWithoutHoles: Type = 'cellsWithoutHoles
    val simplicialComplex: Type = 'simplicialComplex
  }
}

