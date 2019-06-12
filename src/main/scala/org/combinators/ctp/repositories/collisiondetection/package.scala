package org.combinators.ctp.repositories

import org.combinators.cls.types.{Constructor, Type, Variable}
import scalaz._
import Scalaz._
import org.combinators.ctp.repositories.protocol.BoundingBoxData


package object collisiondetection {
  val twoSphereCollisionFunction: Type = Constructor("twoSphereCollisionFunction")
  val sphereBoxColFun: Type = Constructor("colfun", Constructor("box"), Constructor("sphere"))

  val v_colFunctions: Variable = new Variable("v_colFunctions")

  val hColFun = Constructor("hColFun")
  def tohColFun = (t:Type) => Constructor("hColFun", t)


/*
  v_colFunctions, v_colFunctions
*/

  // Was ist ein enumerative report



  //scalaz tree m-way tree (Octree, Quadtree, Binary Tree)
  // Für inserts etc treeloc -> Statisch

val tree: Tree[Int] =  1.node(
    2.leaf,
    3.node(
      4.leaf))

  tree.subForest.head

  val cd_report_var: Variable = new Variable("cd_report_var")

  val cd_report_boolean: Type = Constructor("cd_report_boolean")
  val cd_report_enumerative: Type = Constructor("cd_report_enumerative")
  val cd_report_approximate: Type = Constructor("cd_report_approximate")
  val cd_report_exact: Type = Constructor("cd_report_exact")


}
