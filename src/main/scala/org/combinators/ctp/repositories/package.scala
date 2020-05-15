package org.combinators.ctp

import org.combinators.cls.types.{Kinding, Type, Variable}
import org.combinators.ctp.repositories.taxkinding.{CombinatorialMotionPlanning, CtpTaxonomy, GeometricModelTypes, MpTask, Protocol, RobotModel, SbmpSemanticTypes, SceneDescription}

package object repositories extends CombinatorialMotionPlanning
  with CtpTaxonomy with GeometricModelTypes with MpTask with RobotModel
  with SbmpSemanticTypes with SceneDescription with Protocol {
  def buildKinding(m: Map[Variable, Seq[Type]]): Kinding =
    (m map (a => a._2.foldLeft(Kinding(a._1))((a, b) => a.addOption(b)))).reduce((k1, k2) => k1.merge(k2))

  def overwriteDefaultKindingmap(defaultKindingMap: Map[Variable, Seq[Type]], m: Map[Variable, Seq[Type]],
                                ): Map[Variable, Seq[Type]] = {
    defaultKindingMap.foreach(println)
    println("nxt..")
    (defaultKindingMap++m).foreach(println)
    defaultKindingMap ++ m
  }
}
