package org.combinators.ctp

import org.combinators.cls.types.{Kinding, Type, Variable}
import org.combinators.ctp.repositories.mptasks.MpTasksRepository
import org.combinators.ctp.repositories.taxkinding.{CombinatorialMotionPlanning, CtpTaxonomy, GeometricModelTypes, MpTask, Protocol, RobotModel, SampleBasedMotionPlanning, SceneDescription}

package object repositories extends CombinatorialMotionPlanning
  with CtpTaxonomy with GeometricModelTypes with MpTask with RobotModel
  with SampleBasedMotionPlanning with SceneDescription with Protocol{
  def buildKinding(m: Map[Variable, Seq[Type]]): Kinding =
    (m map (a => a._2.foldLeft(Kinding(a._1))((a, b) => a.addOption(b)))).reduce((k1, k2) => k1.merge(k2))

  val kindingMap = Map.empty[Variable, Seq[Type]]
  override val kinding =Kinding.empty //buildKinding(kindingMap)
}
