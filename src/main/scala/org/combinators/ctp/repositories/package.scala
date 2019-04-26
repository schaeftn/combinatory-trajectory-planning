package org.combinators.ctp

import org.combinators.cls.types.{Kinding, Type, Variable}
import org.combinators.ctp.repositories.mptasks.MpTasksRepository

package object repositories extends MpTasksRepository{
  val v_mptask = Variable("v_mpt")

  val map = Map(v_mptask -> Seq(mpt_sp_type, mpt_pc_type, mpt_mc_type))

  def buildKinding(m: Map[Variable, Seq[Type]]) =
    m map (a => a._2.foldLeft(Kinding(a._1))((a, b) => a.addOption(b)))

  lazy val kinding = buildKinding(map)
}
