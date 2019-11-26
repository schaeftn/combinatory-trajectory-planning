package org.combinators.ctp.repositories.taxkinding

import org.combinators.cls.types.{Constructor, Kinding, Type, Variable}
import org.combinators.ctp.repositories._

trait MpTask {
  val mpt_shortest_path_type: Type = Constructor("mpt_shortest_path_type")
  val mpt_coverage_path_type: Type = Constructor("mpt_coverage_path_type")
  val mpt_min_cost_type: Type = Constructor("mpt_min_cost_type")
  val mpt_target_config_type: Type = Constructor("mpt_target_config_type")

  val mpt_start_goal_position_type: Type = Constructor("mpt_start_goal_position_type")
  val mpt_start_goal_pose_type: Type = Constructor("mpt_start_goal_pose_type")

  val mpt_anytime_type: Type = Constructor("mpt_anytime_type")

  //TODO Persuador oder Evader
}
