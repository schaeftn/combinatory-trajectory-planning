package org.combinators.ctp.repositories.taxkinding

import org.combinators.cls.types.{Constructor, Type, Variable}

//extends geometry
//extends dimensionality
trait RobotModel {
  val r_robotform_type = Constructor("r_robotform_type")

  val r_translation_type = Variable("r_translation_type")
  val r_rotation_type = Variable("r_rotation_type")
  val r_steering_model_type = Variable("r_steering_model_type")
  val r_physics_model_type = Variable("r_physics_model_type")

  val r_pm_inertia_type = Variable("r_physics_model_type")
  val r_pm_friction_type = Variable("r_physics_model_type")
  val r_pm_acceleration_type = Variable("r_physics_model_type")
  val r_pm_velocity_bounds_type = Variable("r_physics_model_type")
  val r_pm_max_curvature_type = Variable("r_physics_model_type")

  // TODO sensors, reactive,
}
