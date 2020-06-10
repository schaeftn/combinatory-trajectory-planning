package org.combinators.ctp.repositories.dynrepository

object SbmpSamplers extends Enumeration {
  type EnumType = Value
  val
//  sbmp_roadmap_valid_state_sampler,
//  sbmp_milling_valid_state_sampler,
  sbmp_uniform_valid_state_sampler,
  sbmp_obstacle_valid_state_sampler,
  sbmp_gaussian_valid_state_sampler,
  sbmp_max_clearance_valid_state_sampler,
  sbmp_valid_path_optimizer_sampler,
  sbmp_path_optimizer_sampler,
  sbmp_uniform_space_sampler,
  sbmp_gaussian_space_sampler,
  not_specified
  = Value

  def isSpaceSampler(v: Value): Boolean =
    v == sbmp_path_optimizer_sampler ||
      v == sbmp_uniform_space_sampler ||
      v == sbmp_gaussian_space_sampler
}
