package org.combinators.ctp.repositories.dynrepository

import org.combinators.cls.types.{Constructor, Type}
import org.combinators.ctp.repositories.taxkinding.SbmpSemanticTypes

case class GeometricPlannerMetaData(usesSpaceSampling: Boolean,
                                    usesValidStateSampling: Boolean,
                                    usesInformedSampler: Boolean,
                                    usesOptObjective: Boolean)

object SbmpPlanners extends Enumeration {
  type EnumType = Value
  val sbmp_planner_PRM, // uses valid state sampler, space sampling as fallback, invokes sample()
  sbmp_planner_PRMStar, // see PRM
  sbmp_planner_LazyPRM, // uses space sampling, invokes sampleUniform()
  sbmp_planner_LazyPRMStar, //see LazyPRM
  sbmp_planner_SST, // uses space sampling, invokes sampleUniform(), uses si motion validator
  sbmp_planner_RRT, // uses space sampling, invokes sampleUniform()
  sbmp_planner_RRTStar, // uses space sampling, invokes sampleUniform(), Informed Sampler, RejectionInfSampler, OrderedInfSampler, invokes sampleUniform)=
  sbmp_planner_LBTRRT, // uses space sampling, invokes sampleUniform()
  sbmp_planner_TRRT, // uses space sampling, invokes sampleUniform()
  sbmp_planner_LazyRRT, // uses space sampling, invokes sampleUniform()
  sbmp_planner_RRTConnect, // uses space sampling, invokes sampleUniform()
  sbmp_planner_EST, // uses valid state sampler, invokes sample() and sampleNear()
  sbmp_planner_SBL, // uses valid state sampler, invokes sample() and sampleNear()
  sbmp_planner_LBKPIECE1, // uses space sampling, invokes sampleNear(), discretization motion
  sbmp_planner_KPIECE1, // uses space sampling, invokes sampleNear(), discretization motion
  sbmp_planner_BKPIECE1, // uses valid state sampling, invokes sampleNear(), discretization motion
  sbmp_planner_STRIDE, // uses valid state sampling, invokes sampleNear(), tree motion (GNAT)
  sbmp_planner_PDST, // uses space sampling, invokes sampleUniform(), GoalSampleableRegion sampleGoal()
  sbmp_planner_FMT, // uses space sampling, invokes sampleUniform(), uses si state validator
  sbmp_planner_BFMT, // uses space sampling, invokes sampleUniform(), NearestNeighborsGNAT, BinaryHeap
  sbmp_planner_RRTsharp, // see RRTXstatic
  sbmp_planner_RRTXstatic, // uses space sampling, invokes sampleUniform(), GoalSampleableRegion sampleGoal(), informed sampler
  sbmp_planner_InformedRRTstar, // see RRTStar
  sbmp_planner_BITstar, // uses space sampling, invokes sampleUniform() (in InplicitGraph data structure), uses spaceinformation.isValid
  not_specified
  = Value

  def printPlannerTypes(): Unit = {
    val sTypes = new SbmpSemanticTypes {}
    plannerInfo.foreach {
      case (pType, b) =>
        val optType = if (b.usesOptObjective) Some(sTypes.any_sbmp_optimization_objective_type) else Some(sTypes.sbmp_opt_path_length)
        val spaceType = if (b.usesSpaceSampling) Some(sTypes.sbmp_spaceSampler_type) else None
        val validType = if (b.usesValidStateSampling) Some(sTypes.sbmp_validStateSampler_type) else None
        val informedType = if (b.usesSpaceSampling) Some(sTypes.sbmp_informedSampler_type) else None
        val asd: List[Option[Type]] = List(spaceType, validType, informedType)
        val lst = asd.filter(_.isDefined).map(_.get).map(b => "(" + optType.get + "=>:" + b + "=>:" + pType + ")")
        val lst2 = lst.reduce(_ + ":&: \n" + _)
        println(lst2)
      case _ => println("printPlannerTypes: planner info mismatch.")
    }
  }


  val plannerInfo: Map[Value, GeometricPlannerMetaData] = Map(
    sbmp_planner_PRM -> GeometricPlannerMetaData(usesSpaceSampling = true, usesValidStateSampling = true,
      usesInformedSampler = false, usesOptObjective = true),
    sbmp_planner_PRMStar -> GeometricPlannerMetaData(usesSpaceSampling = true, usesValidStateSampling = true,
      usesInformedSampler = false, usesOptObjective = true),
    sbmp_planner_LazyPRM -> GeometricPlannerMetaData(usesSpaceSampling = true, usesValidStateSampling = false,
      usesInformedSampler = false, usesOptObjective = true),
    sbmp_planner_LazyPRMStar -> GeometricPlannerMetaData(usesSpaceSampling = true, usesValidStateSampling = false,
      usesInformedSampler = false, usesOptObjective = true),
    sbmp_planner_SST -> GeometricPlannerMetaData(usesSpaceSampling = true, usesValidStateSampling = false,
      usesInformedSampler = false, usesOptObjective = true),
    sbmp_planner_RRT -> GeometricPlannerMetaData(usesSpaceSampling = true, usesValidStateSampling = false,
      usesInformedSampler = false, usesOptObjective = false),
    sbmp_planner_RRTStar -> GeometricPlannerMetaData(usesSpaceSampling = true, usesValidStateSampling = false,
      usesInformedSampler = true, usesOptObjective = true),
    sbmp_planner_LBTRRT -> GeometricPlannerMetaData(usesSpaceSampling = true, usesValidStateSampling = false,
      usesInformedSampler = false, usesOptObjective = false),
    sbmp_planner_TRRT -> GeometricPlannerMetaData(usesSpaceSampling = true, usesValidStateSampling = false,
      usesInformedSampler = false, usesOptObjective = true),
    sbmp_planner_LazyRRT -> GeometricPlannerMetaData(usesSpaceSampling = true, usesValidStateSampling = false,
      usesInformedSampler = false, usesOptObjective = false),
    sbmp_planner_RRTConnect -> GeometricPlannerMetaData(usesSpaceSampling = true, usesValidStateSampling = false,
      usesInformedSampler = false, usesOptObjective = false),
    sbmp_planner_EST -> GeometricPlannerMetaData(usesSpaceSampling = false, usesValidStateSampling = true,
      usesInformedSampler = false, usesOptObjective = false),
    sbmp_planner_SBL -> GeometricPlannerMetaData(usesSpaceSampling = false, usesValidStateSampling = true,
      usesInformedSampler = false, usesOptObjective = false),
    sbmp_planner_LBKPIECE1 -> GeometricPlannerMetaData(usesSpaceSampling = true, usesValidStateSampling = false,
      usesInformedSampler = false, usesOptObjective = false),
    sbmp_planner_KPIECE1 -> GeometricPlannerMetaData(usesSpaceSampling = true, usesValidStateSampling = false,
      usesInformedSampler = false, usesOptObjective = false),
    sbmp_planner_BKPIECE1 -> GeometricPlannerMetaData(usesSpaceSampling = true, usesValidStateSampling = false,
      usesInformedSampler = false, usesOptObjective = false),
    sbmp_planner_STRIDE -> GeometricPlannerMetaData(usesSpaceSampling = false, usesValidStateSampling = true,
      usesInformedSampler = false, usesOptObjective = false),
    sbmp_planner_PDST -> GeometricPlannerMetaData(usesSpaceSampling = true, usesValidStateSampling = false,
      usesInformedSampler = false, usesOptObjective = false),
    sbmp_planner_FMT -> GeometricPlannerMetaData(usesSpaceSampling = true, usesValidStateSampling = false,
      usesInformedSampler = false, usesOptObjective = true),
    sbmp_planner_BFMT -> GeometricPlannerMetaData(usesSpaceSampling = true, usesValidStateSampling = false,
      usesInformedSampler = false, usesOptObjective = true),
    sbmp_planner_RRTsharp -> GeometricPlannerMetaData(usesSpaceSampling = true, usesValidStateSampling = false,
      usesInformedSampler = true, usesOptObjective = true),
    sbmp_planner_RRTXstatic -> GeometricPlannerMetaData(usesSpaceSampling = true, usesValidStateSampling = false,
      usesInformedSampler = true, usesOptObjective = true),
    sbmp_planner_InformedRRTstar -> GeometricPlannerMetaData(usesSpaceSampling = true, usesValidStateSampling = false,
      usesInformedSampler = true, usesOptObjective = true),
    sbmp_planner_BITstar -> GeometricPlannerMetaData(usesSpaceSampling = true, usesValidStateSampling = false,
      usesInformedSampler = true, usesOptObjective = true),
  )
}
