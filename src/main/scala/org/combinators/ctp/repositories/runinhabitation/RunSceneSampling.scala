package org.combinators.ctp.repositories.runinhabitation

import akka.Done
import akka.stream.alpakka.mqtt.MqttMessage
import akka.stream.scaladsl.Sink
import org.combinators.cls.interpreter.{InhabitationResult, ReflectedRepository}
import org.combinators.cls.types.{Type, Variable}
import org.combinators.ctp.repositories.cmp.CmpPythonRepository
import org.combinators.ctp.repositories.graphsearch.GraphSearchRepository
import org.combinators.ctp.repositories.samplebased.SbmpTopLevelRepository
import org.combinators.ctp.repositories.scene.SceneRepository
import org.combinators.ctp.repositories.taxkinding.CombinatorialMotionPlanning
import org.combinators.ctp.repositories.toplevel.{CmpTopLevel, FileBasedTopLevelSbmp, ProblemDefinitionFiles}
import org.combinators.ctp.repositories._
import org.combinators.cls.types.syntax._

import scala.concurrent.Future

object RunSceneSampling extends App {
  lazy val repository = new SceneRepository with CmpTopLevel with FileBasedTopLevelSbmp with CmpPythonRepository
    with GraphSearchRepository with SbmpTopLevelRepository {}
  lazy val cmpRepository = new CombinatorialMotionPlanning {}

  val sbmpKindingMap = Map(sbmp_planner_var -> Seq(sbmp_planner_BKPIECE1),
    sbmp_sampler_var -> Seq(sbmp_uniform_valid_state_sampler),
    sbmp_state_validator_var -> Seq(sbmp_fcl_validator),
    sbmp_motion_validator_var -> Seq(sbmp_discrete_motion_validator),
    sbmp_cost_var -> Seq(sbmp_default_cost_state),
    sbmp_optimization_objective_var -> Seq(sbmp_opt_path_length),
    dimensionality_var -> Seq(dimensionality_three_d_t)
  )
  val kindingMap = repository.cmpDefaultKindingMap ++ repository.sbmpDefaultKindingMap ++ sbmpKindingMap
  val sbmpKinding = buildKinding(kindingMap)

  lazy val Gamma = ReflectedRepository(repository, substitutionSpace = sbmpKinding)


  println(s"# of allowed substitutions: ${Gamma.substitutionSpace.allowedSubstitutions.values.size}")

  println(Gamma.substitutionSpace.allowedSubstitutions.values)

  println("kinding: " + Gamma.substitutionSpace.toString)
  println("Reflected Repository built, starting inhabitation")
  println(s"Combinators ${Gamma.combinators.size}")

  def getTypeFromMap(v: Variable): Type = {
    val typeList = kindingMap(v)
    if (typeList.size != 1)
      println(s"Typesize for $v is not 1: $typeList")
    typeList.head
  }

  val ihBatch = Gamma.InhabitationBatchJob[ProblemDefinitionFiles => List[List[Float]]](
    sbmp_planning_algorithm :&:
      getTypeFromMap(sbmp_planner_var) :&:
      getTypeFromMap(sbmp_sampler_var) :&:
      getTypeFromMap(sbmp_state_validator_var) :&:
      getTypeFromMap(sbmp_motion_validator_var) :&:
      getTypeFromMap(sbmp_optimization_objective_var) :&:
      getTypeFromMap(sbmp_cost_var))
    .addJob[Sink[MqttMessage, Future[Done]]](p_mqttAkkaSink_type :&: cmp_path_only :&: getTypeFromMap(dimensionality_var))
    .addJob[Unit](
      p_fileToAkka_type :&: cmp_path_only :&:
        getTypeFromMap(dimensionality_var) :&:
        getTypeFromMap(sbmp_planner_var) :&:
        getTypeFromMap(sbmp_sampler_var) :&:
        getTypeFromMap(sbmp_state_validator_var) :&:
        getTypeFromMap(sbmp_motion_validator_var) :&:
        getTypeFromMap(sbmp_optimization_objective_var) :&:
        getTypeFromMap(sbmp_cost_var))
  /*   [PlannerScheme[SceneSRT, List[List[Float]]]](sbmp_planner_PRM)
     .addJob[SubstitutionScheme](sbmp_uniform_valid_state_sampler)
     .addJob[SubstitutionScheme](sbmp_fcl_validator)
     .addJob[SubstitutionScheme](sbmp_fcl_motion_validator)
     .addJob[SubstitutionScheme](sbmp_default_cost_state :&: sbmp_opt_path_length)
     .addJob[(SceneSRT, MpTaskStartGoal) => SubstitutionScheme](sbmp_input_data )
     .addJob[(SceneSRT, MpTaskStartGoal) => List[List[Float]]](sbmp_planning_algorithm)*/

  println("...")
  println("done")

  def getResultList(b: Gamma.InhabitationBatchJob) = {
    @scala.annotation.tailrec
    def getElements(l: List[InhabitationResult[Any]], bnew: b.ResultType): List[InhabitationResult[Any]] =
      bnew match {
        case (newJob: b.ResultType, result: InhabitationResult[Any]) => getElements(result +: l, newJob)
        case a: InhabitationResult[Any] => a +: l
      }

    getElements(List.empty, b.run())
  }

  val l = getResultList(ihBatch)

  l.map(i => println(i.target.toString() + "," + (if (i.isEmpty) "inhabitant not found" else "inhabitant found")))
  // l.last.interpretedTerms.index(0)

  l.last.interpretedTerms.index(0)

  println("interpreted term run")
}
