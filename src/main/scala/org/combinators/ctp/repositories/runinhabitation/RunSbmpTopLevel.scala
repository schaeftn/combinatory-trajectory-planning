package org.combinators.ctp.repositories.runinhabitation


import org.combinators.cls.interpreter.{InhabitationResult, ReflectedRepository}
import org.combinators.cls.types.syntax._
import org.combinators.cls.types.{Constructor, Intersection, Type, Variable}
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.cmp.{CmpCdRepository, CmpTopLevelRepository}
import org.combinators.ctp.repositories.graphsearch.GraphSearchRepository
import org.combinators.ctp.repositories.python_interop.{PlannerScheme, SubstitutionScheme}
import org.combinators.ctp.repositories.samplebased.SbmpTopLevelRepository
import org.combinators.ctp.repositories.scene.SceneRepository
import org.combinators.ctp.repositories.taxkinding.CtpSemanticTypes
import org.combinators.ctp.repositories.toplevel.{FileBasedTopLevelSbmp, ProblemDefinitionFiles}

object RunSbmpTopLevel extends App {
  lazy val repository = new SceneRepository with CmpTopLevelRepository with FileBasedTopLevelSbmp with CmpCdRepository
    with GraphSearchRepository with SbmpTopLevelRepository {}
  lazy val cmpRepository = new CtpSemanticTypes {}

  val sbmpKindingMap = Map(sbmp_planner_var -> Seq(sbmp_planner_RRT),
    sbmp_sampler_var -> Seq(sbmp_uniform_valid_state_sampler),
    sbmp_state_validator_var -> Seq(sbmp_fcl_validator),
    sbmp_motion_validator_var -> Seq(sbmp_fcl_motion_validator),
    sbmp_cost_var -> Seq(sbmp_default_cost_state),
    sbmp_optimization_objective_var -> Seq(sbmp_opt_path_length),
    dimensionality_var -> Seq(dimensionality_three_d_t)
  )
  val kindingMap = repository.cmpDefaultKindingMap ++ repository.sbmpDefaultKindingMap ++ sbmpKindingMap
  // cmpFull: 28k substitutions
  // sbmpFull: 94k substitutions
  val sbmpKinding = buildKinding(kindingMap)

  println("Building reflected repository")
  lazy val Gamma = ReflectedRepository(repository, substitutionSpace = sbmpKinding)
// Gamma.addCombinator()
// map combinator name -> instance
// Gamma.combinatorComponents.map(_._2.)
// Gamma.staticCombinatorInfoFor().map(_._2.)
// Dynamic repository


  println(s"Gamma.combinators.size: ${Gamma.combinators.size}")
  println(s"Gamma.combinatorComponents.size: ${Gamma.combinatorComponents.size}")

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

  def resolveTypeExpression(t: Type): Type = t match {
    case Intersection(a, b) => Intersection(resolveTypeExpression(a), resolveTypeExpression(b))
    case Variable(a) => getTypeFromMap(Variable(a))
    case Constructor(name, arguments) => Constructor(name, arguments)
  }

  val ihBatch = Gamma.InhabitationBatchJob[Unit](
    p_fileToAkka_type :&: cmp_path_only :&:
      getTypeFromMap(dimensionality_var) :&:
      getTypeFromMap(sbmp_planner_var) :&:
      getTypeFromMap(sbmp_sampler_var) :&:
      getTypeFromMap(sbmp_state_validator_var) :&:
      getTypeFromMap(sbmp_motion_validator_var) :&:
      getTypeFromMap(sbmp_optimization_objective_var) :&:
      getTypeFromMap(sbmp_cost_var))
    .addJob[ProblemDefinitionFiles => List[List[Float]]](
      sbmp_planning_algorithm :&:
        getTypeFromMap(sbmp_planner_var) :&:
        getTypeFromMap(sbmp_sampler_var) :&:
        getTypeFromMap(sbmp_state_validator_var) :&:
        getTypeFromMap(sbmp_motion_validator_var) :&:
        getTypeFromMap(sbmp_optimization_objective_var) :&:
        getTypeFromMap(sbmp_cost_var))
    .addJob[PlannerScheme[List[List[Float]]]](resolveTypeExpression(sbmp_planner_var))
    .addJob[SubstitutionScheme](resolveTypeExpression(sbmp_sampler_var))
    .addJob[SubstitutionScheme](resolveTypeExpression(sbmp_state_validator_var :&: sbmp_state_validator_var))
    .addJob[SubstitutionScheme](resolveTypeExpression(sbmp_motion_validator_var))
    .addJob[SubstitutionScheme](resolveTypeExpression(sbmp_cost_var :&: sbmp_optimization_objective_var))
    .addJob[ProblemDefinitionFiles => SubstitutionScheme](resolveTypeExpression(sbmp_input_data :&: dimensionality_var))


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
