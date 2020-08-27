package org.combinators.ctp.repositories.runinhabitation

import com.typesafe.scalalogging.LazyLogging
import org.combinators.cls.interpreter.{InhabitationResult, ReflectedRepository}
import org.combinators.cls.types.syntax._
import org.combinators.cls.types.{Constructor, Intersection, Type, Variable}
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.cmp.CmpTopLevelRepository
import org.combinators.ctp.repositories.geometry.{GeometricRepository, GeometryUtils}
import org.combinators.ctp.repositories.graphsearch.{GraphSearchPyRepository, GraphSearchRepository}
import org.combinators.ctp.repositories.scene.SceneRepository
import org.combinators.ctp.repositories.taxkinding.CtpSemanticTypes
import org.combinators.ctp.repositories.toplevel._
import org.locationtech.jts.util.Stopwatch
import scalax.collection.Graph
import scalax.collection.edge.WUnDiEdge

object RunCmpTopLevelFileRm extends App with LazyLogging with AkkaImplicits {
  lazy val repository = new SceneRepository with GeometricRepository with AkkaMqttComponents
    with CmpTopLevelRepository with AkkaMqttTopLevelCmp with GeometryUtils
    with GraphSearchRepository with GraphSearchPyRepository{}
  lazy val cmpRepository = new CtpSemanticTypes{}

  val kindingMap = repository.cmpDefaultKindingMap ++
    Map(
      rmc_connectorNodes_var -> Seq(rmc_cn_withoutConnectorNodes),
      rmc_cellNodeAddFct_var -> Seq(rmc_cna_withoutCellNodes_type),
      rmc_cellGraph_var -> Seq(rmc_cg_centroidsOnly),
      rmc_usingCentroids_var -> Seq(rm_withCentroids_type),
      rmc_startGoalFct_var -> Seq(rmc_startGoal_nn_type),
      cmp_graph_algorithm_var -> Seq(cmp_graph_dijkstra_type),
      rmc_centroidFct_var -> Seq(cFct_avg_type),
      sd_poly_scene_cell_segmentation_var -> Seq(sd_seg_grid_type),
      dimensionality_var -> Seq(dimensionality_three_d_t),
      sd_poly_scene_cell_segmentation_var -> Seq(sd_seg_triangles_simple_type),
    )

  val cmpKinding = buildKinding(kindingMap)

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

  lazy val Gamma = ReflectedRepository(repository, substitutionSpace = cmpKinding)

  println("kinding: " + Gamma.substitutionSpace.toString)
  println("Reflected Repository built, starting inhabitation")

  println(s"# of combinators: ${Gamma.combinators.size}")

  val watch:Stopwatch = new Stopwatch
  watch.start()

  val ihBatch = Gamma.InhabitationBatchJob[ProblemDefinitionFiles => Graph[List[Float], WUnDiEdge]](
    resolveTypeExpression(cmp_sceneSegFct_type :&: cmp_cell_graph_fct_type :&: sd_poly_scene_cell_segmentation_var :&:
      dimensionality_var :&: rmc_cellNodeAddFct_var :&: rmc_startGoalFct_var :&: rmc_usingCentroids_var :&:
      rmc_centroidFct_var :&: sd_cell_type_var :&: rmc_cellGraph_var :&: rmc_connectorNodes_var))
    .addJob[(Graph[List[Float], WUnDiEdge], MpTaskStartGoal) => Seq[List[Float]]](resolveTypeExpression(cmp_graph_algorithm_var))
    .addJob[ProblemDefinitionFiles => List[List[Float]]](resolveTypeExpression(cmp_algorithm_type :&:
      cmp_graph_algorithm_var :&: rmc_connectorNodes_var :&: rmc_centroidFct_var :&: rmc_cellGraph_var :&:
      sd_cell_type_var :&: sd_poly_scene_cell_segmentation_var :&: dimensionality_var :&:
      rmc_cellNodeAddFct_var :&: rmc_startGoalFct_var :&: rmc_usingCentroids_var))



  def getResultList(b: Gamma.InhabitationBatchJob) = {
    @scala.annotation.tailrec
    def getElements(l: List[InhabitationResult[Any]], bnew: b.ResultType):List[InhabitationResult[Any]] =
    bnew match {
      case (newJob:b.ResultType, result:InhabitationResult[Any]) => getElements(result +: l, newJob)
      case a: InhabitationResult[Any] => a +: l
    }
    getElements(List.empty, b.run())
  }


  val l = getResultList(ihBatch)

  watch.stop()
  println(s"elapsed time ${watch.getTimeString}")

  l.map(i => println((if (i.isEmpty) "inhabitant not found" else "inhabitant found")  + "," +  i.target.toString()))

  l.last.interpretedTerms.index(0)

  println("Done")
}
