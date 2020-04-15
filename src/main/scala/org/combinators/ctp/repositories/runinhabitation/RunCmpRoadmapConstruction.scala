package org.combinators.ctp.repositories.runinhabitation

import org.combinators.cls.interpreter.{InhabitationResult, ReflectedRepository}
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.buildKinding
import org.combinators.ctp.repositories.toplevel._
import scalax.collection.edge.WUnDiEdge
import org.combinators.cls.types.syntax._

object RunCmpRoadmapConstruction extends App {
  lazy val repository = new CmpTopLevel{}

  val cmpKinding = buildKinding(repository.cmpDefaultKindingMap ++
    Map(
      rmc_connectorNodes_var -> Seq(rmc_cn_withoutConnectorNodes),
      rmc_cellGraph_var -> Seq(rmc_cg_centroidsOnly),
      rmc_startGoalFct_var -> Seq(rmc_startGoal_nn_type),
      dimensionality_var -> Seq(dimensionality_three_d_t),
      rmc_centroidFct_var -> Seq(cFct_avg_type)))

  lazy val Gamma = ReflectedRepository(repository, substitutionSpace = cmpKinding)

  println("kinding: " + Gamma.substitutionSpace.toString)
  println("Reflected Repository built, starting inhabitation")

  val ihBatch = Gamma.
    InhabitationBatchJob[(PolySceneCellSegmentation, MpTaskStartGoal) => PolySceneSegmentationRoadmap](cmp_cell_graph_fct).
    addJob[(PolySceneCellSegmentation, neighbourCellsNativeType) => List[List[List[Float]]]](
      cmd_centroidFct_type :&: rm_withCentroids_type :&: dimensionality_three_d_t).
    addJob[(PolySceneCellSegmentation, neighbourCellsNativeType) => List[List[List[Float]]]](rmc_cellNodeAddFct_type :&: rmc_cna_withCellNodes_type).
    addJob[(PolySceneCellSegmentation, neighbourCellsNativeType) => List[List[List[Float]]]](rmc_connectorNodeFct_type :&: rmc_cn_withConnectorNodes).
    addJob[(neighbourCellsNativeType, IndexedSeq[RmAuxDataNodes]) => IndexedSeq[WUnDiEdge[List[Float]]]](rmc_edgeAdd_type :&: rmc_cg_centroidsOnly).
    addJob[(PolySceneCellSegmentation, IndexedSeq[RmAuxDataNodes], MpTaskStartGoal) =>
      (List[List[Float]], List[WUnDiEdge[List[Float]]])](rmc_startGoalFct_type :&: rmc_startGoal_nn_type :&: rmc_cg_centroidsOnly :&: dimensionality_three_d_t)

  println("...")
  println("done")

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

  l.map(i => println((if (i.isEmpty) "inhabitant not found" else "inhabitant found") + "," + i.target.toString()))

  l.last.interpretedTerms.index(0)

  println("interpreted term run")





}
