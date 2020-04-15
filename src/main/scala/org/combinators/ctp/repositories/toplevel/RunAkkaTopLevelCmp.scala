package org.combinators.ctp.repositories.toplevel

import java.util.Properties

import akka.Done
import akka.stream.alpakka.mqtt.MqttMessage
import akka.stream.scaladsl.{Sink, Source}
import com.typesafe.scalalogging.LazyLogging
import org.combinators.cls.interpreter.{InhabitationResult, ReflectedRepository}
import org.combinators.cls.types.syntax._
import org.combinators.cls.types.{Type, Variable}
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.geometry.{GeometricRepository, GeometryUtils}
import org.combinators.ctp.repositories.graphsearch.{GraphSearchPyRepository, GraphSearchRepository}
import org.combinators.ctp.repositories.mptasks.MpTaskStartGoal
import org.combinators.ctp.repositories.scene._
import org.combinators.ctp.repositories.taxkinding.CombinatorialMotionPlanning
import org.locationtech.jts.util.Stopwatch

import scala.concurrent.Future

object RunAkkaTopLevelCmp extends App with LazyLogging with AkkaImplicits {
  lazy val repository = new SceneRepository with GeometricRepository with AkkaMqttComponents
    with CmpTopLevel with AkkaMqttTopLevel with GeometryUtils
    with GraphSearchRepository with GraphSearchPyRepository {}
  lazy val cmpRepository = new CombinatorialMotionPlanning {}

  val kindingMap = repository.cmpDefaultKindingMap ++
    Map(
      rmc_connectorNodes_var -> Seq(rmc_cn_withoutConnectorNodes),
      rmc_cellGraph_var -> Seq(rmc_cg_centroidsOnly),
      rmc_startGoalFct_var -> Seq(rmc_startGoal_nn_type),
      rmc_cellNodeAddFct_var -> Seq(rmc_cna_withoutCellNodes_type),
      cmp_graph_algorithm_var -> Seq(cmp_graph_dijkstra_type),
      dimensionality_var -> Seq(dimensionality_three_d_t),
      rmc_centroidFct_var -> Seq(cFct_avg_type)
    )
  val cmpKinding = buildKinding(kindingMap)

  def getTypeFromMap(v: Variable): Type = {
    val typeList = kindingMap(v)
    if (typeList.size != 1)
      println(s"Typesize for $v is not 1: ${typeList}")
    typeList.head
  }

  lazy val Gamma = ReflectedRepository(repository, substitutionSpace = cmpKinding)

  println("kinding: " + Gamma.substitutionSpace.toString)
  println("Reflected Repository built, starting inhabitation")

  println(s"# of combinators: ${Gamma.combinators.size}")

  val watch: Stopwatch = new Stopwatch
  watch.start()

  val ihBatch = Gamma.InhabitationBatchJob[Properties](p_unityConnectionProperties_type)
    .addJob[Source[Scene, Future[Done]]](p_mqttAkkaSource_type :&: sd_unity_scene_type :&:
      getTypeFromMap(dimensionality_var))
    .addJob[Source[MpTaskStartGoal, Future[Done]]](p_mqttAkkaSource_type :&: mpt_start_goal_position_type :&:
      getTypeFromMap(dimensionality_var))
    .addJob[(Scene, MpTaskStartGoal) => PolySceneSegmentationRoadmapPath](
      cmp_algorithm_type :&:
        getTypeFromMap(cmp_graph_algorithm_var) :&:
        getTypeFromMap(rmc_connectorNodes_var) :&:
        getTypeFromMap(rmc_centroidFct_var) :&:
        getTypeFromMap(rmc_cellGraph_var) :&:
        getTypeFromMap(sd_cell_type_var) :&:
        getTypeFromMap(sd_poly_scene_cell_segmentation_var) :&:
        getTypeFromMap(dimensionality_var))
    .addJob[Sink[MqttMessage, Future[Done]]](
      p_mqttAkkaSink_type :&:
        cmp_scene_graph_path :&:
        dimensionality_three_d_t)
    .addJob[Unit](
      p_mqttAkkaComposition_type :&:
        cmp_scene_graph_path :&:
        getTypeFromMap(cmp_graph_algorithm_var) :&:
        getTypeFromMap(rmc_connectorNodes_var) :&:
        getTypeFromMap(rmc_centroidFct_var) :&:
        getTypeFromMap(rmc_cellGraph_var) :&:
        getTypeFromMap(sd_cell_type_var) :&:
        getTypeFromMap(sd_poly_scene_cell_segmentation_var) :&:
        getTypeFromMap(dimensionality_var)
    )

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

  watch.stop()
  println(s"elapsed time ${watch.getTimeString}")

  l.map(i => println((if (i.isEmpty) "inhabitant not found" else "inhabitant found") + "," + i.target.toString()))

  l.last.interpretedTerms.index(0)

  println("Done")
}
