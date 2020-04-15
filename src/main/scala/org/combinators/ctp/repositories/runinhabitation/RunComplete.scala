package org.combinators.ctp.repositories.runinhabitation

import org.combinators.cls.interpreter.{InhabitationResult, ReflectedRepository}
import org.combinators.cls.types.Constructor
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.buildKinding
import org.combinators.ctp.repositories.cmp.CmpPythonRepository
import org.combinators.ctp.repositories.graphsearch.GraphSearchRepository
import org.combinators.ctp.repositories.samplebased.SbmpTopLevelRepository
import org.combinators.ctp.repositories.scene.SceneRepository
import org.combinators.ctp.repositories.taxkinding.CombinatorialMotionPlanning
import org.combinators.ctp.repositories.toplevel.{AkkaMqttTopLevelCmp, CmpTopLevel}
import org.locationtech.jts.util.Stopwatch
import org.combinators.cls.types.syntax._

object RunComplete extends App {
  lazy val repository =  new SceneRepository  with CmpTopLevel with AkkaMqttTopLevelCmp with CmpPythonRepository
    with GraphSearchRepository with SbmpTopLevelRepository{}
  lazy val cmpRepository = new CombinatorialMotionPlanning{}

  val sbmpKinding = buildKinding(repository.sbmpDefaultKindingMap ++ repository.cmpDefaultKindingMap)

  lazy val Gamma = ReflectedRepository(repository, substitutionSpace = sbmpKinding)

  println("kinding: " + Gamma.substitutionSpace.toString)
  println("Reflected Repository built, starting inhabitation")

  println(s"Combinators ${Gamma.combinators.size}")
  val watch:Stopwatch = new Stopwatch
  watch.start()

  val ihBatch = Gamma.InhabitationBatchJob[Unit](p_mqttAkkaComposition_type :&:
    dimensionality_three_d_t :&: cmp_scene_graph_path)
    .addJob[Unit](p_mqttAkkaComposition_type :&: dimensionality_three_d_t :&: cmp_scene_graph_path)
    .addJob[Unit](p_mqttAkkaComposition_type :&: cmp_scene_graph_path :&: cmp_graph_mst_type)
    .addJob[Unit](p_mqttAkkaComposition_type :&: cmp_scene_graph_path :&: sd_seg_triangles_para_type :&: mpt_start_goal_position_type :&: cmp_graph_a_star_type)
    .addJob[Unit](p_mqttAkkaComposition_type :&: cmp_scene_graph_path :&: sd_seg_triangles_simple_type :&:
      mpt_start_goal_position_type)
    .addJob[Unit](p_mqttAkkaComposition_type :&: cmp_scene_graph_path :&: sd_seg_triangles_para_type :&: Constructor("graphTsp"))
    .addJob[Unit](p_mqttAkkaComposition_type :&: cmp_scene_graph_path :&: sd_vertical_cell_decomposition_type)
    .addJob[Unit](p_mqttAkkaComposition_type :&: dimensionality_two_d_t :&: cmp_path_only :&: Constructor("sampleAkka"))
    .addJob[Unit](p_mqttAkkaComposition_type :&: dimensionality_three_d_t :&: cmp_path_only :&: Constructor("sampleAkka"))


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

  l.map(i => println(i.target.toString() + "," + (if (i.isEmpty) "inhabitant not found" else "inhabitant found")))

  l.last.interpretedTerms.index(0)

  println("Done")
}
