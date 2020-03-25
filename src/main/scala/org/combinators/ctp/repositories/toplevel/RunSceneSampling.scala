package org.combinators.ctp.repositories.toplevel

import java.util.Properties

import akka.Done
import akka.stream.scaladsl.Source
import org.combinators.cls.interpreter.{InhabitationResult, ReflectedRepository}
import org.combinators.cls.types.Constructor
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories.celldecomposition.CellDecompRepository
import org.combinators.ctp.repositories.geometry.{GeometricRepository, GeometryUtils, PpVertexList}
import org.combinators.ctp.repositories.graphsearch.GraphSearchRepository
import org.combinators.ctp.repositories.mptasks.MpTaskStartGoal
import org.combinators.ctp.repositories.python_interop.{PythonWrapper, TemplatingScheme}
import org.combinators.ctp.repositories.samplingbased.SamplingBasedMpRepository
import org.combinators.ctp.repositories.scene._
import org.combinators.ctp.repositories.taxkinding.{CombinatorialMotionPlanning, SampleBasedMotionPlanning}
import org.combinators.ctp.repositories.toplevel.RunGraphPathInhabitationTrianglesSP.{Gamma, ihBatch, l}
import org.combinators.ctp.repositories.{cmp_cd_cells, cmp_cell_graph, dimensionality_three_d_t, dimensionality_two_d_t, sd_poly_scene_segmentation, sd_polygon_scene_type, sd_seg_cells, sd_seg_centroid_cells, sd_seg_lines, _}
import scalax.collection.Graph
import scalax.collection.edge.WUnDiEdge

import scala.concurrent.Future

object RunSceneSampling extends App {
  //val ihCall  = InhabitationCall[InteropRepository, Properties](new InteropRepository{}, Constructor("p_unityConnectionProperties_type"))

  lazy val repository = new SamplingBasedMpRepository {}

  lazy val asd = new CombinatorialMotionPlanning{}
  println("sample based naive")

  lazy val Gamma = ReflectedRepository(repository, substitutionSpace = asd.kinding)

  println("kinding: " + Gamma.substitutionSpace.toString)
  println("Reflected Repository built, starting inhabitation")


  val ihBatch = Gamma.InhabitationBatchJob[Properties](p_unityConnectionProperties_type)
    .addJob[String => TemplatingScheme](Constructor("emptyTemplate"))
    .addJob[String](Constructor("tscheme"))
    .addJob[String => PythonWrapper[String,String]](Constructor("omplPlannerPywrapper"))
    .addJob[String => String](Constructor("fooAnyPlanner"))

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

  l.map(i => println(i.target.toString() + "," + (if (i.isEmpty) "inhabitant not found" else "inhabitant found")))
  // l.last.interpretedTerms.index(0)

  val last  = Gamma.inhabit[String => String](Constructor("fooAnyPlanner"))
  last.interpretedTerms.index(0)("asd")

  println("interpreted term run")
}
