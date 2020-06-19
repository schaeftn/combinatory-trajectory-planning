package org.combinators.ctp.repositories.graphsearch

import org.combinators.cls.types.Constructor
import org.combinators.ctp.repositories.geometry.GeometryUtils
import scalax.collection.edge.WUnDiEdge
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scala.language.{postfixOps}

import org.combinators.cls.interpreter._
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.toplevel._
import org.combinators.ctp.repositories.python_interop.PythonTemplateUtils

import io.circe.parser.decode
import scala.sys.process._


trait GraphSearchRepository extends GeometryUtils with PythonTemplateUtils with GraphSearchPyRepository{
//  @combinator object ValueIterationC {
//    def apply(g: Graph[List[Float], WUnDiEdge], termNodes: Set[List[Float]]) =
//      ValueIteration(g, termNodes)
//
//    val semanticType = 'graph =>: 'SynthTarghet =>: 'Path :&: 'mitValIteration
//  }

//  @combinator object ValueIterationCombinator {
//    def apply: (Graph[List[Float], WUnDiEdge], MpTaskStartGoal) =>
//      Seq[List[Float]] = {
//      case (g, mpTask) =>
//        println(s"graph: $g" )
//        println(s"Task: $mpTask")
//        val result = ValueIteration(g, Set(mpTask.endPosition))
//          .optPlanStartingWith(mpTask.startPosition).getOrElse(Seq.empty, Seq.empty,0.0f)._1
//        println(s"VI done: $result")
//        result
//    }
//
//    val semanticType = Constructor("graphTraversalVi")
//  }

  @combinator object DijkstraSpCombinator {
    def apply: (Graph[List[Float], WUnDiEdge], MpTaskStartGoal) => Seq[List[Float]] = {
      case (g, mpTask) =>
        println(s"graph: $g")
        println(s"Task: $mpTask")
        val startNode = g.get(mpTask.startPosition)
        val endNode = g.get(mpTask.endPosition)
        println(s"startNode: $startNode endNode:$endNode")
        val result1 = startNode shortestPathTo endNode
        println("Done shortest path")
        result1 match {
          case Some(p) => println(s"Dijkstra found Path: $p")
            p.nodes.map(_.toOuter).toSeq
          case _ => println(s"Dijkstra could not find a path")
            Seq.empty
        }
    }

    val semanticType = cmp_graph_dijkstra_type
  }
}
