package org.combinators.ctp.repositories.graphsearch

import org.combinators.cls.interpreter._
import org.combinators.cls.types.syntax._
import scalax.collection.Graph
import scalax.collection.edge.WDiEdge

import scala.language.{higherKinds, postfixOps}

trait GraphSearchRepository {

  @combinator object ValueIterationC {
    //apply[N, E[X] <: UnDiEdge[X] with WEdge[X]](graph: Graph[N, E], termNodes: Set[N])
    def apply(g: Graph[(List[Float], List[Float]), WDiEdge], termNodes: Set[(List[Float], List[Float])]) =
      ValueIteration(g, termNodes)

    val semanticType = 'graph =>: 'SynthTarghet =>: 'Path :&: 'mitValIteration
  }

  @combinator object DFS{

    /*
      1 procedure DFS(G, v):
      2     label v as explored
      3     for all edges e in G.incidentEdges(v) do
      4         if edge e is unexplored then
      5             w ← G.adjacentVertex(v, e)
      6             if vertex w is unexplored then
      7                 label e as a discovered edge
      8                 recursively call DFS(G, w)
      9             else
      10               label e as a back edge
      */


    def apply(): Unit = { println("foo")
      //    val g = Graph(1~2 % 4, 2~3 % 2, 1~>3 % 5, 1~5  % 3,
      //      3~5 % 2, 3~4 % 1, 4~>4 % 1, 4~>5 % 0)
    }
    val semanticType = 'graph_traversal =>: 'gt_dfs
  }


  /*  @combinator object ShortestPathDijkstra{
      def apply(g: Graph[Int, WUnDiEdgeAssoc], s: Int, t:Int): g.Path = {
        val p:Option[g.Path] = g get (s) shortestPathTo(g get (t))
        p.get
      }

      val semanticType = 'graph =>: 'graphsource =>: 'graphtarget =>: 'graph_traversal :&: 'gt_dijkstra
    }*/





}
