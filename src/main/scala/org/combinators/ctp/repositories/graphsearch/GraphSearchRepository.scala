package org.combinators.ctp.repositories.graphsearch

import org.combinators.cls.interpreter._
import org.combinators.cls.types.Type
import org.combinators.cls.types.syntax._
import scalax.collection.GraphEdge.EdgeLike
import scalax.collection.GraphPredef.{EdgeParam, Param}
import scalax.collection._
import scalax.collection.edge.WDiEdge
import scalax.collection.edge.Implicits._

class GraphSearchRepository {
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
