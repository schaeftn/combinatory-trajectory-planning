package org.combinators.ctp.repositories.graphsearch

import scalax.collection.edge.Implicits._
import scalax.collection.edge.LkDiEdge // shortcuts

import scalax.collection.Graph // or scalax.collection.mutable.Graph
import scalax.collection.GraphPredef._
//import scalax.collection.GraphEdge._

object scalagraphtest extends App{
  val g = Graph(1 ~ 2 % 4, 2 ~ 3 % 2, 1 ~> 3 % 5, 1 ~ 5 % 3,
    3 ~ 5 % 2, 3 ~ 4 % 1, 4 ~> 4 % 1, 4 ~> 5 % 0)
  def n(outer: Int): g.NodeT = g get outer  // look up 'outer' that is known to be contained

  n(1) findSuccessor (_.outDegree >  3) // Option[g.NodeT] = None
  n(1) findSuccessor (_.outDegree >= 3) // Option[g.NodeT] = Some(3)
  n(4) findSuccessor (_.edges forall (_.undirected)) // Some(2)
  n(4) isPredecessorOf n(1)             // true
  val foopath = n(1) pathTo n(4)                      // Some(Path(1, 1~>3 %5, 3, 3~4 %1, 4))
  n(1) pathUntil (_.outDegree >= 3)     // Some(Path(1, 1~>3 %5, 3))

  val spO = n(3) shortestPathTo n(1) // Path(3, 3~4 %1, 4, 4~>5 %0, 5, 1~5 %3, 1)
  val sp = spO.get                   // here we know spO is defined
  sp.nodes                           // List[g.NodeT] = Nodes(3, 4, 5, 1)
  sp.weight                          // Double = 4.0

  val test = 1 ~ 2

  def negWeight(e: g.EdgeT): Float = 5.5f - e.weight.toFloat
foopath

  val spNO =
    n(3) shortestPathTo (n(1), negWeight) // Path(3, 2~3 %2, 2, 1~2 %4, 1)
  val spN = spNO.get                        // here we know spNO is defined
  spN.weight                                // Double = 6.0

  val pO1 = n(4).withSubgraph(nodes = _ < 4) pathTo n(2) // Some(Path(4, 3~4 %1, 3, 2~3 %2, 2))
 println( pO1.map(_.nodes)                                       )// Some(Nodes(4, 3, 2))

  val pO2 = n(4).withSubgraph(edges = _.weight != 2) pathTo n(2)
  // Some(Path(4, 4~>5 %0, 5, 1~5 %3, 1, 1~2 %4, 2))
  pO2.map(_.nodes)                   // Some(Nodes(4, 5, 1, 2))

  println(pO2.map(_.nodes))
}
