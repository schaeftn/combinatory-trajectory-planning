package org.combinators.ctp.repositories.graphsearch

import scalax.collection.{Graph, GraphTraversal}
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.edge.WBase.WEdge

import scala.annotation.tailrec
import scala.collection.FilterableSet
import scala.language.{higherKinds, postfixOps}

trait test {
  /*val testWalk = new Walk{}*/
}
/**
 * This class represents an implementation of the backwards value iteration algorithm presented in
 * [[http://planning.cs.uiuc.edu/]].
 * It is used to find an optimal plan in a graph scenario starting with
 * a node and ending in one of possibly multiple nodes defined as terminal nodes.
 *
 * Graphs are represented as [[scalax.collection.Graph]]-instances of the scala-graph framework
 * found at [[http://www.scala-graph.org/]].
 *
 * To retrieve an optimal plan use the apply-Method of [[ValueIteration]] in combination with [[optPlanStartingWith]]:
 * {{{
 *   //Single node graph
 *   val graph:Graph[String, WDiEdge] = Graph(("a" ~%> "a") (20))
 *
 *   //Backwards value iteration for terminal nodes "a"
 *   val valueIteration = ValueIteration(graph,Set("a"))
 *
 *   //Optimal plan to reach "a" starting at "a"
 *   val optPlan = valueIteration.optPlanStartingWith("a")
 *
 *   //get Edges of optimal plan
 *   val edges = optPlan match {
 *    case Some(plan) => valueIteration.optPlanEdges(plan)
 *    case None => Set()
 *   }
 *
 * }}}
 *
 * @param graph graph that is used for backwards value iteration
 * @param termNodes terminal nodess
 * @tparam N type of nodes
 * @tparam E type of edges. Needs to be at least an undirected weighted edge
 * @author Tilman Zuckmantel
 */
sealed case class
ValueIteration[N, E[X] <: UnDiEdge[X] with WEdge[X]](graph: Graph[N, E],
                                                     termNodes: Set[N]) {
  val table: List[Map[N, Float]] = buildTableFromGraph(termNodes).map { f =>
    f.foldLeft(scala.collection.immutable.Map.empty[N, Float]) {
      case (m, (k, v)) => m + (k -> v)
    }
  }

  /**
   * A solution plan resulting from the use of method [[optPlanStartingWith]]
   * constists of:
   *
   *  - A sequence of nodes in the order of the optimal plan
   *  - A sequence of edges in the order of the optimal plan
   *  - the total costs for the plan
   */
  type Plan = (Seq[N],Seq[E[N]],Float)

  /**
   * Default value for a Path
   */
  val EmptyPath:Plan = (Seq.empty,Seq.empty,Float.PositiveInfinity)

  private final def minimalCost(costs: List[Float]): Float = costs.min

  /**
   * Builds table entries required by backwards value iteration iteratively.
   *
   * The table entries computed by this method are processed into a map
   * as follows:
   * {{{
   *   buildTableFromGraph(termNodes).map { f =>
   *     f.foldLeft(scala.collection.immutable.Map.empty[N, Float]) {
   *       case (m, (k, v)) => m + (k -> v)}
   * }}}
   *
   * @param termNodes terminal nodes
   * @return a list of table entries
   */
  private final def buildTableFromGraph(termNodes: Set[N]): List[List[(N, Float)]] = {

    def calculateMinPath(lastIter: List[(N, Float)], uOutgoing: Set[graph.EdgeT] with FilterableSet[graph.EdgeT], terminalOptionCost: Float): Float = {
      minimalCost((for (u <- uOutgoing) yield {
        u.toOuter match {
          case x: UnDiEdge[_] => lastIter.find { cost => cost._1.equals(u.to.toOuter) }.get._2 + x.weight.toFloat // check 2 mal gleiche cost?
          case _ => throw new IllegalStateException("Type other than UnDiEdge is not allowed")
        }
      }).toList ++ List(terminalOptionCost))
    }

    @tailrec
    def iteration(table: List[List[(N, Float)]]): List[List[(N, Float)]] = table match {
      case Nil => throw new IllegalArgumentException("No empty table allowed")
      case allIters@lastIter :: _ =>
        val newRow = (for (j <- graph.nodes.toOuter) yield {
          val uOutgoing = graph.get(j).outgoing
          val terminalOptionCost = lastIter.collectFirst { case (n, cost) if n equals j => cost }.get
          val localMin = calculateMinPath(lastIter, uOutgoing, terminalOptionCost)
          (j, localMin)
        }).toList
        if (lastIter equals newRow) {
          allIters
        } else {
          iteration(newRow :: allIters)
        }
    }

    if (graph.nonEmpty) {
      val initList: List[(N, Float)] = graph.nodes.toOuter.map { n => if (termNodes.contains(n)) (n, 0.0f) else (n, Float.PositiveInfinity) }.toList
      iteration(List(initList))
    }
    else {
      Nil
    }
  }

  /**
   * @param optPlan optimal plan retrieved by [[optPlanStartingWith]]
   * @return a sequence of nodes in the order of appearance in the optimal plan
   */
  final def optPlanNodes(optPlan:Plan):Seq[N] = optPlan._1

  /**
   * @param optPlan optimal plan retrieved by [[optPlanStartingWith]]
   * @return a sequence of edges in the order of appearance in the optimal plan
   */
  final def optPlanEdges(optPlan:Plan):Seq[E[N]] = optPlan._2

  /**
   * @param optPlan optimal plan retrieved by [[optPlanStartingWith]]
   * @return costs for the optimal plan
   */
  final def optPlanCosts(optPlan:Plan):Float = optPlan._3

  /**
   * returns an optimal plan for a start node n or None, if no plan exists.
   *
   * use helper functions [[optPlanCosts]], [[optPlanEdges]] and [[optPlanNodes]],
   * if you are only interested in retrieving a specific part of the plan.
   *
   * @see [[Plan]] for the structure of a plan.
   * @param n start node .
   * @return a optimal plan, None if no plan exists.
   */
  final def optPlanStartingWith(n: N): Option[Plan] = {
    def findArgMinimum(minToCalc: Set[(N,graph.EdgeT, Float)]): Option[(N,graph.EdgeT, Float)] = {
      if (minToCalc.nonEmpty)
        Some(minToCalc.minBy(_._3))
      else
        None
    }

    def calculateArgMinimumBodies(currentNode: N, nextCtg: Map[N, Float]): Set[(N, graph.EdgeT, Float)] = {
      //Iteriere über alle Kanten, die vom aktuellen Knoten ausgehen
      for (u <- graph.get(currentNode).outgoing) yield {
        //Sammle die Kosten für die aktuelle Kante
        val edgeCosts = u.toOuter match {
          case x: UnDiEdge[_] => x.weight.toFloat
          case _ => throw new IllegalStateException("no other type then UnDiEdge allowed")
        }
        //Zielknoten der Kante festlegen
        val nextCurrNode = u.to.toOuter
        //Kosten des Knotens als Ziel in der nächsten Iteration
        val costOfNextCostToGoForNextCurrNode = nextCtg.getOrElse(nextCurrNode, throw new IllegalStateException("Illegal table"))
        (nextCurrNode, u, costOfNextCostToGoForNextCurrNode + edgeCosts)
      }
    }

    //TODO Prüfen tailrec
    def calculateMinPath(currentNode: N,
                         costToGo: List[scala.collection.immutable.Map[N, Float]]): Option[Plan] =
      costToGo match {
        case _ :: ctgs if !termNodes.contains(currentNode) =>
          val nextIter = ctgs.headOption
          nextIter match {
            case None => None
            case Some(nextCtg) =>
              val minToCalc = calculateArgMinimumBodies(currentNode, nextCtg)
              val argMinimum = findArgMinimum(minToCalc)
              argMinimum match {
                case Some((node,e,c)) =>
                  val lastRec = calculateMinPath(node,ctgs).getOrElse(Seq.empty,Seq.empty,Float.PositiveInfinity)
                  Some((node +: lastRec._1,e.toOuter +: lastRec._2,c))
                case None => None
              }
          }
        case Nil => None
        case _ if termNodes.contains(currentNode) =>
          Some((Seq.empty,Seq.empty,Float.PositiveInfinity))
      }

    table.headOption match {
      case None => None
      case Some(map) =>
        val startCost = map.getOrElse(n, throw new IllegalArgumentException("Node (" + n + ") is no member of graph"))
        startCost match {
          case Float.PositiveInfinity => None
          case _ =>
            val lastRec = calculateMinPath(n,table)
            Some(n +: lastRec.getOrElse(EmptyPath)._1,lastRec.getOrElse(EmptyPath)._2,table.head(n))
        }
    }
  }

}

object ValueIteration {
  /**
   * Applies backwards value iteration to a given graph `graph` and a set of terminal nodes `termNodes`.
   *
   * The resulting ValueIteration object contains a [[scala.collection.immutable.Map]] containing all iteration
   * steps of the backwards value iteration algorithm. It can then be used to retrieve an optimal plan using the
   * [[ValueIteration.optPlanStartingWith]] method.
   *
   * @param graph graph that is used for backwards value iteration
   * @param termNodes Terminal nodes that need to be contained in `graph`
   * @tparam N Node type
   * @tparam E Edge type. Edges must be weighted
   * @return an instance of [[ValueIteration]] containing a map with all iterations of backwards value iteration
   */
  def apply[N, E[X] <: UnDiEdge[X] with WEdge[X]](graph: Graph[N, E], termNodes: Set[N]): ValueIteration[N, E] = {
    new ValueIteration[N, E](graph, termNodes)
  }
}
