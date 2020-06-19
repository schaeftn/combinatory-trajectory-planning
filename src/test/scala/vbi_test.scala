//import org.combinators.ctp.repositories.graphsearch.ValueIteration
//import org.scalatest.FlatSpec
//import scalax.collection.edge.Implicits._
//import scalax.collection.edge.WDiEdge
//import scalax.collection.immutable.Graph
//
//import scala.util.Random
//
//class vbi_test extends FlatSpec {
//  val (node_a, node_b, node_c, node_d, node_e) = ("a", "b", "c", "d", "e")
//
//  val single_node_graph: Graph[String, WDiEdge] = Graph((node_a ~%> node_a) (20))
//  val single_node_result = ValueIteration(single_node_graph, Set("a"))
//
//  "A single node graph without edges" should "yield the expected result " in {
//    val map = scala.collection.immutable.Map("a" -> 0.0f)
//    assert(single_node_result.table.head equals map)
//  }
//
//  val empty_graph: Graph[String, WDiEdge] = Graph[String, WDiEdge]()
//
//  // from: LaValle, Planning Algorithms. 2.3.1.1
//  val paper_graph: Graph[String, WDiEdge] = Graph((node_a ~%> node_a) (2), (node_a ~%> node_b) (2), (node_b ~%> node_c) (1), (node_b ~%> node_d) (4),
//    (node_c ~%> node_a) (1), (node_c ~%> node_d) (1), (node_d ~%> node_c) (1), (node_d ~%> node_e) (1))
//
//  val failureResult = ValueIteration(empty_graph, Set.empty[String])
//
//  assert(failureResult.table.isEmpty)
//
//  val result = ValueIteration(paper_graph, Set("d"))
//
//  "The example from the paper" should "yield the correct table result" in {
//    val map = scala.collection.immutable.Map(
//      "b" -> 2.0f,
//      "a" -> 4.0f,
//      "c" -> 1.0f,
//      "d" -> 0.0f,
//      "e" -> Float.PositiveInfinity
//    )
//    assert(result.table.head equals map)
//  }
//
//  "The optimal plans" should "be as expected" in {
//    val optA = Some(List("a", "b", "c", "d"))
//    val optB = Some(List("b", "c", "d"))
//    val optC = Some(List("c", "d"))
//    val optD = Some(List("d"))
//    val optE = None
//
//    assert(optA equals result.optPlanStartingWith("a"))
//    assert(optB equals result.optPlanStartingWith("b"))
//    assert(optC equals result.optPlanStartingWith("c"))
//    assert(optD equals result.optPlanStartingWith("d"))
//    assert(optE equals result.optPlanStartingWith("e"))
//  }
//
//  val random: Random.type = scala.util.Random
//
//  val nodeSet = Set(node_a, node_b, node_c, node_d, node_e, "f", "g", "h", "i", "j",
//    "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z", "1", "2", "3", "4", "5", "6", "7", "8", "9", "0")
//
//  val bigGraphNodeList: List[WDiEdge[String]] = nodeSet.subsets(2).flatMap {
//    s => {
//      val _1 = s.head
//      val _2 = s.tail.head
//      Set((_1 ~%> _2) (random.nextInt(10) + 1), (_2 ~%> _1) (random.nextInt(10) + 1))
//    }
//  }.toList
//
//  val bigGraph: Graph[String, WDiEdge] = Graph(bigGraphNodeList: _*)
//
//  println(bigGraph)
//
//  def calculateCostsFor(valueIterationPath: List[String], graph: Graph[String, WDiEdge]): Double = valueIterationPath match {
//    case node :: nextNode :: restList =>
//      bigGraph.get(node).outgoingTo(bigGraph.get(nextNode)).head.weight + calculateCostsFor(nextNode :: restList, graph)
//    case _ :: Nil => 0.0d
//    case Nil => 0.0d
//  }
//
//  for {start <- nodeSet} {
//    for {target <- nodeSet} {
//      val dijstraPath = (bigGraph.get(start) shortestPathTo bigGraph.get(target))
//        .getOrElse(throw new IllegalStateException)
//
//      val valueIterationPath = ValueIteration(bigGraph, Set(target))
//        .optPlanStartingWith(start)
//
//      assert(dijstraPath.edges.foldLeft[Double](0.0) {
//        case (acc, eT) => eT.weight + acc
//      } equals calculateCostsFor(valueIterationPath.get._1.toList, bigGraph), "DijstraPath: " + dijstraPath + "ValueIterationPath: " + valueIterationPath + " are diverging unexpectedly")
//    }
//  }
//  val times = for {i <- Range(0, 20)} yield {
//    val timeBeforeDijkstra = System.currentTimeMillis()
//
//    for {start <- nodeSet} {
//      for {target <- nodeSet} {
//        bigGraph.get(start) shortestPathTo bigGraph.get(target)
//      }
//    }
//
//    val timeBeforeValueiteration = System.currentTimeMillis()
//
//    for {start <- nodeSet} {
//      for {target <- nodeSet} {
//        ValueIteration(bigGraph, Set(target))
//          .optPlanStartingWith(start)
//      }
//    }
//
//    val timeAfterValueIteration = System.currentTimeMillis()
//    println(i)
//    println(s"Dijkstra: ${timeBeforeValueiteration - timeBeforeDijkstra}, Value Iteration: ${timeAfterValueIteration - timeBeforeValueiteration}")
//    (timeBeforeValueiteration - timeBeforeDijkstra, timeAfterValueIteration - timeBeforeValueiteration)
//  }
//
//  println(times)
//}
