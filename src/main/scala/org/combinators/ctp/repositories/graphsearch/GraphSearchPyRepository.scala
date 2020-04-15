package org.combinators.ctp.repositories.graphsearch

import io.circe.parser.decode
import io.circe.generic.auto._
import io.circe.syntax._
import org.combinators.cls.interpreter._
import org.combinators.cls.types.Constructor
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.geometry.GeometryUtils
import org.combinators.ctp.repositories.mptasks.MpTaskStartGoal
import org.combinators.ctp.repositories.python_interop.{PythonTemplateUtils, PythonWrapper, SubstitutionScheme}
import org.combinators.ctp.repositories.scene.PathPreds
import org.combinators.ctp.repositories.toplevel.EncodeImplicits
import scalax.collection.Graph
import scalax.collection.edge.WUnDiEdge

import scala.language.{higherKinds, postfixOps}

trait GraphSearchPyRepository extends GeometryUtils with PythonTemplateUtils with EncodeImplicits{
  @combinator object AStarCombinator {
    def apply: (Graph[List[Float], WUnDiEdge], MpTaskStartGoal) => Seq[List[Float]] = {
      case (g, mpTask) =>
        val nodeList = g.nodes.toIndexedSeq
        val startIndex = nodeList.map(_.toOuter).indexOf(mpTask.startPosition)
        val goalIndex = nodeList.map(_.toOuter).indexOf(mpTask.endPosition)
        println(s"NodeList ${nodeList.size}")

        val str = graphToNxString(g)

        val parseResult = (resultString: String) => resultString.
          substring(1).
          dropRight(1).
          split(", ").toList.
          map(_.toInt).map(nodeList.map(_.toOuter))

        val fileMap = Map(aStarTemplateFile -> aStarStartFile)
        val substMap = Map("$nodes$" -> str._1,
          "$nodeIndices$" -> str._2,
          "$edges$" -> str._3,
          "$startIndex$" -> startIndex.toString,
          "$goalIndex$" -> goalIndex.toString)
        val t = SubstitutionScheme(fileMap, substMap)

        val pWrapper = PythonWrapper.apply(t, aStarStartFile, parseResult)
        pWrapper.computeResult
    }

    val semanticType = cmp_graph_a_star_type
  }

  @combinator object GraphTsp {
    def apply: (Graph[List[Float], WUnDiEdge], MpTaskStartGoal) => Seq[List[Float]] = {
      case (g, mpTask) =>
        println("TSP")
        val nodeList = g.nodes.toIndexedSeq
        println("a11111234432")
        //val distanceInit: List[List[Float]] = List.fill(nodeList.size)(List.fill(nodeList.size)(-1.0f))

        def distanceForNode(n: g.NodeT): String = {
          val filteredG = g filter g.having(edge = e => e.hasSource(n) || e.hasTarget(n))
          println(s"Number of edges: ${filteredG.edges.size}")
          println(s"Edges: ${filteredG.edges}")

          val distance1 = n.incoming.map(e => e._1 -> e.weight).toMap
          val distance2 = n.outgoing.map(e => e._2 -> e.weight).toMap
          val distanceList = distance1 ++ distance2 ++ Map(n -> 0.0d)

          val adjacentList = nodeList.map(node =>
          if (distanceList.keySet.contains(node)) s"${distanceList(node)}" else "654654.0")
          print(s"adjacentList: $adjacentList")
          "[" + adjacentList.mkString(", ") + "]"
        }

        println("asd324234432")
        println(s"nodeList: $nodeList")
        println(s"nodeList.map(_.toOuter): ${nodeList.map(_.toOuter)}")
        println(s"mpTask.startPosition: ${mpTask.startPosition}")
        //val startIndex:Int = g.nodes.toOuter.toList.indexOf(mpTask.startPosition)  // TODO Graphbuild add start node
        println("asd324")
        val startIndex: Int = 0
        val locationString: String = s"""    data['locations'] = ${listToPythonArray(nodeList.map(_.toOuter).map(listToPythonArray).toList)}\n""" // TODO IndexedString test
        val distanceString: String = s"""    data['distances'] = ${listToPythonArray(nodeList.map(distanceForNode).toList)}\n"""
        val vehiclesString: String = s"""    data['num_vehicles'] = 1\n"""
        val startNodeString: String = s"""    data['depot'] = 0\n"""
        println("asd1")
        val s = locationString + distanceString + vehiclesString + startNodeString

        val parseResult = (resultString: String) => {
          println("attempting to parse")
          val index = resultString.indexOf(s"$startIndex -> ")
          println(s"index: $index")
          val list = resultString.substring(resultString.indexOf(s"$startIndex -> ")).
            split(" -> ").toList
          println(s"list: $list")

          val resultList = resultString.substring(resultString.indexOf(s"$startIndex -> ")).
            split(" -> ").toList.map(_.filterNot((x: Char) => x.isWhitespace).toInt)

          println("resultList: $resultList")
          resultList.map(nodeList.map(_.toOuter))
        }

        val testStr = """0
                        |Objective: 10522
                        |Route:
                        | 0 -> 7 -> 78 -> 24 -> 55 -> 54 -> 41 -> 31 -> 4 -> 15 -> 19 -> 74 -> 76 -> 36 -> 12 -> 27 -> 1 -> 71 -> 25 -> 56 -> 33 -> 26 -> 77 -> 79 -> 37 -> 44 -> 13 -> 73 -> 67 -> 45 -> 14 -> 21 -> 64 -> 65 -> 80 -> 30 -> 62 -> 17 -> 32 -> 39 -> 57 -> 61 -> 53 -> 60 -> 16 -> 29 -> 9 -> 3 -> 75 -> 10 -> 42 -> 69 -> 52 -> 40 -> 66 -> 38 -> 58 -> 6 -> 8 -> 72 -> 2 -> 28 -> 5 -> 68 -> 63 -> 49 -> 34 -> 35 -> 48 -> 46 -> 11 -> 70 -> 59 -> 20 -> 18 -> 50 -> 23 -> 51 -> 47 -> 43 -> 22 -> 0
                        |""".stripMargin
        println("testrun")
        print(s"asdtest")
        val fileMap = Map(tspTemplateLocation -> tspStartLocation)
        val substMap = Map("$substitute$" -> s)
        val t = SubstitutionScheme(fileMap, substMap)

        val pWrapper = PythonWrapper.apply(t, tspStartLocation, parseResult)
        pWrapper.computeResult
    }

    val semanticType = cmp_graph_tsp_type
  }

  @combinator object GraphMst {
    def apply: (Graph[List[Float], WUnDiEdge], MpTaskStartGoal) => Seq[List[Float]] = {
      (g, _) =>
        println("MST")
        val nodeList = g.nodes.toIndexedSeq
        println(s"NodeList ${nodeList.size}")

        def distanceForNode(n: g.NodeT): String = {
          val filteredG = g filter g.having(edge = _.hasSource(n))
          val nodeId = nodeList.indexOf(n)
          println(s"current id: $nodeId")
          println(s"Number of edges: ${filteredG.edges.size}")
          val adjacentList = filteredG.edges.map(a =>
            if (nodeList.indexOf(a._2) == nodeId)
              (nodeList.indexOf(a._1), a.weight)
            else
              (nodeList.indexOf(a._2), a.weight)
          )
          if (adjacentList.size == 3)
            println(s"adjacent list: $adjacentList")
          val str = adjacentList.toList.sortBy(_._1).foldLeft((0, "")) {
            case ((0, str), (b, c)) if str.isEmpty =>
              (b, (List.fill(b)("999.0") :+ c).mkString(", "))
            case ((index, str), (b, c)) if str.nonEmpty =>
              (b, (str +: List.fill(b - index - 1)("999.0") :+ c).mkString(", "))
          }
          val rString = str._2 + List.fill(nodeList.size - str._1 - 1)(", 999.0").mkString("")
          s"[$rString]"
        }

        val distanceString: String = s"""    ${listToPythonArray(nodeList.map(distanceForNode).toList)}\n"""

        def parseResult: String => List[List[Float]] = {
          resultString: String =>
            val resultList = decode[PathPreds](resultString).right.get
            println(s"Found IntList $resultList")
            val preds = resultList.preds

            def fullPath(a: Int): List[Int] =
              if (preds(a) == -9999)
                List(a)
              else
                fullPath(preds(a)) :+ a

            def addList(currentPath: List[Int], a: Int, b: Int): List[Int] =
              if (preds(b) == a)
                currentPath :+ b
              else {
                currentPath ++
                  (fullPath(a) diff fullPath(b)).reverse.tail ++
                  List((fullPath(a) intersect fullPath(b)).last) ++
                  (fullPath(b) diff fullPath(a))
              }


            println(s"nodes: ${resultList.nodes}")
            println(s"preds: ${resultList.preds}")
            val path = resultList.nodes.foldLeft((List.empty[Int], 0)) {
              case ((cl, a), b) if b != 0 =>
                (addList(cl, a, b), b)
              case _ => (List(0), 0)
            }

            println(s"computed path: $path")

            val l = path._1.reduce(
              (a: Int, b: Int) =>
                if (preds(b) != a && resultList.nodes.indexOf(b) - resultList.nodes.indexOf(a) != 1) -5 else 1
            )

            println(s"l: $l")

            val rList = path._1.map(i => nodeList(i).toOuter)

            println(s"return")
            println(s"rList: $rList")
            rList
        }

        val fileMap = Map(mstTemplateFile -> mstStartLocation)
        val substMap = Map("$substitute$" -> distanceString)
        val t = SubstitutionScheme(fileMap, substMap)

        val pWrapper = PythonWrapper.apply(t, mstStartLocation, parseResult)
        pWrapper.computeResult
    }

    val semanticType = cmp_graph_mst_type
  }


  def graphToNxString(g: Graph[List[Float], WUnDiEdge]): (String, String, String) = {
    print("g to nx string")
    val outerNodesSeq = g.nodes.toIndexedSeq.map(_.toOuter)
    val nodeStringTemp = outerNodesSeq.map(i =>
      s"""[${i.mkString(",")}]""").reduce(_ + ", " + _)
    val nodeString = s"[$nodeStringTemp]"
    println(s"nodeString: $nodeString")

    val nodeIndexString = s"[${outerNodesSeq.indices.map(i => s"$i").reduce(_ + ", " + _)}]"
    println(s"nodeString: $nodeIndexString")

    def indexOf(n: List[Float]): Int = outerNodesSeq.indexOf(n)

    val eString = g.edges.map(i => i.toOuter match {
      case WUnDiEdge(n1, n2, w) => s"            (${indexOf(n1)},${indexOf(n2)},$w)"
    }).mkString(",\n")
    print(s"eString: $eString")
    print("g to nx string")
    (nodeString, nodeIndexString, eString)
  }
}
