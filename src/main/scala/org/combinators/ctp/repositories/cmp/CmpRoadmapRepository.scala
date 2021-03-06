package org.combinators.ctp.repositories.cmp

import com.typesafe.scalalogging.LazyLogging
import org.combinators.cls.interpreter.combinator
import org.combinators.cls.types.Type
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.toplevel._
import scalax.collection.Graph
import scalax.collection.edge.WUnDiEdge

trait CmpRoadmapRepository extends CmpUtils with CmpCellCentroidsRepository with LazyLogging {

  trait RoadmapCombinatorTrait {
    def apply(
               neighbourFct: PolySceneCellSegmentation => IndexedSeq[(List[Int], Int, Int)],
               centroidFct: (PolySceneCellSegmentation, neighbourCellsNativeType) => List[List[List[Float]]],
               vertexFct: (PolySceneCellSegmentation, neighbourCellsNativeType) => List[List[List[Float]]],
               connxNodeFct: (PolySceneCellSegmentation, neighbourCellsNativeType) => List[List[List[Float]]],
               getEdges: (neighbourCellsNativeType, IndexedSeq[RmAuxDataNodes]) => IndexedSeq[WUnDiEdge[List[Float]]],
               addStartEnd: (PolySceneCellSegmentation, IndexedSeq[RmAuxDataNodes], MpTaskStartGoal) =>
                 (List[List[Float]], List[WUnDiEdge[List[Float]]])):
    (PolySceneCellSegmentation, MpTaskStartGoal) => PolySceneSegmentationRoadmap = {
      (scsc, startGoal) => {
        logger.debug(s"getting neighbours")
        val neighbours = neighbourFct(scsc)
        logger.debug(s"neighbours: $neighbours")
        val centroids = centroidFct(scsc, neighbours)
        logger.debug(s"centroids: $centroids")
        val cellVertices = vertexFct(scsc, neighbours)
        logger.debug(s"cellVertices: $cellVertices")
        val connxNodes = connxNodeFct(scsc, neighbours)
        logger.debug(s"connxNodes: $connxNodes")

        val auxCellData: IndexedSeq[RmAuxDataNodes] = (scsc.freeCells.indices zip centroids zip cellVertices zip
          connxNodes).map { case (((a, b), c), d) =>
          new RmAuxDataNodes {
            override val cellId: Int = a
            override val centroid: List[Float] = if (b.nonEmpty) b.head else List.empty[Float]
            override val vertices: List[List[Float]] = c
            override val connxPoints: List[List[Float]] = d
          }
        }
        logger.debug(s"graph build prep")
        val startEndNodesEdges = addStartEnd(scsc, auxCellData, startGoal)
        logger.debug(s"1")
        val n1: List[List[Float]] = auxCellData.map(i => i.vertices ++ i.connxPoints :+ i.centroid).reduce(_ ++ _)
        logger.debug(s"2")
        val n2 = startEndNodesEdges._1
        logger.debug(s"3")
        val edges = getEdges(neighbours, auxCellData) ++ startEndNodesEdges._2
        logger.debug(s"building graph")
        val roadmap = Graph.from(n1 ++ n2, edges)
        logger.debug(s"rm built")
        scsc.withRoadmap(roadmap)
        logger.debug(s"rm out")
        scsc.withRoadmap(roadmap)
      }
    }

    val semanticType: Type
  }

  @combinator object RoadmapCombinator extends RoadmapCombinatorTrait {
    val semanticType =
      rmc_neighbourFct_type :&: dimensionality_var =>:
        cmd_centroidFct_type :&: rmc_usingCentroids_var :&: rmc_centroidFct_var :&: sd_cell_type_var :&: dimensionality_var =>:
        rmc_cellNodeAddFct_type :&: rmc_cellNodeAddFct_var =>:
        rmc_connectorNodeFct_type :&: rmc_connectorNodes_var =>:
        rmc_edgeAdd_type :&: rmc_cellGraph_var =>:
        rmc_startGoalFct_type :&: rmc_startGoalFct_var :&: rmc_cellGraph_var :&: dimensionality_var =>:
        cmp_cell_graph_fct_type :&: rmc_cellNodeAddFct_var :&: rmc_startGoalFct_var :&: rmc_usingCentroids_var :&:
          rmc_centroidFct_var :&: sd_cell_type_var :&: rmc_cellGraph_var :&: rmc_connectorNodes_var :&: dimensionality_var
  }

  @combinator object RoadmapCombinatorTax extends RoadmapCombinatorTrait {
    val semanticType =
      rmc_neighbourFct_type =>:
        cmd_centroidFct_type =>:
        rmc_cellNodeAddFct_type =>:
        rmc_connectorNodeFct_type =>:
        rmc_edgeAdd_type =>:
        rmc_startGoalFct_type =>:
        cmp_cell_graph_fct_type
  }

  @combinator object Neighbours2D {
    def apply: PolySceneCellSegmentation => IndexedSeq[(List[Int], Int, Int)] = { csc =>
      logger.debug("starting nb 2d")
      println(s"freecells: ${csc.freeCells}")

      val segmentationLines = for (freeCell1 <- csc.freeCells.indices;
                                   freeCell2 <- csc.freeCells.indices;
                                   i = csc.freeCells(freeCell1).intersect(csc.freeCells(freeCell2))
                                   if i.nonEmpty && freeCell1 != freeCell2)
        yield (i, freeCell1, freeCell2)
      logger.debug("yielded")
      val filteredLines = segmentationLines.filter(i => i._2 < i._3 && i._1.size > 1)
      logger.debug("filtered")
      filteredLines
    }

    val semanticType = rmc_neighbourFct_type :&: dimensionality_two_d_t
  }

  @combinator object Neighbours3D {
    def apply: PolySceneCellSegmentation => IndexedSeq[(List[Int], Int, Int)] = { csc =>
      val segmentationLines = for (freeCell1 <- csc.freeCells.indices;
                                   freeCell2 <- csc.freeCells.indices;
                                   i = csc.freeCells(freeCell1).intersect(csc.freeCells(freeCell2))
                                   if i.nonEmpty && freeCell1 != freeCell2)
        yield (i, freeCell1, freeCell2)
      val filteredLines = segmentationLines.filter(i => i._2 < i._3 && i._1.size > 2)
      filteredLines
    }

    val semanticType = rmc_neighbourFct_type :&: dimensionality_three_d_t
  }

  @combinator object WithCellVertices {
    def apply(): (PolySceneCellSegmentation, neighbourCellsNativeType) => List[List[List[Float]]] =
      (sSeg, _) => sSeg.freeCells.map(vertexIndices => vertexIndices.map {
        vId => sSeg.vertices(vId)
      })

    val semanticType = rmc_cellNodeAddFct_type :&: rmc_cna_withCellNodes_type
  }

  @combinator object WithoutNewNodes {
    def apply(): (PolySceneCellSegmentation, neighbourCellsNativeType) => List[List[List[Float]]] =
      (sSeg, _) => sSeg.freeCells.map(_ => List.empty[List[Float]])

    val semanticType = cmd_centroidFct_type :&: rm_withoutCentroids_type :&:
      rmc_cellNodeAddFct_type :&: rmc_cna_withoutCellNodes_type :&:
      rmc_connectorNodeFct_type :&: rmc_cn_withoutConnectorNodes
  }

  @combinator object WithConnectorNodes {
    def apply(): (PolySceneCellSegmentation, neighbourCellsNativeType) => List[List[List[Float]]] =
      (scrm, neighbours) => {
        val list: List[(List[Float], Int, Int)] = {
          neighbours.map {
            case (common_points: List[Int], leftCellId: Int, rightCellId: Int) =>
              if (common_points.size != 2)
                logger.warn(s"WARN: size of common vertices of neighbour cells is " +
                  s"${common_points.size} should be 2. Vertices: " +
                  s"${common_points.map(scrm.vertices)}")
              val v1 = scrm.vertices(common_points.head)
              val v2 = scrm.vertices(common_points.last)
              val connxPoint = lineSegmentCenterPoint(v1, v2)
              (connxPoint, leftCellId, rightCellId)
          }.toList
        }
        scrm.freeCells.indices.map(i => list.filter { case (_, id1, id2) => id1 == i || id2 == i }.map(_._1)).toList
      }

    val semanticType = rmc_connectorNodeFct_type :&: rmc_cn_withConnectorNodes
  }


  @combinator object EdgesCentroidOnly {
    def apply(): (neighbourCellsNativeType, IndexedSeq[RmAuxDataNodes]) => IndexedSeq[WUnDiEdge[List[Float]]] = {
      (neighbours, cellNodes) => {
        val centroids = cellNodes.map(_.centroid)
        val n2 = neighbours.map { case (_, id1, id2) => (centroids(id1), centroids(id2)) }
        n2.map { case (v1, v2) => WUnDiEdge(v1, v2)(distance(v1, v2)) }
      }
    }

    val semanticType = rmc_edgeAdd_type :&: rmc_cg_centroidsOnly
  }

  @combinator object EdgesCentroidToCellVertices {
    def apply(): (neighbourCellsNativeType, IndexedSeq[RmAuxDataNodes]) => IndexedSeq[WUnDiEdge[List[Float]]] = {
      (_, cellNodes: IndexedSeq[RmAuxDataNodes]) =>
        cellNodes.flatMap(cn => edgesVertexToVertexlist(cn.centroid, cn.vertices) ++
          edgesVertexToVertexlist(cn.centroid, cn.vertices ++ cn.connxPoints))
    }

    val semanticType = rmc_edgeAdd_type :&: rmc_cg_centroidCellVertices
  }

  @combinator object EdgesAllCellVertices {
    def apply(): (neighbourCellsNativeType, IndexedSeq[RmAuxDataNodes]) => IndexedSeq[WUnDiEdge[List[Float]]] = {
      (_, cellNodes: IndexedSeq[RmAuxDataNodes]) =>
        cellNodes.flatMap(cn => edgesVertexToVertexlist(cn.centroid, cn.vertices) ++
          edgesVertexlistToVertexlist(cn.vertices ++ cn.connxPoints :+ cn.centroid, cn.connxPoints))
    }

    val semanticType = rmc_edgeAdd_type :&: rmc_cg_allVertices
  }


  @combinator object NodesEdgesStartEndCentroid {
    def apply(): (PolySceneCellSegmentation, IndexedSeq[RmAuxDataNodes], MpTaskStartGoal) => (List[List[Float]], List[WUnDiEdge[List[Float]]]) = {
      case (sSeg: PolySceneCellSegmentation, cellNodes: IndexedSeq[RmAuxDataNodes], startGoal: MpTaskStartGoal) =>
        val (startIndex, endIndex) = findFreeCell2D(sSeg, startGoal)
        val edges = edgesVertexToVertex(cellNodes(startIndex).centroid, startGoal.startPosition) ++
          edgesVertexToVertex(cellNodes(endIndex).centroid, startGoal.endPosition)
        val nodes = List(startGoal.startPosition, startGoal.endPosition)

        (nodes, edges)
    }

    val semanticType = rmc_startGoalFct_type :&: rmc_startGoal_cellbased_type :&: rmc_cg_centroidsOnly :&:
      rmc_cg_centroidCellVertices :&: dimensionality_two_d_t
  }

  @combinator object NodesEdgesStartEndCellBased {
    def apply(): (PolySceneCellSegmentation, IndexedSeq[RmAuxDataNodes], MpTaskStartGoal) =>
      (List[List[Float]], List[WUnDiEdge[List[Float]]]) = {
      case (sSeg: PolySceneCellSegmentation, cellNodes: IndexedSeq[RmAuxDataNodes], startGoal: MpTaskStartGoal) =>
        logger.debug("aaa")
        val (startIndex, endIndex) = findFreeCell2D(sSeg, startGoal)
        logger.debug("bbb")
        val edges = edgesVertexToVertexlist(startGoal.startPosition, cellNodes(startIndex).vertices ++
          cellNodes(startIndex).connxPoints :+ cellNodes(startIndex).centroid) ++
          edgesVertexToVertexlist(startGoal.endPosition, cellNodes(endIndex).vertices ++
            cellNodes(endIndex).connxPoints :+ cellNodes(endIndex).centroid)
        logger.debug("ccc")
        val nodes = List(startGoal.startPosition, startGoal.endPosition)
        logger.debug("ddd")

        (nodes, edges)
    }

    val semanticType = rmc_startGoalFct_type :&: rmc_startGoal_cellbased_type :&:
      rmc_cg_allVertices :&: dimensionality_two_d_t
  }


  @combinator object NodesEdgesStartEndNearest {
    def apply(): (PolySceneCellSegmentation, IndexedSeq[RmAuxDataNodes], MpTaskStartGoal) =>
      (List[List[Float]], List[WUnDiEdge[List[Float]]]) = {
      case (_: PolySceneCellSegmentation, cellNodes: IndexedSeq[RmAuxDataNodes], mpTask: MpTaskStartGoal) =>
        logger.debug(s"startingNOdesEdgesStartEndNEarest")

        val startPoint = mpTask.startPosition
        val endPoint = mpTask.endPosition

        logger.debug(s"startingNOdesEdgesStartEndNEarest2")
        logger.debug(s"cellNodes: ${cellNodes}")

        val a = cellNodes.map(i => i.vertices ++ i.connxPoints :+ i.centroid)
        println(s"map1: ${a}")
        val b = a.map(
          allCellNodes => allCellNodes.map(cellNode => (cellNode, distance(cellNode, startPoint),
            distance(cellNode, endPoint))))
        logger.debug(s"map2: ${b}")

        val startDistances = cellNodes.map(i => i.vertices ++ i.connxPoints :+ i.centroid).map(
          allCellNodes => allCellNodes.map(cellNode => (cellNode, distance(cellNode, startPoint),
            distance(cellNode, endPoint)))).reduce(_ ++ _)
        logger.debug(s"startDistances: $startDistances")
        val minStartNode = startDistances.minBy(_._2)
        logger.debug(s"minstartFound")
        val minEndNode = startDistances.minBy(_._3)
        logger.debug(s"minEndFound")
        val startEdge = WUnDiEdge(minStartNode._1, startPoint)(minStartNode._2)
        val endEdge = WUnDiEdge(minEndNode._1, endPoint)(minEndNode._3)
        (List(startPoint, endPoint), List(startEdge, endEdge))
    }

    val semanticType = rmc_startGoalFct_type :&: rmc_startGoal_nn_type :&: rmc_cellGraph_var :&: dimensionality_var
  }

}
