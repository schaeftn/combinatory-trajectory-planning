package org.combinators.ctp.repositories.temp

import org.combinators.ctp.repositories.toplevel.{PolySceneCellSegmentation, PolySceneLineSegmentation}

object VerticalCellToLineSegmentation {
  //Cells must have vertical lines for this approach
  def apply(polyScene: PolySceneCellSegmentation): PolySceneLineSegmentation = {
    val topBoundary: Float = polyScene.boundaries(1) / 2
    val bottomBoundary: Float = -topBoundary

    val topVertices: List[Int] = polyScene.vertices.filter(vertex => vertex(1) == topBoundary)
      .map(vertex => polyScene.vertices.indexOf(vertex))

    val bottomVertices: List[Int] = polyScene.vertices.filter(vertex => vertex(1) == bottomBoundary)
      .map(vertex => polyScene.vertices.indexOf(vertex))

    val cellVertices: List[List[List[Float]]] = polyScene.freeCells.map(i => i.map(polyScene.vertices))

    val vertexLineList: List[List[List[Float]]] = cellVertices.flatMap(cell => {
      val sortedCell: List[List[Float]] = cell.sortWith((v1, v2) => v1.head <= v2.head)
      val leftLine: List[List[Float]] = sortedCell.takeWhile(vertex => vertex.head == sortedCell.head.head)
      val withoutLeft = sortedCell.diff(leftLine)
      val rightLine: List[List[Float]] = withoutLeft.takeWhile(vertex => vertex.head == withoutLeft.head.head)
      (leftLine.nonEmpty, rightLine.nonEmpty) match {
        case (false, false) =>
          println("This case should not be possible!")
          Nil
        case (false, true) => List(rightLine)
        case (true, false) => List(leftLine)
        case (true, true) => List(leftLine, rightLine)
      }
    })

    val lineList: List[List[Int]] = vertexLineList.map(vertexList =>
      vertexList.map(vertex => polyScene.vertices.indexOf(vertex)))

    PolySceneLineSegmentation(polyScene.vertices, polyScene.obstacles, polyScene.boundaries, topVertices, bottomVertices, lineList)
  }

}