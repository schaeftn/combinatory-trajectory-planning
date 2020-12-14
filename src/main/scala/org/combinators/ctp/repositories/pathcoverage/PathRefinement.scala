package org.combinators.ctp.repositories.pathcoverage

import com.typesafe.scalalogging.LazyLogging
import org.combinators.ctp.repositories.pathcoverage.RunTrochoidalPath.trochPrimitive
import org.combinators.ctp.repositories.scene.SceneUtils
import org.combinators.ctp.repositories.toplevel.Cnc2DModel
import org.locationtech.jts.geom.{Coordinate, Geometry, LineString, MultiLineString}
import org.locationtech.jts.linearref.LengthIndexedLine


trait PathRefinement extends LazyLogging with JtsUtils {
  def refine(list: List[List[Float]], maxClearanceBetweenPoints: Double): List[List[Float]] = {
    val coordList = list.map(asCoordinate)
    val refinedCoordList = coordList.foldLeft(List.empty[Coordinate]) {
      case (accList, c) if (accList.isEmpty || accList.last.distance(c) <= maxClearanceBetweenPoints) => accList :+ c
      case (accList, c) => accList ++ splitLine(accList.last, c, maxClearanceBetweenPoints.toDouble)
    }
    asFloatList(refinedCoordList.toArray)
  }

  def refineMinClearance(l: List[List[Float]], minClearance: Double): List[List[Float]] = {
    val coordList = l.map(asCoordinate)
    val foldedList = coordList.foldLeft(List.empty[Coordinate]) {
      case (accList, newCoord) if (accList.isEmpty) => accList :+ newCoord
      case (accList, newCoord) =>
        if (accList.last.distance(newCoord) > minClearance) accList :+ newCoord else accList
    }
    asFloatList(foldedList.toArray)
  }

  def splitLine(c1: Coordinate, c2: Coordinate, maxClear: Double): List[Coordinate] = {
    val dist = c1.distance(c2)
    val numberSteps = Math.ceil(dist / maxClear).toInt

    val indLs = new LengthIndexedLine(getNewLineString(c1, c2))
    val coordList = (0 to numberSteps).map(i => indLs.extractPoint(dist / numberSteps * i)).toList
    coordList
  }

  def segmentTime(c1: List[Float], c2: List[Float]): List[Float] = {
    val accTime = c1.last
    val velocity = c1.dropRight(1).last * 1000 / 60 // m/min -> mm/s
    val distance = asCoordinate(c1).distance(asCoordinate(c2))
    val time = velocity * distance
    val newTime = accTime + time.toFloat
    c2 :+ newTime
  }

  def calcTime(l: List[List[List[Float]]]): List[List[List[Float]]] = l.map(path =>
    path.tail.foldLeft(List(path.head :+ 0.0f)) {
      case (accPath, newV) => accPath :+ segmentTime(accPath.last, newV)
    })

  def pathTimes(l: List[List[List[Float]]]): List[Float] = l.map(path =>
    singlePathTime(path))

  def singlePathTime(l: List[List[Float]]): Float = if (l.isEmpty) 0.0f else l.last.last

  def completeTime(l: List[List[List[Float]]]): Float = pathTimes(l).sum

}
