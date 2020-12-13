package org.combinators.ctp.repositories.pathcoverage

import com.typesafe.scalalogging.LazyLogging
import org.combinators.ctp.repositories.pathcoverage.RunTrochoidalPath.trochPrimitive
import org.combinators.ctp.repositories.scene.SceneUtils
import org.combinators.ctp.repositories.toplevel.Cnc2DModel
import org.locationtech.jts.geom.{Coordinate, Geometry, LineString, MultiLineString}
import org.locationtech.jts.linearref.LengthIndexedLine


trait PathRefinement extends LazyLogging with JtsUtils {
  def refine(list:List[List[Float]], maxClearanceBetweenPoints: Double): List[List[Float]] ={
   val coordList = list.map(asCoordinate)
   val refinedCoordList = coordList.foldLeft(List.empty[Coordinate]){
      case (accList, c) if (accList.isEmpty || accList.last.distance(c) <= maxClearanceBetweenPoints) => accList :+ c
      case (accList, c) => accList ++ splitLine(accList.last, c, maxClearanceBetweenPoints.toDouble)
    }
    asFloatList(refinedCoordList.toArray)
}

  def splitLine(c1:Coordinate, c2:Coordinate, maxClear : Double): List[Coordinate] = {
  val dist = c1.distance(c2)
    val numberSteps = Math.ceil(dist / maxClear).toInt

    val indLs = new LengthIndexedLine(getNewLineString(c1,c2))
    val coordList = (0 to numberSteps).map(i => indLs.extractPoint(dist / numberSteps * i)).toList
    coordList
  }
}
