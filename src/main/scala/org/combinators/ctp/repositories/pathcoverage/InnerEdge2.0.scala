package org.combinators.ctp.repositories.pathcoverage

import com.typesafe.scalalogging.LazyLogging
import org.combinators.ctp.repositories.pathcoverage.RunTrochoidalPath.trochPrimitive
import org.combinators.ctp.repositories.scene.SceneUtils
import org.combinators.ctp.repositories.toplevel.Cnc2DModel
import org.locationtech.jts.geom.{Geometry, LineString, MultiLineString}


trait InnerEdge2 extends LazyLogging with JtsUtils {
  val model: Cnc2DModel
  lazy val p1 : Geometry = model.rest.head

  lazy val selectBoundaryLine: Option[LineString] = {
    val intersectionLineCandidate: Geometry = model.machinedMultiPolygon.intersection(p1)
    if (intersectionLineCandidate.isEmpty) None
    else
      intersectionLineCandidate.getGeometryType match {
        case "LineString" => Some(intersectionLineCandidate.asInstanceOf[LineString])
        case "MultiLineString" =>
          Some(multiGeoToGeoList(intersectionLineCandidate.asInstanceOf[MultiLineString]).
            maxBy(_.getLength).asInstanceOf[LineString])
      }
  }

  lazy val directionY: Boolean = selectBoundaryLine.map(_.getEnvelopeInternal).forall(i => i.getWidth > i.getHeight)

  def getSteps(currentModel: Cnc2DModel): List[List[List[Float]]]

}

object InnerEdge2Test extends App {

}