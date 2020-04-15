package org.combinators.ctp.repositories.toplevel

import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, Materializer}
import io.circe.{Decoder, Encoder, Json}
import org.combinators.ctp.repositories.graphsearch.TspData
import org.combinators.ctp.repositories.scene.PolySceneSegmentationRoadmapPath
import scalax.collection.Graph
import scalax.collection.edge.WUnDiEdge
import scalax.collection.edge.Implicits._

trait AkkaImplicits extends EncodeImplicits{
  implicit val as: ActorSystem = ActorSystem()
  implicit val am: Materializer = ActorMaterializer()
}


