package org.combinators.ctp.repositories.toplevel

import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, Materializer}
import io.circe.{Encoder, Json}
import io.circe.generic.auto._
import io.circe.syntax._
import scalax.collection.Graph
import scalax.collection.edge.WUnDiEdge

trait EncodeImplicits {
  implicit val encodeGraph: Encoder[Graph[List[Float], WUnDiEdge]] = (a: Graph[List[Float], WUnDiEdge]) => Json.obj(
    ("nodes", Json.fromValues(a.nodes.toOuter.map(i =>
      Json.fromValues(i.map(Json.fromFloat(_).get))))),
    ("edges", Json.fromValues(a.edges.toOuter.map(i =>
      Json.fromValues(List(
        Json.fromValues(i.head.map(Json.fromFloat(_).get)),
        Json.fromValues(i.last.map(Json.fromFloat(_).get))))))),
    ("weights", Json.fromValues(a.edges.toOuter.map(i =>
      Json.fromFloat(i.weight.toFloat).get))))


  implicit val encodePath: Encoder[(Seq[List[Float]], Seq[WUnDiEdge[List[Float]]], Float)] = {
    case (a, b, c) => Json.obj(
      ("pnodes", Json.fromValues(a.map(i => Json.fromValues(i.flatMap(Json.fromFloat))))),
      ("pedges", Json.fromValues(b.map { edge: WUnDiEdge[List[Float]] =>
        edge match {
          case WUnDiEdge(from, to, _) => Json.fromValues(List(
            Json.fromValues(from.flatMap(Json.fromFloat)),
            Json.fromValues(to.flatMap(Json.fromFloat))))
        }})),
      ("plength", Json.fromFloat(c).get)
    )
  }
}


