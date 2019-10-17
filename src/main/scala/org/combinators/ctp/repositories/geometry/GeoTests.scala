package org.combinators.ctp.repositories.geometry

import org.combinators.ctp.repositories._
import math._

object GeoTests extends App{
  def test: (vertexType, vertexType) => Float =
    (a:vertexType, b:vertexType) => {
      math.sqrt(
        (a zip b) map {
          case (x: Float, y: Float) => pow(x - y, 2)
        } reduce { (c, d) => abs(c) + abs(d) }).toFloat
    }
  val p1: vertexType = List(20.0f,30.0f,40.0f)
  val p2: vertexType = List(80f,44f,90f)

  println(test(p1,p2))
}