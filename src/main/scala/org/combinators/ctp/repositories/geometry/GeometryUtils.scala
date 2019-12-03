package org.combinators.ctp.repositories.geometry

import org.apache.commons.math3.ml.distance.EuclideanDistance
import org.combinators.cls.interpreter._
import org.combinators.cls.types.Kinding
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.scene.{MqttCubeData, MqttTransform}

import scala.math.{pow, sqrt}

trait GeometryUtils {
  /*Works for 2D and 3D*/
  def distance(v1: List[Float], v2: List[Float]): Float = {
    if(v1.size != v2.size)
      println(s"Warning: computation of distance requires same size of lists. v1.size: ${v1.size}, v2.size: ${v2.size}")
    sqrt((v1 zip v2).map { case (x, y) => pow(y - x, 2) }.sum).toFloat
  }

  def commons_distance(v1: List[Float], v2: List[Float]): Float = {
    val t = new EuclideanDistance()
    t.compute(v1.map(_.toDouble).toArray, v2.map(_.toDouble).toArray).toFloat
  }
}