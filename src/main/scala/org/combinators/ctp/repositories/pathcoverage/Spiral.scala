package org.combinators.ctp.repositories.pathcoverage

import org.combinators.ctp.repositories.toplevel.PathCoverageStepConfig

trait Spiral {
  val minRadius: Double
  val maxRadius: Double
  val midPoint: List[Double]
  val ae: Float
  val config: PathCoverageStepConfig

  //    if (Math.abs(Math.sin(t)) < 0.5)
//    0.5 else Math.abs(Math.sin(t))

  def morphingFunction(t: Double) = 1.0

  lazy val linearRadiusGrowFactor = (maxRadius - minRadius) + ae // divided by Parameter interval 1.0f
  lazy val omega: Double = ((linearRadiusGrowFactor / ae)) * math.Pi * 2 // rad per parameter 1.0f 100*pi = 50 circles
  lazy val linearRadiusGrowFactorOh = maxRadius * math.Pi * 2
  lazy val resolution = 800

  def direction(t: Double): List[Double] = List(math.cos(omega * t), math.sin(omega * t), 0.0) //TODO
  def radius(t: Double) = List(maxRadius, minRadius + (linearRadiusGrowFactor) * t * morphingFunction(omega*t) ).min
  def position(t: Double):List[Float] = (midPoint zip direction(t).map(i => i * radius(t))).
    map { case (mpCoord, vecCoord) => (mpCoord + vecCoord).toFloat }

  def discretePath: List[List[Float]] = (0 until resolution).map(i => position(1.0f * i / resolution)).filter(_.nonEmpty).toList
}

object RunSpiral extends App {
//  val spiral = new Spiral {}
//  val path = spiral.discretePath.map(_.map(_.toFloat))
//  println(path)
//  ToAkka(path)
//  println("done")
}