package org.combinators.ctp.repositories.pathcoverage

trait Spiral {
  val minRadius: Double = 0.1
  val maxRadius: Double = 50.0
  val midPoint: List[Double] = List(0.0, 0.0, 0, 0)

  def morphingFunction(t:Double) =
    if (Math.abs(Math.sin(t)) < 0.5)
    0.5 else Math.abs(Math.sin(t))

  val ae = 4.0f
  val linearRadiusGrowFactor = (maxRadius - minRadius)+ae // divided by Parameter interval 1.0f
  val omega: Double = ((linearRadiusGrowFactor / ae)) * math.Pi * 2 // rad per parameter 1.0f 100*pi = 50 circles
  val linearRadiusGrowFactorOh = maxRadius * math.Pi * 2
  val resolution = 10000

  def direction(t: Double): List[Double] = List(math.cos(omega * t), math.sin(omega * t), 0.0) //TODO
  def radius(t: Double) = List(maxRadius, minRadius + (linearRadiusGrowFactor) * t * morphingFunction(omega*t) ).min
  def position(t: Double):List[Double] = (midPoint zip direction(t).map(i => i * radius(t))).
    map { case (mpCoord, vecCoord) => mpCoord + vecCoord }

  def discretePath: List[List[Double]] = (0 until resolution).map(i => position(1.0f * i / resolution)).filter(_.nonEmpty).toList
}

object RunSpiral extends App {
  val spiral = new Spiral {}
  val path = spiral.discretePath.map(_.map(_.toFloat))
  println(path)
  ToAkka(path)
  println("done")
}