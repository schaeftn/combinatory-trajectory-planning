package org.combinators.ctp.repositories.boundingvolumes

import java.io.{ByteArrayInputStream, DataInputStream, InputStream}
import java.util
import java.util.stream.DoubleStream

import com.dreizak.miniball.highdim.Miniball
import com.dreizak.miniball.model.PointSetUtils
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.geometricrepresentation.{vertexArrayType, vertexPairType, vertexType}
import org.combinators.ctp.repositories.protocol._
import com.dreizak.miniball.model.ArrayPointSet

object foo extends App {


  def getBs: UnityMeshData => (vertexType, Float) = vertexData => {
    val vArray = vertexData.vertexArray.toArray

    val (d, n) = (3, vertexData.vertexArray.size/3)

    val pts: ArrayPointSet = new ArrayPointSet(3, n)

    for (
      j <- 0 to d-1;
      i <- 0 to n-1
    ) yield pts.set(i, j, vArray(j * i))

    val mb: Miniball = new Miniball(pts)
    ((mb.center() map (_.toFloat)).toList, mb.radius().toFloat)
  }
  def getBB: UnityMeshData => vertexPairType = { a: UnityMeshData =>
    val va = a.vertexArray

    def getXyz(v: vertexArrayType): vertexArrayType = v.zipWithIndex.filter {
      case (_, b) if (b % 3) == 0 => true
      case _ => false
    } map (_._1)

    def minmax: vertexArrayType => (Float, Float) = { (a: vertexArrayType) => (a.max, a.min) }

    println("xArray: "+ getXyz(va))
    println("yArray: "+ getXyz(va.tail))
    println("zArray: "+ getXyz(va.drop(2)))

    val (maxX, minX) = minmax(getXyz(va))
    val (maxY, minY) = minmax(getXyz(va.tail))
    val (maxZ, minZ) = minmax(getXyz(va.drop(2)))

    (List(minX, minY, minZ), List(maxX, maxY, maxZ))
  }

  val asd = List(0.42426407, -0.5, 0.28284273, 0.2828427, -0.5, 0.42426407, 0.42426407, 0.5, 0.28284273, 0.2828427, 0.5, 0.42426407, -0.2828427, 0.5, -0.42426407, -0.42426407, 0.5, -0.28284273, -0.2828427, -0.5, -0.42426407, -0.42426407, -0.5, -0.28284273, 0.42426407, 0.5, 0.28284273, 0.2828427, 0.5, 0.42426407, -0.2828427, 0.5, -0.42426407, -0.42426407, 0.5, -0.28284273, -0.2828427, -0.5, -0.42426407, 0.42426407, -0.5, 0.28284273, 0.2828427, -0.5, 0.42426407, -0.42426407, -0.5, -0.28284273, 0.2828427, -0.5, 0.42426407, 0.2828427, 0.5, 0.42426407, -0.42426407, 0.5, -0.28284273, -0.42426407, -0.5, -0.28284273, -0.2828427, -0.5, -0.42426407, -0.2828427, 0.5, -0.42426407, 0.42426407, 0.5, 0.28284273, 0.42426407, -0.5, 0.28284273)
val foo = new UnityMeshData(List(),asd map (a => a.toFloat))

  println(getBB(foo))
  println("")
  println(getBs(foo))
}
