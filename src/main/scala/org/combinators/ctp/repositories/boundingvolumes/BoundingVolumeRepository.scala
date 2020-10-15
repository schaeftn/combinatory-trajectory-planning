package org.combinators.ctp.repositories.boundingvolumes

import com.dreizak.miniball.highdim.Miniball
import com.dreizak.miniball.model.ArrayPointSet
import org.combinators.cls.interpreter.combinator
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.toplevel.UnityMeshData

trait BoundingVolumeRepository {
  @combinator object UnityDataBoundingBox {
    def apply: UnityMeshData => (List[Float],List[Float] ) = { a: UnityMeshData =>
      val va = a.vertexArray

      def getXyz(v: List[Float]): List[Float] = v.zipWithIndex.filter {
        case (_, b) if (b % 3) == 0 => true
        case _ => false} map (_._1)

      def minmax: List[Float] => (Float, Float) = { a: List[Float] => (a.max, a.min) }

      val (maxX, minX) = minmax(getXyz(va))
      val (maxY, minY) = minmax(getXyz(va.tail))
      val (maxZ, minZ) = minmax(getXyz(va.drop(2)))

      (List(minX, minY, minZ), List(maxX, maxY, maxZ))
    }

    val semanticType = bv_bounding_box
  }

  @combinator object MiniBall {
    def apply: UnityMeshData => (List[Float], Float) = {
      vertexData =>
        //Magic number 5: sphere computation seems to be sensitive to # of digits
        val vSeq = vertexData.vertexArray.map { a => BigDecimal(a).setScale(5, BigDecimal.RoundingMode.HALF_UP).toFloat }

        val n = vSeq.size / 3
        val pts: ArrayPointSet = new ArrayPointSet(3, n)

        for (
          j <- 0 to 2;
          i <- 0 until n
        ) yield pts.set(i, j, vSeq(3*i + j))

        val mb: Miniball = new Miniball(pts)
        ((mb.center() map (_.toFloat)).toList, mb.radius().toFloat)
    }

     val semanticType = bv_bounding_sphere
  }


  //TODO SciPy Convex Hull

//  @combinator object pythonBVH {
//    def apply = ???
//    python call, determine BVH func signature
//    val semanticType = ???
//  }
}