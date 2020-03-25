package org.combinators.ctp.repositories.geometry

import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.scene.Scene

import math._
import io.circe.generic.JsonCodec
import io.circe.parser.decode
import io.circe.generic.auto._
import io.circe.syntax._
import org.apache.commons.geometry.euclidean.threed.{AffineTransformMatrix3D, Vector3D}
import org.apache.commons.geometry.euclidean.threed.rotation.QuaternionRotation
import org.apache.commons.math3.geometry.euclidean.threed.Rotation


object GeoTests extends App{
/*  def test: (vertexType, vertexType) => Float =
    (a:vertexType, b:vertexType) => {
      math.sqrt(
        (a zip b) map {
          case (x: Float, y: Float) => pow(x - y, 2)
        } reduce { (c, d) => abs(c) + abs(d) }).toFloat
    }
  val p1: vertexType = List(20.0f,30.0f,40.0f)
  val p2: vertexType = List(80f,44f,90f)

  println(test(p1,p2))*/

  //Rot scaling == scaling rot?


  def removeScaling(tMatrix: AffineTransformMatrix3D): (AffineTransformMatrix3D, List[Float]) = {
    val arr: Array[Double] = tMatrix.toArray
    val te:List[Double] = List(arr(0), arr(5), arr(10))
    val test: List[Double]  = te.map(1.0/_)

    val shouldBeOne: AffineTransformMatrix3D = tMatrix.scale(test(0), test(1), test(2))
   // print(s"shouldBeOne: ${shouldBeOne}")
    (shouldBeOne, te.map(_.toFloat))
  }

/*

  def removeScaling(tMatrix: List[List[Float]]): (List[List[Float]], List[Float]) =
    List(tMatrix(0)(0), tMatrix(1)(1), tMatrix(2)(2))
*/
class AffineTransformMatrix3DAccessor(val affineTransform: AffineTransformMatrix3D) {
  self =>
  val arr = affineTransform.toArray.map(_.toFloat)
  val _m00 = arr(0)
  val _m01 = arr(1)
  val _m02 = arr(2)
  val _m03 = arr(3)

  val _m10 = arr(4)
  val _m11 = arr(5)
  val _m12 = arr(6)
  val _m13 = arr(7)

  val _m20 = arr(8)
  val _m21 = arr(9)
  val _m22 = arr(10)
  val _m23 = arr(11)
}

  def orthogonalRotationMatrixToQuaternion(arr: Array[Double]): QuaternionRotation = orthogonalRotationMatrixToQuaternion(
    arr(0),arr(1),arr(2),
    arr(4),arr(5),arr(6),
    arr(8),arr(9),arr(10),
  )

  def orthogonalRotationMatrixToQuaternion(m00: Double, m01: Double, m02: Double,
                                           m10: Double, m11: Double, m12: Double,
                                           m20: Double, m21: Double, m22: Double): QuaternionRotation =
  { // reference formula:
    // http://www.euclideanspace.com/maths/geometry/rotations/conversions/matrixToQuaternion/
    // The overall approach here is to take the equations for converting a quaternion to
    // a matrix along with the fact that 1 = x^2 + y^2 + z^2 + w^2 for a normalized quaternion
    // and solve for the various terms. This can theoretically be done using just the diagonal
    // terms from the matrix. However, there are a few issues with this:
    // 1) The term that we end up taking the square root of may be negative.
    // 2) It's ambiguous as to whether we should use a plus or minus for the value of the
    //    square root.
    // We'll address these concerns by only calculating a single term from one of the diagonal
    // elements and then calculate the rest from the non-diagonals, which do not involve
    // a square root. This solves the first issue since we can make sure to choose a diagonal
    // element that will not cause us to take a square root of a negative number. The second
    // issue is solved since only the relative signs between the quaternion terms are important
    // (q and -q represent the same 3D rotation). It therefore doesn't matter whether we choose
    // a plus or minus for our initial square root solution.
    val trace = m00 + m11 + m22
    var w = .0
    var x = .0
    var y = .0
    var z = .0
    if (trace > 0.0) { // let s = 4*w
      val s = 2.0 * Math.sqrt(1.0 + trace)
      val sinv = 1.0 / s
      x = (m21 - m12) * sinv
      y = (m02 - m20) * sinv
      z = (m10 - m01) * sinv
      w = 0.25 * s
    }
    else if ((m00 > m11) && (m00 > m22)) { // let s = 4*x
      val s = 2.0 * Math.sqrt(1.0 + m00 - m11 - m22)
      val sinv = 1.0 / s
      x = 0.25 * s
      y = (m01 + m10) * sinv
      z = (m02 + m20) * sinv
      w = (m21 - m12) * sinv
    }
    else if (m11 > m22) { // let s = 4*y
      val s = 2.0 * Math.sqrt(1.0 + m11 - m00 - m22)
      val sinv = 1.0 / s
      x = (m01 + m10) * sinv
      y = 0.25 * s
      z = (m21 + m12) * sinv
      w = (m02 - m20) * sinv
    }
    else { // let s = 4*z
      val s = 2.0 * Math.sqrt(1.0 + m22 - m00 - m11)
      val sinv = 1.0 / s
      x = (m02 + m20) * sinv
      y = (m21 + m12) * sinv
      z = 0.25 * s
      w = (m10 - m01) * sinv
    }
    return QuaternionRotation.of(w, x, y, z)
  }

  //input matrix: output: rotation and quarternion
  def tMatrixToRigidBodyTransformation(tMatrix: List[List[Float]]) = {
    // unity order:
    // localscale, localrot, local trans, pscale, prot, ptrans

    val (localScale, globalScale) = (Array(0.03, 0.02, 0.5), Array(100.0, 100.0, 100.0))
    val localRot = Array(0.8027017, 0.3481822, 0.4802812, 0.06139394)
    val localTranslate = Array(0.037, 0.117, 0.096)

    val localScaleVector = Vector3D.of(localScale)
    val globalScaleVector = Vector3D.of(globalScale)
    val localRotQuat = QuaternionRotation.of(localRot(0), localRot(1), localRot(2), localRot(3))
    val localTranslateVector = Vector3D.of(localTranslate)

    val affineTransMatrix = AffineTransformMatrix3D.of(tMatrix.dropRight(1).reduce(_ ++ _).toArray.map(_.toDouble): _*)
    val a = new AffineTransformMatrix3DAccessor(affineTransMatrix)

    println("-------------")
    println("Calculated Matrix")
    println(s"${AffineTransformMatrix3D.identity().scale(localScaleVector).rotate(localRotQuat).translate(localTranslateVector).scale(globalScaleVector)}")
    println("-------------")


    // val newMat = AffineTransformMatrix3D.of()
    // val removedAccessor = new AffineTransformMatrix3DAccessor(removed._1)
    val qw = math.sqrt(1 + a._m00 + a._m11 + a._m22) / 2
    val qx = (a._m21 - a._m12) / (4 * qw)
    val qy = (a._m02 - a._m20) / (4 * qw)
    val qz = (a._m10 - a._m01) / (4 * qw)

    val rQuaternion = QuaternionRotation.of(qw, qx, qy, qz)

    val rQuat2 = orthogonalRotationMatrixToQuaternion(affineTransMatrix.toArray)

    println(s"Input Matrix:")
    println(s"${affineTransMatrix.toString}")

    /*
    println(s"Translation: ${rTranslation.toString}")
    println(s"ScaleFactors: ${rScaling.toString}")
    println(s"Quaternion: $qw, $qx, $qy, $qz")
    println(s"Quaternion: ${rQuaternion.toString}")
    println(s"Quaternion2: ${rQuat2.toString}")
*/

    //    val newMatrix: AffineTransformMatrix3D = AffineTransformMatrix3D.of()
    //    println(idMat.translate(0.1, 0.5, 10).rotate(QuaternionRotation()))
    //    println(s"Matrix: ")
    //    println(s"${tMatrix}")
    //    println(s"mat: ")
    //    println(s"${affineTransMatrix}")
    //    println(s"Translation: ${translation.toString()}")
    //    println(s"Translation: ${}")
    //    println(s"Translation: ${affineTransMatrix.toArray}")
    //    println(s"-----------------------------")
  }

  val sceneString3D: String = """{"boundaries":[100,100,100],"obstacles":[{"tMatrix":[[1.59336424,0.47177738,40.6898842,3.7],[1.29903829,1.49999964,-25.0000057,11.7],[-2.18487763,1.235891,14.8099127,9.6],[0.0,0.0,0.0,1.0]],"cubeSize":[0.03,0.02,0.5]}]}"""
  val scene: Scene = decode[Scene](sceneString3D).right.get

  scene.obstacles.map(i => tMatrixToRigidBodyTransformation(i.tMatrix))

}