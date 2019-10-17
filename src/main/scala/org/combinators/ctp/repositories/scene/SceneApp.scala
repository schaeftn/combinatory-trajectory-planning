package org.combinators.ctp.repositories.scene

import io.circe.generic.JsonCodec
import io.circe.syntax._
import org.combinators.ctp.repositories._


object SceneApp extends App {
  val vArray:Array[Array[Float]] = Array(
    Array(-1.0f, -1.0f, -1.0f),
    Array(1.0f, -1.0f, -1.0f),
    Array(1.0f, 1.0f, -1.0f),
    Array(-1.0f, 1.0f, -1.0f),
    Array(-1.0f, -1.0f, 1.0f),
    Array(1.0f, -1.0f, 1.0f),
    Array(1.0f, 1.0f, 1.0f),
    Array(-1.0f, 1.0f, 1.0f))

  val triangles = Array(Array(0, 2, 1),
    Array(0, 3, 2),
    Array(5, 0, 1),
    Array(5, 4, 0),
    Array(2, 6, 1),
    Array(3, 6, 2),
    Array(5, 1, 6),
    Array(5, 6, 4),
    Array(0, 7, 3),
    Array(0, 4, 7),
    Array(3, 7, 6),
    Array(6, 7, 4))

  val voxels = Array(Array(0, 3, 7, 6),
    Array(0, 3, 6, 2),
    Array(0, 2, 6, 1),
    Array(5, 0, 6, 1),
    Array(5, 0, 4, 6),
    Array(6, 0, 4, 7))

  val foo = SceneMesh(
    vertices = vArray,
    faces = triangles,
    voxels = voxels)

print(foo.asJson)
}