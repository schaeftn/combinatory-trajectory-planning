package org.combinators.ctp.repositories.geometry

import io.circe.generic.JsonCodec
import org.combinators.ctp.repositories.vertexType

case class PpLine(p: PpPoint_3D)

case class PpHalfspace(l: PpLine, p: PpPoint_3D)

case class PpCube(width: Int, height: Int, depth: Int)

case class PpPoint_3D(x: Float, y: Float, z: Float)

case class PpPoint_2D(x: Float, y: Float)

@JsonCodec
case class PpVertexList(vertices: List[List[Float]])

@JsonCodec
case class PpPolyhedronMesh(vertices: List[List[Float]], triangles: List[Int], faces: List[Int])

@JsonCodec
case class PpSurfaceMesh(vertices: List[List[Float]], triangles: List[Int])

@JsonCodec
case class PpPolygon(vertices: List[List[Float]], triangles: List[Int])

@JsonCodec
case class PpPolygonMesh(vertices: List[List[Float]], triangles: List[Int])

case class PpPointCloud3D(points: List[PpPoint_3D])

case class PpPointCloud2D(points: List[PpPoint_2D])

case class PpUrdfInfo()

case class PpSphere(radius: Float)

case class PpCircle(radius: Float)

@JsonCodec
case class PpAaBb2D(xBounds: (Float, Float), yBounds: (Float, Float))

@JsonCodec
case class PpAaBb3D(xBounds: (Float, Float), yBounds: (Float, Float), zBounds: (Float, Float))

