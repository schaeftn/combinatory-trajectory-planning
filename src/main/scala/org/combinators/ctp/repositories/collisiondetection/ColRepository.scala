package org.combinators.ctp.repositories.collisiondetection

import org.combinators.cls.interpreter._
import org.combinators.cls.types.{Constructor, Type}
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories._
import org.combinators.ctp._
import scalaz.Tree

trait ColRepository {

//  bounding volumes, compare bounding volumes
  //Wenn keine konvexen Polygone verwendet werden, kann space partitioning genutzt werden

  //TODO prüfen
val geoPointDiffUnity = Constructor("geoPointDiffUnity")

  @combinator object TwoSphereCollisionPrimitive {
    def apply(f: (vertexType, vertexType) => Double): (sphereData, sphereData) => Boolean = {
      case ((a, b), (c, d)) => f(a, c) < (b + d)
    }
    val semanticType = geoPointDiffUnity =>: twoSphereCollisionFunction
  }

  @combinator object SphereAABBCollisionPrimitive {
    def apply(f: (vertexType, vertexType) => Double): (sphereData, boxData) => Boolean = {
      case ((a, b), (c, d)) => ???
    }
    val semanticType = geoPointDiffUnity =>: twoSphereCollisionFunction
  }

  @combinator object AABBAABBCollisionPrimitive{
    def apply(f: (vertexType, vertexType) => Double): (sphereData, boxData) => Boolean = {
      case ((a, b), (c, d)) => ???
    }
    val semanticType = geoPointDiffUnity =>: twoSphereCollisionFunction
  }

  @combinator object TwoSphereDistance {
    def apply(f: (vertexType, vertexType) => Double): (sphereData, sphereData) => Double = {
      case ((a, b), (c, d)) => f(a, c) - (b + d)
    }
    val semanticType = geoPointDiffUnity =>: twoSphereCollisionFunction
  }

  @combinator object AABBAABBDistance {
    def apply(f: (vertexType, vertexType) => Double): (sphereData, sphereData) => Double = {
      case ((a, b), (c, d)) => f(a, c) - (b + d)
    }
    val semanticType = geoPointDiffUnity =>: twoSphereCollisionFunction
  }


/*  @combinator object HCollisionDetector {
def apply(f:(Any, Any)) : (Tree, vertexType) => Boolean = ???
    val semanticType = v_colFunctions =>: 'h(v_colFunctions)
  }*/
}
