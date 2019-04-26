package org.combinators.ctp.repositories.collisiondetection

import org.combinators.cls.interpreter._
import org.combinators.cls.types.Type
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories.geometricrepresentation._
import org.combinators.ctp._

trait ColRepository {

//  bounding volumes, compare bounding volumes
  //Wenn keine konvexen Polygone verwendet werden, kann space partitioning genutzt werden

  @combinator object TwoSphereCombinator {
    def apply(f: (vertexType, vertexType) => Double): (sphereData, sphereData) => Boolean = {
      case ((a, b), (c, d)) => f(a, c) < (b + d)
    }
    val semanticType = geoPointDiffUnity =>: twoSphereCollisionFunction
  }

  @combinator object SphereBoxCombinator {
    def apply(f: (vertexType, vertexType) => Double): (sphereData, boxData) => Boolean = {
      case ((a, b), (c, d)) => true
    }
    val semanticType = geoPointDiffUnity =>: twoSphereCollisionFunction
  }

  @combinator object TwoBoxCombinator

}
