package org.combinators.ctp.repositories.collisiondetection

import org.combinators.cls.interpreter._
import org.combinators.cls.types.Type
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories.geometricrepresentation._
import org.combinators.ctp._
import org.combinators.ctp.repositories.protocol.{BoundingBoxData, BoundingSphereData}
import scalaz.Tree

class ProximityQueries {
  @combinator object BoxSphereBvhOverlap {
    def apply(f: (vertexType, vertexType) => Double): (sphereData, sphereData) => Boolean = {
      case ((a, b), (c, d)) => f(a, c) < (b + d)
    }
    val semanticType = 'sTree2 :&: cd_report_boolean
  }

  @combinator object TwoSphereBvhOverlap {
    def apply(f: (vertexType, vertexType) => Double): (sphereData, sphereData) => Boolean = {
      case ((a, b), (c, d)) => f(a, c) < (b + d)
    }
    val semanticType = 'sTree2 :&: cd_report_boolean
  }

  @combinator object AABBB_S_BvhOverlap {
    def apply(f: (vertexType, vertexType) => Double): (sphereData, sphereData) => Boolean = {
      case ((a, b), (c, d)) => f(a, c) < (b + d)
    }
    val semanticType = 'sTree2 :&: cd_report_boolean
  }

/*  @combinator object BvhOverlap {
    def apply(f: (sphereData, boxData) => Boolean): (Tree[BoundingSphereData], Tree[BoundingBoxData]) => Boolean = {
      case (a, b) => {
        f(a.loc.root, b.loc.root)
      } //Check collisions NxM?
    }
    val semanticType = sphereBoxColFun =>: 'bvhcol('box,'sphere) :&: cd_report_boolean :&: 'threed :&: 'static
  }*/
}
