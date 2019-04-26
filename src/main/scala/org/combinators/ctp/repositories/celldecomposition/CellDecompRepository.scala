package org.combinators.ctp.repositories.celldecomposition

import org.combinators.cls.interpreter._
import org.combinators.cls.types.Type
import org.combinators.cls.types.syntax._

trait CellProperties{
  val cp_convexCells: Type = 'convexCells
  val cp_cellsWithHoles: Type = 'cellsWHoles
  val cp_cellsWithoutHoles: Type = 'cellsWithoutHoles
  val simplicialComplex: Type = 'simplicialComplex
}

trait CellDecompRepository {
  @combinator object VerticalDecomposition2D {
    def apply: Unit = ???
    val semanticType = ???
  }


  @combinator object VerticalDecomposition3D {
    def apply: Unit = ???
    val semanticType = ???
  }

  @combinator object SweepLineCombinatorVd {
    def apply: Unit = ???
    val semanticType = ???
  }

  @combinator object SweepPlaneVd {
    def apply: Unit = ???
    val semanticType = ???
  }

    @combinator object CylindricalDecomposition2D {
    def apply: Unit = ???
    val semanticType = ???
  }

  @combinator object SweepLineCombinatorCyd {
    def apply: Unit = ???
    val semanticType = ???
  }

  @combinator object SweepPlaneCyd {
    def apply: Unit = ???
    val semanticType = ???
  }

}