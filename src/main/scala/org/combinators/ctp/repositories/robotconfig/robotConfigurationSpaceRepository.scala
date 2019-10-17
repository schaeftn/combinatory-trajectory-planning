package org.combinators.ctp.repositories.robotconfig

import org.combinators.cls.interpreter._
import org.combinators.cls.types.Type
import org.combinators.cls.types.syntax._


trait RobotMotionsRepository {
  val pointRobot2D: Type = 'rm_Translation2D
  val pointRobot3D: Type = 'rm_Translation3D
  val robotCellOne: Type = 'rm_Rotation
  val robotCellTwo: Type = 'robotCellTwo
  val robotCellThree: Type = 'robotCellTwo

  @combinator object PointRobot {
    def apply: String = ???
    val semanticType = pointRobot2D
  }

  @combinator object RobotCellOne {
    def apply: String = ???
    val semanticType = robotCellOne
  }

  @combinator object RobotCellTwo {
    def apply: String = ???
    val semanticType = robotCellTwo
  }

  @combinator object RobotCellThree {
    def apply: String = ???
    val semanticType = robotCellThree
  }
}

trait CellTypes{
  val ZeroCell: Type = 'zeroCell
  val oneCell: Type = 'oneCell
  val twoCell: Type = 'twoCell
  val threeCell: Type = 'threeCell

  val pointRobot: Type = 'pointRobot
  val robotCellOne: Type = 'robotCellOne
  val robotCellTwo: Type = 'robotCellTwo
  val nonRigidRobot: Type = 'nonRigidRobot
}

trait RobotFormRepository extends CellTypes {
  //Composed of n Elements
  @combinator object NonRigidRobot {
    def apply = ???
    val semanticType= ???
  }
  @combinator object PointRobot {
    def apply: String = ???
    val semanticType = pointRobot :&: oneCell
  }

  @combinator object RobotCellOne {
    def apply: String = ???
    val semanticType = robotCellOne
  }

  @combinator object RobotCellTwo {
    def apply: String = ???
    val semanticType = robotCellTwo
  }
}
