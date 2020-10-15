package org.combinators.ctp.repositories.pathcoverage

import org.combinators.ctp.repositories.scene.SceneUtils

trait ZigZag extends SceneUtils{
  // mit StepBased? Stepsize (flexibel, fest) Auf/Abrunden, letzter Schritt
  /*
  // Parameter: Gleichlauf, Gegenlauf,
   */
  val width:Float=1.0f
  val depth:Float=1.0f

  def getZigZag(): Unit = ???
}