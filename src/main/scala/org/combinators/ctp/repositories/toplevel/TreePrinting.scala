package org.combinators.ctp.repositories.toplevel

import org.combinators.cls.inhabitation.Tree

trait TreePrinting {
  def getStringForTree(t: Tree, n: Int = 0): String = {
    (("\t" * n + t.name) +:
      t.arguments.map { (a: Tree) =>
        getStringForTree(a, n + 1)
      }).mkString("\r\n")
  }
}
