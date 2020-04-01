package org.combinators.ctp.repositories.python_interop
import java.util.Properties

import scala.sys.process._

object run_python_tetrahedralize extends App {

  val connectionSettings = new Properties()
  connectionSettings.load(getClass.getClassLoader.getResourceAsStream("pythoninterop.properties"))
  val tetStartLocation = connectionSettings.getProperty("org.combinators.ctp.python.cdStartLocationTet")

  val foo = s"python3 $tetStartLocation"
  val result = foo.lineStream_!
  val resultString = result.takeWhile(_ => true).foldLeft("")((b, s) => b.concat(s + "\n"))
  println(resultString)
}
