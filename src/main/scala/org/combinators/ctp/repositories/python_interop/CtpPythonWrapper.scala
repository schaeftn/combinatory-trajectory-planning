package org.combinators.ctp.repositories.python_interop

import java.io.{BufferedWriter, File, FileWriter}
import com.typesafe.scalalogging.LazyLogging

import scala.io.Source
import scala.sys.process._

case class SubstitutionScheme(f: Map[String, String], substitutes: Map[String, String])
  extends PythonTemplateUtils with LazyLogging {
  self =>
  def executeTemplating():Unit = {
    f.foreach { case (template, target) =>
      logger.debug(s"Trying to read template file: $template")
      val templateSource = Source.fromFile(template)
      val content = templateSource.getLines.mkString("\n")
      assert(content.length > 0)
      templateSource.close
      val newContent = substitutes.foldLeft(content) { case (a: String, (b: String, c: String)) => a.replace(b, c) }
      logger.debug(s"Template file $template read")
      logger.debug("Replaced file contents: \n" + newContent)
      logger.debug(s"Attempting to write file $target")
      val bw = new BufferedWriter(new FileWriter(target))
      bw.write(newContent)
      bw.close()
      logger.debug("outFile written")
    }
  }

  def merge(tScheme: SubstitutionScheme): SubstitutionScheme =
    SubstitutionScheme(self.f ++ tScheme.f, self.substitutes ++ tScheme.substitutes) //overwrites if key exists
}

object SubstitutionScheme {
  def apply(fileList: Map[String, String], //File in, File out
            substitutes: Map[String, String]) //Subst in, Subst out
  = new SubstitutionScheme(fileList, substitutes) {
  }
}

case class ModifierPlannerScheme[A, B](st: SubstitutionScheme, pf: (A, String) => B, startFile: String)
case class PlannerScheme[B](st: SubstitutionScheme, pf: (String) => B, startFile: String)

abstract class PythonWrapper[B](t: SubstitutionScheme, startFile: String) extends LazyLogging {
  def generateFiles(): Unit = {
    t.executeTemplating()
  }

  def executePythonFile(args: String): String = {
    val foo = s"python3 $startFile $args"
    foo.lazyLines_!.takeWhile(_ => true).toList.mkString("\r\n")
  }
}

abstract case class SimplePythonWrapper[B](t: SubstitutionScheme, startFile: String, args: String ="")
  extends PythonWrapper[B](t, startFile) {
  def parseResult: String => B

  def computeResult: B = {
    generateFiles()
    logger.info(s"Python files generated. Running Python.")
    val resultString: String = executePythonFile(args)
    logger.debug(s"ResultString (computeResult): ")
    logger.debug(s"$resultString")
    parseResult(resultString)
  }
}

abstract case class PythonWrapperModifier[A, B](t: SubstitutionScheme, startFile: String)
  extends PythonWrapper[B](t, startFile) {
  def computeResult: A => B = { input: A =>
    logger.debug(s"Generating files... ")
    generateFiles()
    val resultString: String = executePythonFile("")
    logger.debug(s"ResultString (computeResult): ")
    logger.debug(s"$resultString")
    parseResultAndModifyInput(input, resultString)
  }

  def parseResultAndModifyInput: (A, String) => B
}

object PythonWrapper {
  def apply[A, B](t: SubstitutionScheme,
                  startFile: String,
                  parseFct: (A, String) => B): PythonWrapperModifier[A, B] =
    new PythonWrapperModifier[A, B](t, startFile) {
      override def parseResultAndModifyInput: (A, String) => B = parseFct
    }

  def apply[A, B](t: SubstitutionScheme,
                  startFile: String,
                  parseFct: String => B): SimplePythonWrapper[B] =
    new SimplePythonWrapper[B](t, startFile) {
      override def parseResult: String => B = parseFct
    }

  def apply[A, B](t: SubstitutionScheme,
                  startFile: String,
                  p_args: String,
                  parseFct: String => B): SimplePythonWrapper[B] =
    new SimplePythonWrapper[B](t, startFile, args = p_args) {
      override def parseResult: String => B = parseFct
    }
}
