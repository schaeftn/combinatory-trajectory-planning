package org.combinators.ctp.repositories.python_interop

import java.io.{BufferedWriter, File, FileWriter}
import com.typesafe.scalalogging.LazyLogging

import scala.io.Source
import scala.sys.process._

case class SubstitutionScheme(f: Map[String, String], substitutes: Map[String, String])
  extends PythonTemplateUtils {
  self =>
  def executeTemplating() = {
    f.map { case (template, target) =>
      println(s"Trying to read template file: $template")
      val templateSource = Source.fromFile(template)
      val content = templateSource.getLines.mkString("\n")
      assert(content.length > 0)
      templateSource.close
      val newContent = substitutes.foldLeft(content) { case (a: String, (b: String, c: String)) => a.replace(b, c) }
      println(s"Template file $template read")
      println("Replaced file contents: \n" + newContent)
      println(s"Attempting to write file $target")
      val bw = new BufferedWriter(new FileWriter(target))
      bw.write(newContent)
      bw.close()
      println("outFile written")
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

case class PlannerScheme[A, B](st: SubstitutionScheme, pf: (A, String) => B, startFile: String)

abstract class PythonWrapper[B](t: SubstitutionScheme, startFile: String) extends LazyLogging {
  def generateFiles(): Unit = {
    t.executeTemplating()
  }

  def executePythonFile(): String = {
    val foo = s"python3 $startFile"
    foo.lineStream_!.takeWhile(_ => true).toList.mkString("\r\n")
  }
}

abstract case class SimplePythonWrapper[B](t: SubstitutionScheme, startFile: String)
  extends PythonWrapper[B](t, startFile) {
  def parseResult: String => B

  def computeResult: B = {
    generateFiles()
    val resultString: String = executePythonFile()
    println(s"ResultString (computeResult): ")
    println(s"$resultString")
    parseResult(resultString)
  }
}

abstract case class PythonWrapperModifier[A, B](t: SubstitutionScheme, startFile: String)
  extends PythonWrapper[B](t, startFile) {
  def computeResultAndModifyInput: A => B = { input: A =>
    println(s"Generating files... ")
    generateFiles()
    val resultString: String = executePythonFile()
    println(s"ResultString (computeResult): ")
    println(s"$resultString")
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
}
