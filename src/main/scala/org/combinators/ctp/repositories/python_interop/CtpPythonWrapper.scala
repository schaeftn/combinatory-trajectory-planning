package org.combinators.ctp.repositories.python_interop

import java.io.{BufferedWriter, File, FileWriter}
import java.util.Properties

import com.typesafe.scalalogging.LazyLogging

import scala.io.Source
import scala.sys.process._


trait PythonDataTransform[A] {
  def getString[A](input: A)
}

case class TemplatingScheme(f: Map[String, String], substitutes: Map[String,String])
  extends PythonTemplateUtils {
  self =>
  def executeTemplating() = {
    f.map{case (template, target) =>
      val templateFile = new File(template)
      assert(templateFile.exists)
      assert(templateFile.getAbsolutePath.length > 0)
      val templateSource = Source.fromFile(templateFile)
      val content = templateSource.getLines.mkString("\n")
      assert(content.length > 0)
      templateSource.close
      val newContent = substitutes.foldLeft(content) { case (a: String, (b: String, c: String)) => (a.replace(b, c)) }
      println(s"Template file ${template} read")
      println("Replaced file contents: \n" + newContent)
      val outFile = new File(target)
      assert(outFile.exists)
      val bw = new BufferedWriter(new FileWriter(outFile))
      bw.write(newContent)
      bw.close()
      println("outFile written")
    }

  } // TODO Impl

  def merge(tScheme: TemplatingScheme): TemplatingScheme =
    TemplatingScheme(self.f ++ tScheme.f, self.substitutes ++ tScheme.substitutes)
}

object TemplatingScheme {
  def apply(fileList: Map[String, String], //File in, File out
               substitutes: Map[String, String])//Subst in, Subst out
  = new TemplatingScheme(fileList, substitutes) {
  }
}

abstract case class PythonWrapper[A, B](t: TemplatingScheme, startFile: String, args: String*) extends LazyLogging {
  def generateFiles(): Unit = {
    t.executeTemplating()
  }
  def parseResult: String => B

  def computeResult: B = {
    generateFiles()
    val resultString: String = executePythonFile()
    println(s"ResultString (computeResult): ")
    println(s"${resultString}")
    parseResult(resultString)
  }

  def executePythonFile(): String = {
    val foo = s"python3 $startFile $args"
    foo.lineStream_!.takeWhile(_ => true).toList.mkString("\r\n")
  }
}

object PythonWrapper {
  def apply[A, B](t: TemplatingScheme,
                  startFile: String,
                  parseFct: String => B,
                  args: String*): PythonWrapper[A, B] =
    new PythonWrapper[A, B](t, startFile, args: _*) {
      override def parseResult: String => B = parseFct
    }
}
