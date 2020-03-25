package org.combinators.ctp.repositories.samplingbased

import org.combinators.cls.interpreter.combinator
import org.combinators.cls.types.Constructor
import org.combinators.ctp.repositories.scene.SceneUtils
import org.combinators.ctp.repositories.python_interop.{PythonTemplateUtils, PythonWrapper, TemplatingScheme}
import org.combinators.cls.types.syntax._


trait SamplingBasedMpRepository extends SceneUtils with PythonTemplateUtils {
  trait PythonRunner[A, B] {
    def apply(p: A => PythonWrapper[A, B]): A => B = {input: A => p(input).computeResult}
  }

  @combinator object RunPythonWrapper extends PythonRunner[String, String] {
    val semanticType = Constructor("omplPlannerPywrapper") =>: Constructor("fooAnyPlanner")
  }

  @combinator object OmplPlanner {
    def apply(a: String => TemplatingScheme): String => PythonWrapper[String, String] = { (input: String) =>
      val parseFct = { a: String => a }
      PythonWrapper(a(input),
        samplingStartFile,
        parseFct,
        Seq("p1,p2"): _*)
    }

    val semanticType = Constructor("emptyTemplate") =>: Constructor("omplPlannerPywrapper")
  }

  trait EmptyTemplateScheme[A]{
    def apply: (A => TemplatingScheme) = (input:A) => TemplatingScheme(
      Map.empty[String, String], //no files
      Map.empty[String, String], //no substitutions
    )
  }
  @combinator object EmptyTemplateScheme extends EmptyTemplateScheme[String]{
    val semanticType = Constructor("emptyTemplate")
  }




//
//
//  @combinator object SceneToPath {
//    def apply(): (SceneSRT, MpTaskStartGoal) => List[List[Float]] = { case (s, t) =>
//      def runCdFile(fclSceneString: String, startStopStateString: String):List[List[Float]] = {
//        //TODO SubstTemplate instead
//        substituteStringsInFile(fclSamplingTemplateFile, fclSamplingGenFile,
//          Map("{substitute}" -> fclSceneString))
//        substituteStringsInFile(samplingStartFileTemplate,  samplingStartFile,
//          Map("{substitute}" -> startStopStateString))
//        val foo = s"python3 $samplingStartFile"
//        val resultString = foo.lineStream_!.takeWhile(_ => true)
//        println(s"""Resultstring: ${resultString.toList.mkString("\r\n")}""")
//        println("decoded")
//        resultStringToVertexList(resultString.filter(_.startsWith("RealVectorState")))
//      }
//
//      val pyString: String = sceneSRTtoFclPythonString(s)
//      val startGoalStr: String =   taskGoalStartToPythonOMPLString(t)
//      println(s"Python Scene String: ${pyString}")
//      runCdFile(pyString, startGoalStr)
//    }
//
//    val semanticType = Constructor("sampleCombinatorTop")
//  }
//
//
//
//  @combinator object SceneToPath2D {
//    def apply(): (SceneSRT, MpTaskStartGoal) => List[List[Float]] = { case (s, t) =>
//      val pythonSettings = new Properties()
//      pythonSettings.load(getClass.getClassLoader.getResourceAsStream("pythoninterop.properties"))
//
//      //TODO Anpassen...
//
//
//      def runCdFile(cgalFileLocation: String, startStopStateString: String):List[List[Float]] = {
//        substituteStringsInFile(fclSamplingTemplateFile, fclSamplingGenFile,
//          Map("{substituteScene}" -> cgalFileLocation))
//        substituteStringsInFile(samplingStartFileTemplate, samplingStartFile,
//          Map("{substituteTask}" -> startStopStateString, "{substitutePath}"-> startStopStateString))
//        val foo = s"python3 $samplingStartFile"
//        val resultString = foo.lineStream_!.takeWhile(_ => true)
//        println(s"""Resultstring: ${resultString.toList.mkString("\r\n")}""")
//        println("decoded")
//        resultStringToVertexList(resultString.filter(_.startsWith("RealVectorState")))
//      }
//
//      val pyString: String = sceneSRTtoFclPythonString(s)
//      val startGoalStr: String =   taskGoalStartToPythonOMPLString(t)
//      println(s"Python Scene String: ${pyString}")
//      runCdFile(pyString, startGoalStr)
//    }
//
//    val semanticType = Constructor("2dTest")
//  }
//
//  def resultStringToVertexList(lines: Stream[String]): List[List[Float]] = {
//    lines.map(_.replace("RealVectorState ", "").replace("[", "")
//      .replace("]", "").split(" "))
//      .map(line => line.map(element => element.toFloat).toList).toList
//  }
}