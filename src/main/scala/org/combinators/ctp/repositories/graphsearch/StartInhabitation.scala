package org.combinators.ctp.repositories.graphsearch

import org.combinators.cls.interpreter._
import org.combinators.cls.types.Type
import org.combinators.cls.types.syntax._

object StartInhabitation extends {

//  def checkResult(ihResult: InhabitationResult[irType]): Unit = {
//    try {
//      println("Found: " + ihResult.interpretedTerms.index(0))
//      println(ihResult.interpretedTerms.index(0).getClass)
//      println(ihResult.terms.index(0))
//
//      while (!scala.io.StdIn.readLine().equals("q")) {
//        println("Starting")
//        ihResult.interpretedTerms.index(0).unsafeRunSync()
//        println("checkResult done")
//      }
//    }
//    catch {
//      case _: IndexOutOfBoundsException => println("Index out of bounds exception")
//      case x: Exception => println("Exception: ")
//        x.printStackTrace
//    }
//  }
//
//  lazy val Gamma = new GraphSearchRepository{}
//
//  lazy val reflectedGamma = ReflectedRepository(Gamma, Gamma.semanticTaxonomy, Gamma.kinding)
//
//  def printCombinator: ((String,Type)) => Unit = {
//    case (typ, str) => {
//      print(strCombinator(typ))
//      print(makroNativeType(str.toString().substring(0, str.toString().indexOf('&')-1)).replaceAll("\\-\\>","\\$\\\\rightarrow\\$").replaceAll("\\=\\>","\\$\\\\Rightarrow\\$"))
//      println(makroSemanticType(str.toString().substring(str.toString().indexOf('&')).replaceAll("&","\\~\\\\cap\\~").replaceAll("\\-\\>","\\\\rightarrow")))
//    }}
//
//
//  def strCombinator(str: String):String = "\\combinatorName{"+str+"}"
//  def makroNativeType(str: String):String = "\\nativeType{"+str+"}"
//  def makroSemanticType(str: String):String = "\\semanticType{"+str+"}\\\\"
//
//  //reflectedGamma.combinators.foreach(printCombinator)
//  val ty = 'twoLightSensors :&: 'touchSensor :&: 'car :&: 'followsLine :&: 'robotProgram
//  println("Type: " + ty)
//  checkResult(reflectedGamma.inhabit[irType](ty))
//
//  println("done")
}
