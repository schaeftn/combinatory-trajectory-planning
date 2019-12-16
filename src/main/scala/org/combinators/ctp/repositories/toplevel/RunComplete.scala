package org.combinators.ctp.repositories.toplevel

import org.combinators.cls.interpreter.ReflectedRepository
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories.celldecomposition.CellDecompRepository
import org.combinators.ctp.repositories.geometry.{GeometricRepository, GeometryUtils, PpVertexList}
import org.combinators.ctp.repositories.scene._
import org.combinators.ctp.repositories.taxkinding.CombinatorialMotionPlanning
import org.combinators.ctp.repositories.{cmp_cd_cells, _}

object RunComplete extends App {
  //val ihCall  = InhabitationCall[InteropRepository, Properties](new InteropRepository{}, Constructor("p_unityConnectionProperties_type"))

  lazy val repository = new ListenerRepository with SceneRepository with GeometricRepository with AkkaMqttComponents
    with CombinatorialTopLevel with AkkaMqttTopLevel with CellDecompRepository with GeometryUtils
  lazy val asd = new CombinatorialMotionPlanning{}

  lazy val Gamma = ReflectedRepository(repository, substitutionSpace = asd.kinding)

  println("kinding: " + Gamma.substitutionSpace.toString)
  println("Reflected Repository built, starting inhabitation")


  val ihCall5 = Gamma.inhabit[Any](p_unitySceneAgent_type)
/* val teststr ="{\n  \"boundaries\": [\n    100,\n    100,\n    0\n  ],\n  \"obstacles\": [\n    {\n      \"tMatrix\": [\n        [\n          26.97,\n          0.0,\n          0.0,\n          -33.1\n        ],\n        [\n          0.0,\n          22.36,\n          0.0,\n          25.7\n        ],\n        [\n          0.0,\n          0.0,\n          0.0,\n          0.0\n        ],\n        [\n          0.0,\n          0.0,\n          0.0,\n          1.0\n        ]\n      ],\n      \"cubeSize\": [\n        26.97,\n        22.36,\n        0.0\n      ]\n    },\n    {\n      \"tMatrix\": [\n        [\n          17.8544483,\n          5.656854,\n          0.0,\n          17.5\n        ],\n        [\n          -17.8544464,\n          5.65685463,\n          0.0,\n          -21.1\n        ],\n        [\n          0.0,\n          0.0,\n          0.0,\n          0.0\n        ],\n        [\n          0.0,\n          0.0,\n          0.0,\n          1.0\n        ]\n      ],\n      \"cubeSize\": [\n        25.25,\n        8.0,\n        0.0\n      ]\n    },\n    {\n      \"tMatrix\": [\n        [\n          22.2738647,\n          -13.43503,\n          0.0,\n          23.9\n        ],\n        [\n          22.2738628,\n          13.4350319,\n          0.0,\n          28.3\n        ],\n        [\n          0.0,\n          0.0,\n          0.0,\n          0.0\n        ],\n        [\n          0.0,\n          0.0,\n          0.0,\n          1.0\n        ]\n      ],\n      \"cubeSize\": [\n        31.5,\n        19.0000019,\n        0.0\n      ]\n    }\n  ]\n}"
  val decoded = decode[Scene](teststr)*/
  println("donefinal")

  println(s"5: ${ihCall5.isEmpty}")

  ihCall5.size match {
    case Some(_) => {
      println(s"size: ${ihCall5.size.get}")
      (0 until ihCall5.size.get.toInt).foreach(i => println(s"$i: ${ihCall5.terms.index(5).toString}"))
      println(ihCall5.grammar)

//      ihCall5.interpretedTerms.index(0)
    }
    case None => ()
  }
}
