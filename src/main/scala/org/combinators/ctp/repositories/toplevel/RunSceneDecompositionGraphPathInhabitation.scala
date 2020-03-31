package org.combinators.ctp.repositories.toplevel

import org.combinators.cls.interpreter.ReflectedRepository
import org.combinators.cls.types.Constructor
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories.celldecomposition.CellDecompRepository
import org.combinators.ctp.repositories.geometry.{GeometricRepository, GeometryUtils, PpVertexList}
import org.combinators.ctp.repositories.graphsearch.{GraphSearchPyRepository, GraphSearchRepository}
import org.combinators.ctp.repositories.scene._
import org.combinators.ctp.repositories.taxkinding.CombinatorialMotionPlanning
import org.combinators.ctp.repositories.{cmp_cd_cells, cmp_cell_graph, dimensionality_two_d_t, sd_poly_scene_segmentation, sd_polygon_scene_type, sd_seg_cells, sd_seg_centroid_cells, sd_seg_lines, _}
import scalax.collection.Graph
import scalax.collection.edge.WUnDiEdge

object RunSceneDecompositionGraphPathInhabitation extends App {
  //val ihCall  = InhabitationCall[InteropRepository, Properties](new InteropRepository{}, Constructor("p_unityConnectionProperties_type"))

  lazy val repository = new ConnectionPropertiesRepository with SceneRepository with CellDecompRepository with AkkaMqttComponents
    with GeometryUtils with GeometricRepository with CmpTopLevel with AkkaMqttTopLevel with GraphSearchRepository
    with GraphSearchPyRepository {}

  lazy val asd = new CombinatorialMotionPlanning{}
  println("vcd + graph + path")

  lazy val Gamma = ReflectedRepository(repository, substitutionSpace = asd.kinding)

  println("kinding: " + Gamma.substitutionSpace.toString)
  println("Reflected Repository built, starting inhabitation")

  val ihCall = Gamma.inhabit[Any](p_mqttCirceClient_type)
  println("...")
  val ihCall2 = Gamma.inhabit[Any](p_unitySceneConnectionValues_two_d_type :&: cmp_cd_cells)
  println("...")
  val ihCall2a = Gamma.inhabit[List[MqttCubeData] => List[PpVertexList]]( gm_CubeToPoly :&: dimensionality_two_d_t)
  println("...")
  val ihCall3 = Gamma.inhabit[Scene => PolygonScene](sd_unity_scene_type =>: sd_polygon_scene_type)
  println("...")
  val ihCall4 = Gamma.inhabit[PolygonScene => PolySceneLineSegmentation](p_mqttAkkaSource_type :&: sd_unity_scene_type)
  println("...")

    val ihCall5a = Gamma.inhabit[PolySceneLineSegmentation => PolySceneCellSegmentation](
    sd_seg_lines :&: sd_poly_scene_segmentation =>: sd_seg_cells :&: sd_poly_scene_segmentation)
  println("...")
  val ihCall5centroids = Gamma.inhabit[PolySceneCellSegmentation => PolySceneCellSegmentationCentroids](sd_poly_scene_segmentation :&: sd_seg_cells =>: sd_poly_scene_segmentation :&: sd_seg_centroid_cells)
  println("...")
  val ihCall5b = Gamma.inhabit[PolySceneCellSegmentationCentroids => Graph[List[Float], WUnDiEdge]](sd_poly_scene_segmentation :&: sd_seg_centroid_cells =>: cmp_cell_graph)
  println("...")
  val ihCall5 = Gamma.inhabit[Unit](p_unitySceneAgent_type :&: cmp_scene_graph_path :&: cmp_vertical_cell_decomposition_type)
/* val teststr ="{\n  \"boundaries\": [\n    100,\n    100,\n    0\n  ],\n  \"obstacles\": [\n    {\n      \"tMatrix\": [\n        [\n          26.97,\n          0.0,\n          0.0,\n          -33.1\n        ],\n        [\n          0.0,\n          22.36,\n          0.0,\n          25.7\n        ],\n        [\n          0.0,\n          0.0,\n          0.0,\n          0.0\n        ],\n        [\n          0.0,\n          0.0,\n          0.0,\n          1.0\n        ]\n      ],\n      \"cubeSize\": [\n        26.97,\n        22.36,\n        0.0\n      ]\n    },\n    {\n      \"tMatrix\": [\n        [\n          17.8544483,\n          5.656854,\n          0.0,\n          17.5\n        ],\n        [\n          -17.8544464,\n          5.65685463,\n          0.0,\n          -21.1\n        ],\n        [\n          0.0,\n          0.0,\n          0.0,\n          0.0\n        ],\n        [\n          0.0,\n          0.0,\n          0.0,\n          1.0\n        ]\n      ],\n      \"cubeSize\": [\n        25.25,\n        8.0,\n        0.0\n      ]\n    },\n    {\n      \"tMatrix\": [\n        [\n          22.2738647,\n          -13.43503,\n          0.0,\n          23.9\n        ],\n        [\n          22.2738628,\n          13.4350319,\n          0.0,\n          28.3\n        ],\n        [\n          0.0,\n          0.0,\n          0.0,\n          0.0\n        ],\n        [\n          0.0,\n          0.0,\n          0.0,\n          1.0\n        ]\n      ],\n      \"cubeSize\": [\n        31.5,\n        19.0000019,\n        0.0\n      ]\n    }\n  ]\n}"
  val decoded = decode[Scene](teststr)*/
  println("done")

  println(f"1: ${ihCall.isEmpty}, 2: ${ihCall2.isEmpty}, 2a: ${ihCall2a.isEmpty}, 3: ${ihCall3.isEmpty}, " +
    f"4: ${ihCall4.isEmpty}, 5a: ${ihCall5a.isEmpty}, 5centroids: ${ihCall5centroids.isEmpty}, " +
    f"5b: ${ihCall5b.isEmpty}, 5: ${ihCall5.isEmpty}")

  ihCall5.size match {
    case Some(_) =>
      println(ihCall5.terms.index(0))
      println(ihCall5.grammar)
      ihCall5.interpretedTerms.index(0)
    case None => ()
  }
}
