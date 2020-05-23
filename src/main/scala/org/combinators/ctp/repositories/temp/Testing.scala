package org.combinators.ctp.repositories.temp

import io.circe.generic.auto._
import io.circe.parser.decode
import org.combinators.ctp.repositories.cmp.CmpUtils
import org.combinators.ctp.repositories.temp.GridCMP._ //{bitmapFromPolyScene, printBitmap, sceneTransform}
import org.combinators.ctp.repositories.toplevel.{PolySceneLineSegmentation, PolygonScene, Scene}

object Testing extends App with CmpUtils {

  val sceneString: String = """{"boundaries":[100,100,0],"obstacles":[{"tMatrix":[[53.85289,4.82381058,0.0,-49.88],[-12.1389685,4.810618,0.0,-8.34],[0.0,0.0,0.0,0.0],[0.0,0.0,0.0,1.0]],"cubeSize":[64.34709,9.13852,0.0]},{"tMatrix":[[10.2148867,0.0,0.0,26.8900013],[0.0,48.25598,0.0,-10.5599995],[0.0,0.0,0.0,0.0],[0.0,0.0,0.0,1.0]],"cubeSize":[10.2148867,48.25598,0.0]},{"tMatrix":[[10.4838343,3.62617254,0.0,-0.38],[-1.06201494,35.7962837,0.0,32.94],[0.0,0.0,0.0,0.0],[0.0,0.0,0.0,1.0]],"cubeSize":[10.537488,35.97948,0.0]},{"tMatrix":[[42.2332878,-0.3470646,0.0,-35.4600029],[-4.27824259,-3.426098,0.0,8.7300005],[0.0,0.0,0.0,0.0],[0.0,0.0,0.0,1.0]],"cubeSize":[42.44943,-3.44363236,0.0]},{"tMatrix":[[16.3090668,0.0,0.0,-36.3],[0.0,-10.5733194,0.0,39.6],[0.0,0.0,0.0,0.0],[0.0,0.0,0.0,1.0]],"cubeSize":[16.3090668,-10.5733194,0.0]},{"tMatrix":[[16.3090668,0.0,0.0,-25.2],[0.0,-10.5733194,0.0,20.6],[0.0,0.0,0.0,0.0],[0.0,0.0,0.0,1.0]],"cubeSize":[16.3090668,-10.5733194,0.0]},{"tMatrix":[[16.3090668,0.0,0.0,8.5],[0.0,-10.5733194,0.0,3.6],[0.0,0.0,0.0,0.0],[0.0,0.0,0.0,1.0]],"cubeSize":[16.3090668,-10.5733194,0.0]},{"tMatrix":[[6.57370663,0.0,0.0,2.80000019],[0.0,46.4312019,0.0,-29.1999989],[0.0,0.0,0.0,0.0],[0.0,0.0,0.0,1.0]],"cubeSize":[6.57370663,46.4312019,0.0]}]}"""
  val decScene: Scene = decode[Scene](sceneString).right.get
  val polyScene: PolygonScene = sceneTransform(decScene)

  //Nutzung der Methoden um Polygon Scene als Bitmap auszugeben

  printBitmap(bitmapFromPolyScene(polyScene, 50, 50))

  val resultString = """{
                       |"vertices" :
                       |[[-79.218350,  -4.675825],
                       | [-25.365461, -16.814793],
                       | [-20.541650, -12.004175],
                       | [-74.394540,   0.134793],
                       | [ 21.782558, -34.687990],
                       | [ 31.997444, -34.687990],
                       | [ 31.997444,  13.567991],
                       | [ 21.782558,  13.567991],
                       | [ -7.435003,  15.572865],
                       | [  3.048831,  14.510849],
                       | [  6.675003,  50.307133],
                       | [ -3.808831,  51.369150],
                       | [-56.403114,  12.582171],
                       | [-14.169826,   8.303928],
                       | [-14.516891,   4.877830],
                       | [-56.750180,   9.156073],
                       | [-44.454533,  44.886658],
                       | [-28.145466,  44.886658],
                       | [-28.145466,  34.313340],
                       | [-44.454533,  34.313340],
                       | [-33.354534,  25.886660],
                       | [-17.045467,  25.886660],
                       | [-17.045467,  15.313341],
                       | [-33.354534,  15.313341],
                       | [  0.345467,   8.886660],
                       | [ 16.654533,   8.886660],
                       | [ 16.654533,  -1.686660],
                       | [  0.345467,  -1.686660],
                       | [ -0.486853, -52.415600],
                       | [  6.086854, -52.415600],
                       | [  6.086854,  -5.984398],
                       | [ -0.486853,  -5.984398],
                       | [ -0.486853,  14.869015],
                       | [ -0.486853, -50.000000],
                       | [  6.086854,  -1.686660],
                       | [  6.086854, -50.000000],
                       | [  6.086854,  -1.686660],
                       | [  0.345467,  14.784701],
                       | [  0.345467,  -5.984398],
                       | [ 16.654533,  50.000000],
                       | [ 16.654533, -50.000000],
                       | [  0.345467,  14.784701],
                       | [  0.345467,  -5.984398],
                       | [-33.354534,  34.313340],
                       | [-33.354534,  10.247344],
                       | [-17.045467,  50.000000],
                       | [-17.045467,   8.595231],
                       | [-44.454533,  50.000000],
                       | [-44.454533,  11.371776],
                       | [-28.145466,  50.000000],
                       | [-28.145466,  25.886660],
                       | [-56.403114,  50.000000],
                       | [-14.169826,  50.000000],
                       | [-14.169826, -50.000000],
                       | [-14.516891, -50.000000],
                       | [-56.750180,  50.000000],
                       | [-56.750180,  -3.842418],
                       | [ -7.435003,  50.000000],
                       | [ -7.435003, -50.000000],
                       | [  3.048831,   8.886660],
                       | [  6.675003,  50.000000],
                       | [  6.675003,   8.886660],
                       | [ -3.808831,  50.000000],
                       | [ 21.782558,  50.000000],
                       | [ 21.782558, -50.000000],
                       | [ 31.997444,  50.000000],
                       | [ 31.997444, -50.000000],
                       | [-79.218350,  50.000000],
                       | [-79.218350, -50.000000],
                       | [-25.365461, -50.000000],
                       | [-20.541650,   5.488140],
                       | [-20.541650, -50.000000],
                       | [-74.394540,  50.000000]],
                       |"obstacles" :
                       |[[28, 29, 30, 31, 38, 42], [24, 25, 26, 27, 34, 36, 59, 61], [20, 21, 22, 23, 50], [16, 17, 18, 19, 43], [12, 13, 14, 15, 44, 46, 48, 70], [8, 9, 10, 11, 32, 37, 41], [4, 5, 6, 7], [0, 1, 2, 3, 56]],
                       |"boundaries" :
                       |[100.000000, 100.000000,   0.000000],
                       |"topVertices" :
                       |[39, 39, 45, 45, 47, 49, 49, 47, 51, 52, 55, 57, 60, 62, 63, 65, 65, 63,
                       | 67, 72],
                       |"bottomVertices" :
                       |[33, 35, 35, 33, 40, 40, 53, 54, 58, 64, 66, 66, 64, 68, 69, 71],
                       |"lines" :
                       |[[28, 32],
                       | [28, 33],
                       | [29, 34],
                       | [29, 35],
                       | [30, 36],
                       | [30, 35],
                       | [31, 32],
                       | [31, 33],
                       | [24, 37],
                       | [24, 38],
                       | [25, 39],
                       | [25, 40],
                       | [26, 39],
                       | [26, 40],
                       | [27, 41],
                       | [27, 42],
                       | [20, 43],
                       | [20, 44],
                       | [21, 45],
                       | [21, 46],
                       | [22, 45],
                       | [22, 46],
                       | [23, 43],
                       | [23, 44],
                       | [16, 47],
                       | [16, 48],
                       | [17, 49],
                       | [17, 50],
                       | [18, 49],
                       | [18, 50],
                       | [19, 47],
                       | [19, 48],
                       | [12, 51],
                       | [13, 52],
                       | [13, 53],
                       | [14, 54],
                       | [15, 55],
                       | [15, 56],
                       | [ 8, 57],
                       | [ 8, 58],
                       | [ 9, 59],
                       | [10, 60],
                       | [10, 61],
                       | [11, 62],
                       | [ 4, 63],
                       | [ 4, 64],
                       | [ 5, 65],
                       | [ 5, 66],
                       | [ 6, 65],
                       | [ 6, 66],
                       | [ 7, 63],
                       | [ 7, 64],
                       | [ 0, 67],
                       | [ 0, 68],
                       | [ 1, 69],
                       | [ 2, 70],
                       | [ 2, 71],
                       | [ 3, 72]]
                       |}
                       |""".stripMargin

  println("Python: ")
  val polySceneLinePython: PolySceneLineSegmentation = decode[PolySceneLineSegmentation](resultString).right.get

  polySceneLinePython.lines.foreach(i => println(s"line: ${i.map(polySceneLinePython.vertices)}"))

  printBitmap(bitmapFromPolySceneLineSegmentation(polySceneLinePython,50,50))

  //val polySceneCellPython = vcdLtC2.apply(polySceneLinePython)

  //polySceneCellPython.freeCells.foreach(i => println(s"freeCell: ${i.map(polySceneCellPython.vertices)}"))

  //val newPolySceneLinePython = VerticalCellToLineSegmentation(polySceneCellPython)

  //newPolySceneLinePython.lines.foreach(i => println(s"line: ${i.map(newPolySceneLinePython.vertices)}"))

  //printBitmap(bitmapFromPolySceneLineSegmentation(VerticalCellToLineSegmentation(polySceneCellPython),50,50))

  println("Scala: ")
  val polySceneLineScala: PolySceneLineSegmentation = Vcd2D(polyScene)

  polySceneLineScala.lines.foreach(i => println(s"line: ${i.map(polySceneLineScala.vertices)}"))

  printBitmap(bitmapFromPolySceneLineSegmentation(polySceneLineScala,50,50))

  val polySceneCellScala = LineSweep(polyScene)

  println("free cells: ")
  polySceneCellScala.freeCells.foreach(i => println(s"freeCell: ${i.map(polySceneCellScala.vertices)}"))

  printBitmap(bitmapFromPolySceneLineSegmentation(VerticalCellToLineSegmentation(polySceneCellScala),50,50))

}


