package org.combinators.ctp.repositories.runinhabitation

import com.sun.xml.internal.ws.policy.privateutil.PolicyUtils.Commons
import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.math3.util.{FastMath, MathUtils}
import org.combinators.cls.interpreter.{InhabitationResult, ReflectedRepository}
import org.combinators.cls.types.Taxonomy
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.pathcoverage.{CamMpTopRepository, JtsUtils}
import org.combinators.ctp.repositories.toplevel._
import org.locationtech.jts.algorithm.Angle
import org.locationtech.jts.geom.{Coordinate, Geometry, GeometryFactory, LineString, LinearRing, MultiLineString, Polygon}
import org.locationtech.jts.geom.impl.CoordinateArraySequence
import org.locationtech.jts.io.WKTReader
import org.locationtech.jts.util.Stopwatch
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.parser.decode


trait RunDecomposition extends LazyLogging with AkkaImplicits with JtsUtils{
  lazy val repository = new CamMpTopRepository {}
  lazy val Gamma = ReflectedRepository(repository, Taxonomy.empty, repository.aluKinding)

  logger.debug("kinding: " + Gamma.substitutionSpace.toString)
  logger.debug("Reflected Repository built, starting inhabitation")

  logger.debug(s"# of combinators: ${Gamma.combinators.size}")

  val watch: Stopwatch = new Stopwatch
  watch.start()

  val ihBatch = Gamma.InhabitationBatchJob[PathCoverageStep](repository.pathCoverageFctRoot)

  def getResultList(b: Gamma.InhabitationBatchJob) = {
    @scala.annotation.tailrec
    def getElements(l: List[InhabitationResult[Any]], bnew: b.ResultType): List[InhabitationResult[Any]] =
      bnew match {
        case (newJob: b.ResultType, result: InhabitationResult[Any]) => getElements(result +: l, newJob)
        case a: InhabitationResult[Any] => a +: l
      }

    getElements(List.empty, b.run())
  }

  val l = getResultList(ihBatch)

  watch.stop()
  logger.debug(s"elapsed time ${watch.getTimeString}")

  l.map(i => logger.debug((if (i.isEmpty) "inhabitant not found" else "inhabitant found") + "," +
    i.target.toString()))

  logger.info(s"${l.last.isInfinite}")
//  (500 to 700).map(i => logger.info(s"${l.last.terms.index(i)}"))

  val wktReader = new WKTReader()
//  val tgtGeo = wktReader.read("""POLYGON ((0 0, 0 10, 50 50, 40 0, 50 -50, 0 0),
//                               |  (18 10, 18 6, 18 -2, 24 -2, 24 6, 30 6, 30 10, 24 10, 18 10))""".stripMargin)
//
//  val tgtGeo = wktReader.read("""POLYGON ((0 0, 0 10, 70 70, 60 0, 70 -50, 0 0),
//                                |  (18 10, 18 6, 18 -2, 24 -2, 24 6, 30 6, 30 10, 24 10, 18 10))""".stripMargin)
//
//  val tgtGeo = wktReader.read("""MULTIPOLYGON (((5 4, 5 60, 40 60, 40 4, 5 4)))""".stripMargin)
  //val tgtGeo = wktReader.read("""POLYGON ((20 20, 20 0, 0 0, 0 20, 20 20)))""".stripMargin)
  val tgtGeo = wktReader.read("""POLYGON ((0 5, 0 95, 0.0759610012173653 95.86824035644531, 0.3015370070934296 96.71009826660156, 0.6698729991912842 97.5, 1.1697779893875122 98.21393585205078, 1.7860620021820068 98.8302230834961, 2.5 99.33012390136719, 3.2898991107940674 99.6984634399414, 4.131759166717529 99.92404174804688, 5 100, 95 100, 95.86824035644531 99.92404174804688, 96.71009826660156 99.6984634399414, 97.5 99.33012390136719, 98.21393585205078 98.8302230834961, 98.8302230834961 98.21393585205078, 99.33012390136719 97.5, 99.6984634399414 96.71009826660156, 99.92404174804688 95.86824035644531, 100 95, 100 5, 99.92404174804688 4.131759166717529, 99.6984634399414 3.2898991107940674, 99.33012390136719 2.5, 98.8302230834961 1.7860620021820068, 98.21393585205078 1.1697779893875122, 97.5 0.6698729991912842, 96.71009826660156 0.3015370070934296, 95.86824035644531 0.0759610012173653, 95 0, 5 0, 4.131759166717529 0.0759610012173653, 3.2898991107940674 0.3015370070934296, 2.5 0.6698729991912842, 1.7860620021820068 1.1697779893875122, 1.1697779893875122 1.7860620021820068, 0.6698729991912842 2.5, 0.3015370070934296 3.2898991107940674, 0.0759610012173653 4.131759166717529, 0 5),
                                |  (50 15, 70 15, 70 20, 79.51886749267578 20, 79.71178436279297 20, 79.92462921142578 20, 80.15074157714844 20, 80.38346862792969 20, 80.61628723144531 20, 80.84294128417969 20, 81.05754852294922 20, 81.254638671875 20, 81.42925262451172 20, 85 20, 85 50, 60 50, 58.263519287109375 49.848079681396484, 56.57979965209961 49.39692687988281, 55 48.660255432128906, 54.42972183227539 48.26094055175781, 54.16383361816406 48.074764251708984, 53.86625289916992 47.866397857666016, 53.57212448120117 47.66044235229492, 53.54927062988281 47.63759231567383, 53.263980865478516 47.35230255126953, 52.974609375 47.062931060791016, 52.689002990722656 46.77732467651367, 52.41493606567383 46.503257751464844, 52.33955764770508 46.42787551879883, 52.192283630371094 46.217552185058594, 52.00510025024414 45.950225830078125, 51.339744567871094 45, 50.60307312011719 43.42020034790039, 50.151920318603516 41.736480712890625, 50 40, 50 15))""".stripMargin)
val str = """{ "vertices" :
            |[[  0.000000,   5.000000,   0.000000],
            | [  0.000000,   5.000000,   3.000000],
            | [  0.000000,  95.000000,   0.000000],
            | [  0.000000,  95.000000,   3.000000],
            | [  0.075961,   4.131759,   0.000000],
            | [  0.075961,   4.131759,   3.000000],
            | [  0.075961,  95.868241,   0.000000],
            | [  0.075961,  95.868241,   3.000000],
            | [  0.301537,   3.289899,   0.000000],
            | [  0.301537,   3.289899,   3.000000],
            | [  0.301537,  96.710101,   0.000000],
            | [  0.301537,  96.710101,   3.000000],
            | [  0.669873,   2.500000,   0.000000],
            | [  0.669873,   2.500000,   3.000000],
            | [  0.669873,  97.500000,   0.000000],
            | [  0.669873,  97.500000,   3.000000],
            | [  1.169778,   1.786062,   0.000000],
            | [  1.169778,   1.786062,   3.000000],
            | [  1.169778,  98.213938,   0.000000],
            | [  1.169778,  98.213938,   3.000000],
            | [  1.786062,   1.169778,   0.000000],
            | [  1.786062,   1.169778,   3.000000],
            | [  1.786062,  98.830222,   0.000000],
            | [  1.786062,  98.830222,   3.000000],
            | [  2.500000,   0.669873,   0.000000],
            | [  2.500000,   0.669873,   3.000000],
            | [  2.500000,  99.330127,   0.000000],
            | [  2.500000,  99.330127,   3.000000],
            | [  3.289899,   0.301537,   0.000000],
            | [  3.289899,   0.301537,   3.000000],
            | [  3.289899,  99.698463,   0.000000],
            | [  3.289899,  99.698463,   3.000000],
            | [  4.131759,   0.075961,   0.000000],
            | [  4.131759,   0.075961,   3.000000],
            | [  4.131759,  99.924039,   0.000000],
            | [  4.131759,  99.924039,   3.000000],
            | [  5.000000,   0.000000,   0.000000],
            | [  5.000000,   0.000000,   3.000000],
            | [  5.000000, 100.000000,   0.000000],
            | [  5.000000, 100.000000,   3.000000],
            | [ 50.000000,  15.000000,   0.000000],
            | [ 50.000000,  15.000000,   3.000000],
            | [ 50.000000,  40.000000,   0.000000],
            | [ 50.000000,  40.000000,   3.000000],
            | [ 50.151922,  41.736482,   0.000000],
            | [ 50.151922,  41.736482,   3.000000],
            | [ 50.603074,  43.420201,   0.000000],
            | [ 50.603074,  43.420201,   3.000000],
            | [ 51.339746,  45.000000,   0.000000],
            | [ 51.339746,  45.000000,   3.000000],
            | [ 52.005100,  45.950225,  -0.000000],
            | [ 52.192284,  46.217551,   0.000000],
            | [ 52.339556,  46.427876,   0.000000],
            | [ 52.339556,  46.427876,   3.000000],
            | [ 52.414936,  46.503257,   0.000000],
            | [ 52.689002,  46.777323,   0.000000],
            | [ 52.974611,  47.062931,   0.000000],
            | [ 53.263982,  47.352302,   0.000000],
            | [ 53.549271,  47.637592,  -0.000000],
            | [ 53.572124,  47.660444,  -0.000000],
            | [ 53.572124,  47.660444,   3.000000],
            | [ 53.866254,  47.866397,  -0.000000],
            | [ 54.163834,  48.074764,   0.000000],
            | [ 54.429720,  48.260940,   0.000000],
            | [ 55.000000,  48.660254,  -0.000000],
            | [ 55.000000,  48.660254,   3.000000],
            | [ 56.579799,  49.396926,   0.000000],
            | [ 56.579799,  49.396926,   3.000000],
            | [ 58.263518,  49.848078,  -0.000000],
            | [ 58.263518,  49.848078,   3.000000],
            | [ 60.000000,  50.000000,   0.000000],
            | [ 60.000000,  50.000000,   3.000000],
            | [ 70.000000,  15.000000,   0.000000],
            | [ 70.000000,  15.000000,   3.000000],
            | [ 70.000000,  20.000000,   0.000000],
            | [ 70.000000,  20.000000,   0.600000],
            | [ 70.000000,  20.000000,   1.200000],
            | [ 70.000000,  20.000000,   3.000000],
            | [ 79.518866,  20.000000,   0.000000],
            | [ 79.711788,  20.000000,   0.000000],
            | [ 79.924628,  20.000000,   0.000000],
            | [ 80.150743,  20.000000,   0.000000],
            | [ 80.383468,  20.000000,   0.000000],
            | [ 80.616284,  20.000000,   0.000000],
            | [ 80.842939,  20.000000,   0.000000],
            | [ 81.057545,  20.000000,   0.000000],
            | [ 81.254641,  20.000000,   0.000000],
            | [ 81.429252,  20.000000,   0.000000],
            | [ 85.000000,  20.000000,   0.000000],
            | [ 85.000000,  20.000000,   3.000000],
            | [ 85.000000,  50.000000,   0.000000],
            | [ 85.000000,  50.000000,   3.000000],
            | [ 95.000000,   0.000000,   0.000000],
            | [ 95.000000,   0.000000,   3.000000],
            | [ 95.000000, 100.000000,   0.000000],
            | [ 95.000000, 100.000000,   3.000000],
            | [ 95.868241,   0.075961,   0.000000],
            | [ 95.868241,   0.075961,   3.000000],
            | [ 95.868241,  99.924039,   0.000000],
            | [ 95.868241,  99.924039,   3.000000],
            | [ 96.710101,   0.301537,   0.000000],
            | [ 96.710101,   0.301537,   3.000000],
            | [ 96.710101,  99.698463,   0.000000],
            | [ 96.710101,  99.698463,   3.000000],
            | [ 97.500000,   0.669873,   0.000000],
            | [ 97.500000,   0.669873,   3.000000],
            | [ 97.500000,  99.330127,   0.000000],
            | [ 97.500000,  99.330127,   3.000000],
            | [ 98.213938,   1.169778,   0.000000],
            | [ 98.213938,   1.169778,   3.000000],
            | [ 98.213938,  98.830222,   0.000000],
            | [ 98.213938,  98.830222,   3.000000],
            | [ 98.830222,   1.786062,   0.000000],
            | [ 98.830222,   1.786062,   3.000000],
            | [ 98.830222,  98.213938,   0.000000],
            | [ 98.830222,  98.213938,   3.000000],
            | [ 99.330127,   2.500000,   0.000000],
            | [ 99.330127,   2.500000,   3.000000],
            | [ 99.330127,  97.500000,   0.000000],
            | [ 99.330127,  97.500000,   3.000000],
            | [ 99.698463,   3.289899,   0.000000],
            | [ 99.698463,   3.289899,   3.000000],
            | [ 99.698463,  96.710101,   0.000000],
            | [ 99.698463,  96.710101,   3.000000],
            | [ 99.924039,   4.131759,   0.000000],
            | [ 99.924039,   4.131759,   3.000000],
            | [ 99.924039,  95.868241,   0.000000],
            | [ 99.924039,  95.868241,   3.000000],
            | [100.000000,   5.000000,   0.000000],
            | [100.000000,   5.000000,   3.000000],
            | [100.000000,  95.000000,   0.000000],
            | [100.000000,  95.000000,   3.000000]],
            | "obstacles" :
            |[[  1,   3,   0],
            | [  5,   1,   0],
            | [  3,   2,   0],
            | [ 42,   0,   2],
            | [  4,   5,   0],
            | [112,   4,   0],
            | [ 40,   0,  42],
            | [112,   0,  40],
            | [  5,   3,   1],
            | [  3,   7,   2],
            | [  7,   6,   2],
            | [ 50,   2,   6],
            | [  2,  44,  42],
            | [  2,  46,  44],
            | [  2,  48,  46],
            | [  2,  50,  48],
            | [  5,   7,   3],
            | [  5,   4,   8],
            | [  4, 112,   8],
            | [  7,   5,  11],
            | [  9,   5,   8],
            | [  5,   9,  13],
            | [  5,  15,  11],
            | [ 17,   5,  13],
            | [ 15,   5,  19],
            | [  5,  17,  21],
            | [  5,  23,  19],
            | [  5,  21,  25],
            | [ 23,   5,  27],
            | [ 29,   5,  25],
            | [  5,  31,  27],
            | [  5,  29,  33],
            | [ 31,   5,  35],
            | [  5,  33,  37],
            | [ 35,   5,  39],
            | [ 93,   5,  37],
            | [  5,  95,  39],
            | [ 43,   5,  41],
            | [ 41,   5, 129],
            | [ 45,   5,  43],
            | [ 47,   5,  45],
            | [ 49,   5,  47],
            | [ 53,   5,  49],
            | [ 60,   5,  53],
            | [131,   5,  60],
            | [ 97,   5,  93],
            | [  5,  99,  95],
            | [  5,  97, 101],
            | [  5, 103,  99],
            | [105,   5, 101],
            | [103,   5, 107],
            | [  5, 105, 109],
            | [107,   5, 111],
            | [113,   5, 109],
            | [  5, 115, 111],
            | [  5, 113, 117],
            | [115,   5, 119],
            | [121,   5, 117],
            | [  5, 123, 119],
            | [  5, 121, 125],
            | [123,   5, 127],
            | [  5, 125, 129],
            | [127,   5, 131],
            | [  6,   7,  10],
            | [ 51,   6,  10],
            | [  6,  51,  50],
            | [  7,  11,  10],
            | [ 12,   9,   8],
            | [112,  12,   8],
            | [ 13,   9,  12],
            | [ 11,  15,  10],
            | [ 15,  14,  10],
            | [ 10,  14,  54],
            | [ 52,  51,  10],
            | [ 54,  52,  10],
            | [ 17,  13,  12],
            | [ 17,  12,  16],
            | [ 12, 112,  16],
            | [ 14,  15,  18],
            | [ 55,  14,  18],
            | [ 54,  14,  55],
            | [ 15,  19,  18],
            | [ 17,  16,  20],
            | [ 16, 112,  20],
            | [ 21,  17,  20],
            | [ 19,  23,  18],
            | [ 23,  22,  18],
            | [ 22,  56,  18],
            | [ 56,  55,  18],
            | [ 24,  21,  20],
            | [112,  24,  20],
            | [ 21,  24,  25],
            | [ 22,  23,  26],
            | [ 57,  22,  26],
            | [ 22,  57,  56],
            | [ 23,  27,  26],
            | [ 24,  29,  25],
            | [ 29,  24,  28],
            | [ 24, 112,  28],
            | [ 27,  31,  26],
            | [ 31,  30,  26],
            | [ 58,  26,  30],
            | [ 26,  58,  57],
            | [ 32,  29,  28],
            | [112,  32,  28],
            | [ 29,  32,  33],
            | [ 30,  31,  34],
            | [ 34,  61,  30],
            | [ 59,  58,  30],
            | [ 30,  61,  59],
            | [ 31,  35,  34],
            | [ 33,  32,  37],
            | [ 37,  32,  36],
            | [ 32, 112,  36],
            | [ 34,  35,  38],
            | [ 34,  38,  62],
            | [ 34,  62,  61],
            | [ 35,  39,  38],
            | [ 93,  37,  36],
            | [ 92,  93,  36],
            | [112,  92,  36],
            | [ 39,  95,  38],
            | [ 63,  62,  38],
            | [ 64,  63,  38],
            | [ 38,  66,  64],
            | [ 38,  94,  66],
            | [ 95,  94,  38],
            | [ 43,  41,  40],
            | [ 40,  41,  73],
            | [ 42,  43,  40],
            | [ 72,  40,  73],
            | [ 40,  72, 112],
            | [129,  73,  41],
            | [ 43,  42,  45],
            | [ 45,  42,  44],
            | [ 45,  44,  47],
            | [ 47,  44,  46],
            | [ 47,  46,  49],
            | [ 49,  46,  48],
            | [ 49,  48,  53],
            | [ 53,  48,  50],
            | [ 53,  50,  51],
            | [ 53,  51,  52],
            | [ 53,  52,  60],
            | [ 60,  52,  54],
            | [ 60,  54,  55],
            | [ 60,  55,  56],
            | [ 60,  56,  57],
            | [ 60,  57,  58],
            | [ 60,  58,  59],
            | [ 60,  59,  65],
            | [ 65,  59,  61],
            | [131,  60,  65],
            | [ 65,  61,  62],
            | [ 65,  62,  63],
            | [ 65,  63,  64],
            | [ 65,  64,  67],
            | [ 67,  64,  66],
            | [131,  65,  67],
            | [ 67,  66,  69],
            | [ 69,  66,  68],
            | [ 94,  68,  66],
            | [131,  67,  69],
            | [ 69,  68,  71],
            | [ 71,  68,  70],
            | [ 94,  70,  68],
            | [131,  69,  71],
            | [ 91,  71,  70],
            | [ 90,  91,  70],
            | [ 90,  70,  94],
            | [ 91, 131,  71],
            | [ 72,  73,  76],
            | [ 72,  75,  74],
            | [ 72,  74,  78],
            | [ 75,  72,  76],
            | [ 72,  78, 112],
            | [ 76,  73,  77],
            | [ 89,  77,  73],
            | [129,  89,  73],
            | [ 74,  75,  78],
            | [ 75,  76,  88],
            | [ 78,  75,  79],
            | [ 79,  75,  80],
            | [ 80,  75,  81],
            | [ 81,  75,  82],
            | [ 82,  75,  83],
            | [ 83,  75,  84],
            | [ 84,  75,  85],
            | [ 85,  75,  86],
            | [ 86,  75,  87],
            | [ 87,  75,  88],
            | [ 76,  77,  89],
            | [ 88,  76,  89],
            | [ 78,  79, 112],
            | [112,  79,  80],
            | [ 81, 112,  80],
            | [ 81,  82, 112],
            | [ 82,  83, 112],
            | [ 83,  84, 112],
            | [ 84,  85, 112],
            | [ 85,  86, 112],
            | [ 86,  87, 112],
            | [ 87,  88, 112],
            | [ 91,  88,  89],
            | [ 88,  91,  90],
            | [ 90,  94,  88],
            | [ 94, 112,  88],
            | [129,  91,  89],
            | [129, 131,  91],
            | [ 97,  93,  92],
            | [ 96,  97,  92],
            | [112,  96,  92],
            | [ 95,  98,  94],
            | [ 98, 112,  94],
            | [ 99,  98,  95],
            | [ 97,  96, 100],
            | [ 96, 112, 100],
            | [ 97, 100, 101],
            | [103,  98,  99],
            | [ 98, 103, 102],
            | [112,  98, 102],
            | [100, 105, 101],
            | [104, 105, 100],
            | [112, 104, 100],
            | [103, 106, 102],
            | [106, 112, 102],
            | [103, 107, 106],
            | [105, 104, 108],
            | [104, 112, 108],
            | [109, 105, 108],
            | [106, 107, 110],
            | [112, 106, 110],
            | [110, 107, 111],
            | [113, 109, 108],
            | [112, 113, 108],
            | [115, 110, 111],
            | [114, 112, 110],
            | [115, 114, 110],
            | [113, 112, 116],
            | [112, 114, 118],
            | [116, 112, 120],
            | [122, 112, 118],
            | [112, 124, 120],
            | [112, 122, 126],
            | [124, 112, 128],
            | [112, 126, 130],
            | [112, 130, 128],
            | [113, 116, 117],
            | [114, 115, 118],
            | [115, 119, 118],
            | [116, 121, 117],
            | [121, 116, 120],
            | [119, 123, 118],
            | [123, 122, 118],
            | [124, 121, 120],
            | [125, 121, 124],
            | [122, 123, 126],
            | [126, 123, 127],
            | [125, 124, 128],
            | [129, 125, 128],
            | [126, 127, 131],
            | [126, 131, 130],
            | [131, 129, 128],
            | [130, 131, 128]],
            | "boundaries" :
            |[]}""".stripMargin
  //val tgtGeo = str2Wkt(str)

  // val tgtGeo = wktReader.read("""POLYGON ((-10 42, 15 42, 15 35, 17 35, 17 42, 62 42, 62 -33, -10 -33, -10 42))""".stripMargin)
//  val tgtGeo = wktReader.read("""POLYGON ((-55.5 77.5, 121 77.5, 121 -87, -55.5 -87, -55.5 77.5),
//                                |  (10 10, 10 -30, 30 -30, 30 -10, 50 -10, 50 20, 20 20, 20 19.995327360502255, 19.470765458798372 19.985985720017784, 18.767682731028415 19.92377922711879, 18.070739345905032 19.812132965291546, 17.383407440315594 19.651603150596497, 16.7091112672399 19.44298953449556, 16.051210136294088 19.18733141953056, 15.412981677819097 18.88590248157027, 14.79760551389118 18.540204424421262, 14.208147417604668 18.151959498415124, 13.647544039544615 17.72310192024389, 13.118588277541312 17.255768236789827, 12.623915362593777 16.752286680956306, 12.165989730281474 16.215165572528594, 11.747092743070228 15.64708082185087, 11.36931132467916 15.050862598575474, 11.034527563131714 14.42948123190008, 10.74440933428779 13.786032412537036, 10.500401992570092 13.12372177013794, 10.303721170281118 12.445848903008002, 10.155346721384166 11.755790939673208, 10.05601783992018 11.05698571419593, 10.006229377380219 10.352914639058527, 10.006229377380219 10, 10 10))""".stripMargin)

  val scene = Cnc2DModel(boundaries = List(-20.0f, 80.0f, -60.0f, 60.0f),
    targetGeometry = tgtGeo, rest = List(tgtGeo), machined = List.empty[Geometry])

  def str2Wkt(s: String) = {
    val pSceneOpt = decode[PolygonScene](s) match {
      case Left(s) => logger.info(s"Could not decode String: $s")
        None
      case Right(s) => Some(s)
    }
    val vertices = pSceneOpt.map(pScene => pScene.obstacles.map(o => o.map(vid => pScene.vertices(vid)))).getOrElse(List.empty)
    val polyList = vertices.filter(v => v.map(point => point(2)).contains(0.0)).map(poly => poly.map(asCoordinate)).map(getPolygonByCoordList)
    val multiPoly = gf.createMultiPolygon(polyList.toArray)
    logger.info(s"multiPoly: $multiPoly")
    multiPoly
  }
  val config = PathCoverageStepConfig()

  def printSummary(index: Int, pcr: PathCoverageResult): Unit = {
    val endScene = pcr.computeModelHistory._1.last
    val pathList = pcr.computeModelHistory._2
    logger.info(s"index: $index")
    logger.info(s"target geo: \r\n${scene.targetGeometry}")
    logger.info(s"endScene Rest head: \r\n${endScene.rest.head}")
    logger.info(s"endScene RestMultiGeo: \r\n${endScene.getRestMultiGeo}")
    logger.info(s"endScene Machined: \r\n${endScene.getMachinedMultiGeo}")
    logger.info(s"pathList length: ${pathList.length}")
    logger.info(s"Result pathList: \r\n${pathList}")
    logger.info(s"Corrupt paths : \r\n${pathList.filterNot(_.length > 1)}")
    //logger.info(s"pathAndVfMax: ${pcr.withMaxVfByAngle}")
    val rPath = new MultiLineString(pathList.filter(_.length > 1).map(p =>
      new LineString(
        new CoordinateArraySequence(
          p.map(i => new Coordinate(i(0), i(1))).toArray), new GeometryFactory())).toArray, gf)
    logger.info(s"Result path: \r\n$rPath")
//    logger.info(s"pcr ${pcr.withMaxVfByAcc}")
//    pcr.printAll()
  }
}

object RunCncPathCoverage extends App with RunDecomposition {
  lazy val lines = Iterator.continually(scala.io.StdIn.readLine()).takeWhile(_ != "exit")
  while (lines.hasNext) {
    lines.next() match {
      case inputString if inputString.contains("-") =>
        val values = inputString.split("-").map(i => i.toIntOption).filter {
          case Some(a) => true
          case _ => false
        }.map(_.get)
        val results = if (values.length > 1)
          (values.head to values.last) map { i: Int =>
            val fct = l.last.interpretedTerms.index(i).asInstanceOf[PathCoverageStep]
            val pcr = PathCoverageResult( scene, config, List(fct))
            (i, pcr)
          }
        else List.empty

        if(results.nonEmpty){
        val (index, pcr) = results.minBy { case (_, p:PathCoverageResult) => p.computeModelHistory._1.last.getRestMultiGeo.getArea }
          printSummary(index, pcr)
        } else {
          logger.info("""Wrong format. Please use "startIndex-endIndex", eg. "100-200"""")
        }

        logger.info("Done")
      case inputString if inputString.equals("it") =>
        def getResults(accList: List[(Int, PathCoverageResult)], i: Int): List[(Int, PathCoverageResult)] = {
          if (accList.size > 10) {
            accList
          }
          else {
            val fct = l.last.interpretedTerms.index(i).asInstanceOf[PathCoverageStep]
            val pcr = PathCoverageResult(scene, config, List(fct))
            val restarea = pcr.computeModelHistory._1.last.getRestMultiGeo.getArea
            val initialRest = scene.getRestMultiGeo.getArea
            val percentage = restarea / initialRest
            val newList = if (percentage < 0.005) {
              accList :+ (i, pcr)
            }
            else {
              accList
            }
            getResults(newList, i + 1)
          }
        }

        val selectedResults = getResults(List.empty, 0)
        logger.info(s"selected Indizes: ${selectedResults.map(_._1)}")
        logger.info("Done")

      case inputString =>
        inputString.toIntOption match {
          case Some(i) =>
            logger.info(s"${l.last.terms.index(i)}")
            val pcs =
              l.last.interpretedTerms.index(i).asInstanceOf[PathCoverageStep]
            //logger.info(s"info: $path")
            val pcr = PathCoverageResult( scene, config, List(pcs))
            val endScene = pcr.endScene
            val path = pcr.pathList
            logger.info(s"target geo: \r\n${scene.targetGeometry}")
            logger.info(s"endScene Rest head: \r\n${endScene.rest.head}")
            logger.info(s"endScene RestMultiGeo: \r\n${endScene.getRestMultiGeo}")
            logger.info(s"endScene Machined: \r\n${endScene.getMachinedMultiGeo}")
            logger.info(s"path length: ${path.length}")
            logger.info(s"Result path: \r\n${path}")
            logger.info(s"Corrupt paths : \r\n${path.filterNot(_.length > 1)}")

            def extractZZeroLines(p: List[List[Float]]): List[List[List[Float]]] = {
              if (p.isEmpty)
                List.empty[List[List[Float]]]
              else if (p.head.size == 3 && p.head(2) == 20.0f)
                extractZZeroLines(p.tail)
              else {
                val asd = p.takeWhile(i => i.size < 3 || i(2) == 0.0f)
                logger.info(s"asd size ${asd.size}")
                logger.info(s"p before ${p.size}")
                logger.info(s"p after ${p.drop(asd.size)}")
                if(asd.isEmpty)
                  extractZZeroLines(p.dropWhile(i => i.size > 2 && i(2) != 0.0f))
                else
                  asd +: extractZZeroLines(p.drop(asd.size))
              }
            }

            val newPath = path.filter(_.length > 1).map(extractZZeroLines).reduceOption(_ ++ _).getOrElse(List.empty)
            if(newPath.nonEmpty){
            val toPaths = newPath.map{ singleCompPathlist =>
              new LineString(
                new CoordinateArraySequence(
                  singleCompPathlist.map(c => new Coordinate(c(0), c(1))).toArray)
              , gf)}

              val mlString = new MultiLineString(toPaths.toArray,gf)

            logger.info(s"Result path: \r\n${mlString}")
            pcr.computeModelHistory._1.foreach(i => logger.info(s"\r\n${i.getMachinedMultiGeo}"))
//            logger.info(s"pathAndVfMax: ${pcr.withMaxVfByAngle}")
//            logger.info(s"pathAndVfMax: ${pcr.maxVfByToolAndAngle}")
//            logger.info(s"pcr ${pcr.withMaxVfByAcc}")
//            pcr.printAll()
            }
            else{
              logger.info(s"empty path")
            }
          //          logger.info(s"Result path: \r\n${
          //          path.filter(_.length > 1).map(p =>
          //            new LineString(
          //              new CoordinateArraySequence(p.map(i => new Coordinate(i(0), i(1))).toArray), new GeometryFactory())).
          //            reduce[Geometry] { case (a: Geometry, b: Geometry) => a.union(b) }
          //        }")
          //  ToAkka(path)
          case None => logger.info("ignoring user input")
        }
    }
  }

  logger.info("Done")
}