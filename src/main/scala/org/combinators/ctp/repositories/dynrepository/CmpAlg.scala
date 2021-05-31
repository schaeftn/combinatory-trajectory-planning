package org.combinators.ctp.repositories.dynrepository

import java.util.UUID

import akka.Done
import akka.stream.alpakka.mqtt.MqttMessage
import akka.stream.scaladsl.{Sink, Source}
import com.typesafe.scalalogging.LazyLogging
import org.combinators.cls.interpreter.{InhabitationResult, ReflectedRepository}
import org.combinators.cls.types.Constructor
import org.combinators.ctp.repositories.samplebased.{SbmpPlannerTemplateRepository, SbmpPlannerTemplateRepositoryWithStates, SbmpTopLevelRepository}
import org.combinators.ctp.repositories.taxkinding.{CtpSemanticTypes, SbmpSemanticTypes}
import io.circe.parser.decode
import io.circe.generic.auto._
import io.circe.syntax._
import org.combinators.cls.types.syntax._
import org.combinators.cls.inhabitation.Repository
import org.combinators.ctp.repositories.python_interop.{PlannerScheme, SubstitutionSchema}
import org.combinators.ctp.repositories.toplevel.{AkkaImplicits, AkkaMqttTopLevelCmp, AkkaMqttTopLevelSbmp, EncodeImplicits, MpTaskStartGoal, PolySceneCellSegmentation, PolySceneSegmentationRoadmap, PolySceneSegmentationRoadmapPath, PolygonScene, ProblemDefinitionFiles, Scene, SceneSRT}
import org.locationtech.jts.util.Stopwatch
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.cmp.CmpTopLevelRepository
import org.combinators.ctp.repositories.graphsearch.GraphSearchRepository
import org.combinators.ctp.repositories.scene.SceneRepository
import scalax.collection.Graph
import scalax.collection.edge.WUnDiEdge

import scala.concurrent.Future


/*

 val cmpFullKindingMap = Map(
    dimensionality_var -> Seq(
      dimensionality_two_d_t,
      dimensionality_three_d_t,
      dimensionality_n_d_t),
    sd_cell_type_var -> Seq(
      sd_cell_triangle_type,
      sd_cell_vertical_type),
    sd_poly_scene_cell_segmentation_var -> Seq(
      sd_vertical_cell_decomposition_type,
      sd_seg_triangles_simple_type,
      sd_seg_triangles_para_type,
      sd_seg_grid_type
    ),
    rmc_cellGraph_var -> Seq(
      rmc_cg_centroidsOnly,
      rmc_cg_allVertices,
      rmc_cg_centroidCellVertices),
    rmc_usingCentroids_var -> Seq(
      rm_withCentroids_type,
      rm_withoutCentroids_type),
    rmc_centroidFct_var -> Seq(
      cFct_centroids_naive_type,
      cFct_jts_default_type,
      cFct_jts_incentre_type,
      cFct_avg_type,
      triangle_centroidsFctNd_type),
    rmc_connectorNodes_var -> Seq(
      rmc_cn_withConnectorNodes,
      rmc_cn_withoutConnectorNodes),
    rmc_cellNodeAddFct_var -> Seq(
      rmc_cna_withCellNodes_type,
      rmc_cna_withoutCellNodes_type),
    rmc_startGoalFct_var -> Seq(
      rmc_startGoal_nn_type,
      rmc_startGoal_cellbased_type),
    cmp_graph_algorithm_var -> Seq(
      cmp_graph_dijkstra_type,
      cmp_graph_a_star_type,
      cmp_graph_vbi_type,
      cmp_graph_mst_type,
      cmp_graph_tsp_type)*

*/

//TODO out path only or graph path
case class CmpAlg(cellType: CmpCellType.EnumType,
                  cellSegmentation: CmpCellSegmentation.EnumType,
                  cellGraph: CmpCellGraph.EnumType,
                  centroidFct: CmpCentroidFct.EnumType,
                  usingCentroids: Boolean,
                  usingConnectorNodes: Boolean,
                  usingCellNodes: Boolean,
                  startGoalFct: CmpStartGoalFct.EnumType,
                  graphAlg: CmpGraphAlg.EnumType,
                  sceneInput: SceneInput.EnumType,
                  dimensionality: Dimensionality.EnumType,
                  id: UUID
                 ) extends LazyLogging with DynAlgDef {
  self =>
  val cmpTopLevelRepository = new CmpTopLevelRepository with GraphSearchRepository {}
  val akkaTopLevelRepository = new AkkaMqttTopLevelCmp {}
  val sceneRepository = new SceneRepository {}
  //  val startGoalFct =  CmpStartGoalFct.rmc_startGoal_nn_type
  //  val graphAlg= CmpGraphAlg.cmp_graph_a_star_type
  //  val sceneInput= SceneInput.scene_input_from_mqtt
  //val = Dimensionality.dimensionality_two_d_t
  //  val id =  UUID.fromString("7c7c12c5-b125-4385-b2ca-56dd9afd03e4")


  //  "usingCentroids": true,
  //  "usingConnectorNodes": false,
  //  "usingCellNodes": true,
  //  "startGoalFct": " rmc_startGoal_nn_type",
  //  "graphAlg": " cmp_graph_a_star_type",
  //  "sceneInput": "scene_input_from_mqtt",
  //  "dimensionality": "dimensionality_two_d_t",
  //  "id": "7c7c12c5-b125-4385-b2ca-56dd9afd03e4"


  def addCmpTopLevelCombinator[R]: ReflectedRepository[R] => ReflectedRepository[R] = { r =>
    sceneInput match {
      case SceneInput.scene_input_data_file =>
        r.addCombinator(cmpTopLevelRepository.CmpTopLevelCombinatorFileBasedRmSegTax)
      case SceneInput.scene_input_mqtt => r.addCombinator(cmpTopLevelRepository.CmpTopLevelCombinatorTax)
    }
  }

  def addAkkaCombinators[R]: ReflectedRepository[R] => ReflectedRepository[R] = { r =>
    def addAkkaComponents[R]: ReflectedRepository[R] => ReflectedRepository[R] = { r =>
      dimensionality match {
        case Dimensionality.dimensionality_two_d_t =>
          r.addCombinator(akkaTopLevelRepository.UnityMqttAkkaSourceScene).
            addCombinator(akkaTopLevelRepository.UnityMqttAkkaSinkSceneSegGraphPathTax).
            addCombinator(akkaTopLevelRepository.UnityMqttAkkaSourceTask2D)
        case Dimensionality.dimensionality_three_d_t =>
          r.addCombinator(akkaTopLevelRepository.UnityMqttAkkaSourceScene3D).
            addCombinator(akkaTopLevelRepository.UnityMqttAkkaSinkSceneSegGraphPathTax).
            addCombinator(akkaTopLevelRepository.UnityMqttAkkaSourceTask3D)
      }
    }

    def addAkkaTopLevelCombinators[R]: ReflectedRepository[R] => ReflectedRepository[R] = { r =>
      sceneInput match {
        case SceneInput.scene_input_data_file =>
          r.addCombinator(akkaTopLevelRepository.AkkaCmpProbFilesTax)
        case SceneInput.scene_input_mqtt =>
          r.addCombinator(akkaTopLevelRepository.AkkaGraphSceneSegTax)
      }
    }

    addAkkaComponents(addAkkaTopLevelCombinators(r))
  }

  def addRoadMapCombinators[R]: ReflectedRepository[R] => ReflectedRepository[R] = { r =>
    def addEdgeCombinators[R]: ReflectedRepository[R] => ReflectedRepository[R] = { r =>
      cellGraph match {
        case CmpCellGraph.rmc_cg_allVertices => r.addCombinator(cmpTopLevelRepository.EdgesAllCellVertices)
        case CmpCellGraph.rmc_cg_centroidCellVertices =>
          r.addCombinator(cmpTopLevelRepository.EdgesCentroidToCellVertices)
        case CmpCellGraph.rmc_cg_centroidsOnly => r.addCombinator(cmpTopLevelRepository.EdgesCentroidOnly)
        case CmpCellGraph.not_specified =>
          logger.info("CellGraph not specified, defaulting to edges between all cell vertices")
          r.addCombinator(cmpTopLevelRepository.EdgesAllCellVertices)
      }
    }

    def addCentroidFct[R]: ReflectedRepository[R] => ReflectedRepository[R] = {
      r =>
        if (usingCentroids)
          centroidFct match {
            case CmpCentroidFct.cFct_centroids_naive_type =>
              r.addCombinator(cmpTopLevelRepository.CellToCentroidCellNaive)
            case CmpCentroidFct.cFct_jts_default_type =>
              r.addCombinator(cmpTopLevelRepository.CellToCentroidCellJTSCentroids)
            case CmpCentroidFct.cFct_jts_incentre_type =>
              r.addCombinator(cmpTopLevelRepository.CentroidFctJTSIncentre)
            case CmpCentroidFct.cFct_avg_type => r.addCombinator(cmpTopLevelRepository.CellToCentroidND)
            case CmpCentroidFct.triangle_centroidsFctNd_type =>
              r.addCombinator(cmpTopLevelRepository.CellToCentroidND)
          }
        else r
    }

    def addConnectorNodeFct[R]: ReflectedRepository[R] => ReflectedRepository[R] = { r =>
      if (usingConnectorNodes) r.addCombinator(cmpTopLevelRepository.WithConnectorNodes)
      else r
    }

    def addCellNodeFct[R]: ReflectedRepository[R] => ReflectedRepository[R] = { r =>
      if (usingCellNodes) r.addCombinator(cmpTopLevelRepository.WithCellVertices)
      else r
    }

    def addNNCombinator[R]: ReflectedRepository[R] => ReflectedRepository[R] = { r =>
      dimensionality match {
        case Dimensionality.dimensionality_two_d_t => r.addCombinator(cmpTopLevelRepository.Neighbours2D)
        case Dimensionality.dimensionality_three_d_t => r.addCombinator(cmpTopLevelRepository.Neighbours3D)
        case _ => r
      }
    }

    def addStartGoalCombinator[R]: ReflectedRepository[R] => ReflectedRepository[R] = { r =>
      (startGoalFct, cellGraph) match {
        case (CmpStartGoalFct.rmc_startGoal_cellbased_type, CmpCellGraph.rmc_cg_centroidsOnly) =>
          r.addCombinator(cmpTopLevelRepository.NodesEdgesStartEndCentroid)
        case (CmpStartGoalFct.rmc_startGoal_cellbased_type, CmpCellGraph.rmc_cg_centroidCellVertices) =>
          r.addCombinator(cmpTopLevelRepository.NodesEdgesStartEndCentroid)
        case (CmpStartGoalFct.rmc_startGoal_cellbased_type, CmpCellGraph.rmc_cg_allVertices) =>
          r.addCombinator(cmpTopLevelRepository.NodesEdgesStartEndCellBased)
        case (CmpStartGoalFct.rmc_startGoal_nn_type, _) =>
          r.addCombinator(cmpTopLevelRepository.NodesEdgesStartEndNearest)
        case _ => r.addCombinator(cmpTopLevelRepository.NodesEdgesStartEndNearest)
      }
    }

    addCmpTopLevelCombinator(
      addStartGoalCombinator(
        addNNCombinator(
          addCellNodeFct(
            addConnectorNodeFct(
              addCentroidFct(
                addEdgeCombinators(
                  r.addCombinator(cmpTopLevelRepository.WithoutNewNodes).
                    addCombinator(cmpTopLevelRepository.RoadmapCombinatorTax))))))))
  }

  def addGraphTraversalCombinator[R]: ReflectedRepository[R] => ReflectedRepository[R] = { r =>
    graphAlg match {
      case CmpGraphAlg.cmp_graph_a_star_type => r.addCombinator(cmpTopLevelRepository.AStarCombinator)
      case CmpGraphAlg.cmp_graph_dijkstra_type => r.addCombinator(cmpTopLevelRepository.DijkstraSpCombinator)
      case CmpGraphAlg.cmp_graph_mst_type => r.addCombinator(cmpTopLevelRepository.GraphMst)
      case CmpGraphAlg.cmp_graph_tsp_type => r.addCombinator(cmpTopLevelRepository.GraphTsp)
      case _ => logger.warn("graphAlg match case not handled. Defaulting to Dijkstra")
        r.addCombinator(cmpTopLevelRepository.DijkstraSpCombinator)
    }
  }

  def addCellSegmentationCombinator[R]: ReflectedRepository[R] => ReflectedRepository[R] = { r =>
    cellSegmentation match {
      case CmpCellSegmentation.sd_seg_grid_type => r.addCombinator(cmpTopLevelRepository.GridCombinator)
      case CmpCellSegmentation.sd_seg_triangles_para_type => r.addCombinator(cmpTopLevelRepository.TriangulatePolyParametrized)
      case CmpCellSegmentation.sd_seg_triangles_simple_type => r.addCombinator(cmpTopLevelRepository.TriangulatePoly)
      case CmpCellSegmentation.sd_vertical_cell_decomposition_type => r.addCombinator(cmpTopLevelRepository.VcdLineSweepJTS)
      case _ => logger.warn("cellSegmentation match case not handled. Defaulting to parameterized triangulation")
        r.addCombinator(cmpTopLevelRepository.TriangulatePolyParametrized)
    }
  }

  def addSceneTransformationCombinators[R]: ReflectedRepository[R] => ReflectedRepository[R] = { r =>
    dimensionality match {
      case Dimensionality.dimensionality_two_d_t if
      cellSegmentation ==CmpCellSegmentation.sd_vertical_cell_decomposition_type  =>
        r.addCombinator(sceneRepository.SceneToScenePolyTax).
        addCombinator(sceneRepository.ObstacleSceneBoundingCut2D).
        addCombinator(sceneRepository.AabbGen2D).
        addCombinator(sceneRepository.CubeToPolyTax).
        addCombinator(sceneRepository.ApplyAffineTransform2DVertexList).
        addCombinator(sceneRepository.ApplyAffineTransform2D).
        addCombinator(sceneRepository.RectangleVertices2D)
      case Dimensionality.dimensionality_two_d_t =>
        r.addCombinator(sceneRepository.SceneToScenePolyTax).
        addCombinator(sceneRepository.ObstacleSceneNoBoundingCut2D).
        addCombinator(sceneRepository.AabbGen2D).
        addCombinator(sceneRepository.CubeToPolyTax).
        addCombinator(sceneRepository.ApplyAffineTransform2DVertexList).
        addCombinator(sceneRepository.ApplyAffineTransform2D).
        addCombinator(sceneRepository.RectangleVertices2D)
      case Dimensionality.dimensionality_three_d_t =>
        r.addCombinator(sceneRepository.SceneToScenePolyTax).
          addCombinator(sceneRepository.ObstacleSceneBoundingCut3D).
          addCombinator(sceneRepository.AabbGen3D).
          addCombinator(sceneRepository.CubeToPolyTax).
          addCombinator(sceneRepository.CubeToSurfaceMeshTax).
          addCombinator(sceneRepository.ApplyAffineTransform3D).
          addCombinator(sceneRepository.ApplyAffineTransform3DVertexList).
          addCombinator(sceneRepository.CubeVertices3D)
      case _ => logger.warn(f"Dimensionality case not matched while adding scene transformation combinators: $dimensionality")
        r
    }
  }

  def buildRepository: ReflectedRepository[EmptyClass] = {
    logger.info("Building cmp repository")
    val test = new EmptyClass()
    val r = ReflectedRepository[EmptyClass](test, classLoader =
      this.getClass.getClassLoader, semanticTaxonomy = new CtpSemanticTypes {}.cmp_taxonomy)
    val rNew = addSceneTransformationCombinators(
      addAkkaCombinators(
        addRoadMapCombinators(
          addGraphTraversalCombinator(
            addCellSegmentationCombinator(r)))))

    logger.info("Repository built")
    rNew
  }


  def getIhResult[T](r: ReflectedRepository[T]): r.InhabitationBatchJob = {
    sceneInput match {
      case SceneInput.`scene_input_data_file` =>
        r.InhabitationBatchJob[ProblemDefinitionFiles => Graph[List[Float], WUnDiEdge]](
          cmp_sceneSegFct_type :&: cmp_cell_graph_fct_type)
          .addJob[(Graph[List[Float], WUnDiEdge], MpTaskStartGoal) => Seq[List[Float]]](
            cmp_any_graph_algorithm_type)
          .addJob[Sink[MqttMessage, Future[Done]]](p_mqttAkkaSink_type :&: cmp_scene_graph_path)
          .addJob[Unit](p_mqttAkkaComposition_type :&: cmp_scene_graph_path)
      case SceneInput.scene_input_mqtt =>
        r.InhabitationBatchJob[Scene => PolygonScene](sd_unity_scene_type =>: sd_polygon_scene_type)
          .addJob[PolygonScene => PolySceneCellSegmentation](cmp_sceneSegFct_type)
          .addJob[(PolySceneCellSegmentation, MpTaskStartGoal) => PolySceneSegmentationRoadmap](cmp_cell_graph_fct_type)
          .addJob[(Graph[List[Float], WUnDiEdge], MpTaskStartGoal) => Seq[List[Float]]](cmp_any_graph_algorithm_type)
          .addJob[(Scene, MpTaskStartGoal) => PolySceneSegmentationRoadmapPath](cmp_algorithm_type)
      //          .addJob[Source[Option[Scene], Future[Done]]](p_mqttAkkaSource_type :&: sd_unity_scene_type)
      //          .addJob[Source[Option[MpTaskStartGoal], Future[Done]]](p_mqttAkkaSource_type :&: mpt_start_goal_position_type)
      //          .addJob[Sink[MqttMessage, Future[Done]]](p_mqttAkkaSink_type :&: cmp_scene_graph_path)
      //          .addJob[Unit](p_mqttAkkaComposition_type :&: cmp_scene_graph_path)
      case _ =>
        r.InhabitationBatchJob[ProblemDefinitionFiles => Graph[List[Float], WUnDiEdge]](
          cmp_sceneSegFct_type :&: cmp_cell_graph_fct_type)
          .addJob[(Graph[List[Float], WUnDiEdge], MpTaskStartGoal) => Seq[List[Float]]](
            cmp_any_graph_algorithm_type)
          .addJob[Sink[MqttMessage, Future[Done]]](p_mqttAkkaSink_type :&: cmp_scene_graph_path)
          .addJob[Unit](p_mqttAkkaComposition_type :&: cmp_scene_graph_path)
    }
  }

  /*def getIhResult[T](r: ReflectedRepository[T]): r.InhabitationBatchJob = {
    sceneInput match {
      case SceneInput.`scene_input_data_file` =>
        r.InhabitationBatchJob[ProblemDefinitionFiles => Graph[List[Float], WUnDiEdge]](
          cmp_sceneSegFct_type :&: cmp_cell_graph_fct_type)
          .addJob[(Graph[List[Float], WUnDiEdge], MpTaskStartGoal) => Seq[List[Float]]](
            cmp_any_graph_algorithm_type)
          .addJob[Sink[MqttMessage, Future[Done]]](p_mqttAkkaSink_type :&: cmp_scene_graph_path)
          .addJob[Unit](p_mqttAkkaComposition_type :&: cmp_scene_graph_path)
      case SceneInput.scene_input_mqtt =>
        r.InhabitationBatchJob[Scene => PolygonScene](sd_unity_scene_type =>: sd_polygon_scene_type)
          .addJob[PolygonScene => PolySceneCellSegmentation](cmp_sceneSegFct_type)
          .addJob[(PolySceneCellSegmentation, MpTaskStartGoal) => PolySceneSegmentationRoadmap](cmp_cell_graph_fct_type)
          .addJob[(Graph[List[Float], WUnDiEdge], MpTaskStartGoal) => Seq[List[Float]]](cmp_any_graph_algorithm_type)
          .addJob[(Scene, MpTaskStartGoal) => PolySceneSegmentationRoadmapPath](cmp_algorithm_type)
          .addJob[Source[Option[Scene], Future[Done]]](p_mqttAkkaSource_type :&: sd_unity_scene_type)
          .addJob[Source[Option[MpTaskStartGoal], Future[Done]]](p_mqttAkkaSource_type :&: mpt_start_goal_position_type)
          .addJob[Sink[MqttMessage, Future[Done]]](p_mqttAkkaSink_type :&: cmp_scene_graph_path)
          .addJob[Unit](p_mqttAkkaComposition_type :&: cmp_scene_graph_path)
      case _ =>
        r.InhabitationBatchJob[ProblemDefinitionFiles => Graph[List[Float], WUnDiEdge]](
          cmp_sceneSegFct_type :&: cmp_cell_graph_fct_type)
          .addJob[(Graph[List[Float], WUnDiEdge], MpTaskStartGoal) => Seq[List[Float]]](
            cmp_any_graph_algorithm_type)
          .addJob[Sink[MqttMessage, Future[Done]]](p_mqttAkkaSink_type :&: cmp_scene_graph_path)
          .addJob[Unit](p_mqttAkkaComposition_type :&: cmp_scene_graph_path)
    }
  }*/
}

object RunCmpTest extends App with EncodeImplicits with LazyLogging with AkkaImplicits {
  //  val fpp2 = SbmpAlg().withPlanner(SbmpPlanners.sbmp_planner_BITstar).
  //    withSampler(SbmpSamplers.sbmp_gaussian_valid_state_sampler).
  //    withStateValidator(SbmpStateValidators.sbmp_fcl_validator).
  //    withMotionValidator(SbmpMotionValidators.sbmp_discrete_motion_validator).
  //    withSimplification(SbmpSimplification.sbmp_use_simplification).
  //    withSceneInput(SbmpSceneInput.sbmp_from_data_file)

  val str =
    """{
      |	"cellType": "sd_cell_triangle_type",
      |	"cellSegmentation": "sd_seg_triangles_para_type",
      |	"cellGraph": "rmc_cg_centroidCellVertices",
      |	"centroidFct": "cFct_centroids_naive_type",
      | "usingCentroids": true,
      | "usingConnectorNodes": false,
      | "usingCellNodes": true,
      | "startGoalFct": "rmc_startGoal_nn_type",
      | "graphAlg": "cmp_graph_a_star_type",
      | "sceneInput": "scene_input_mqtt",
      | "dimensionality": "dimensionality_two_d_t",
      | "id": "7c7c12c5-b125-4385-b2ca-56dd9afd03e4"
      |}""".stripMargin

  println(str)
  val fpp2 = decode[CmpAlg](str).toOption.get
  logger.info("AlgDef Json string decoded")

  logger.debug(fpp2.asJson.toString())
  logger.info("AlgDef Json string decoded")

  val watch: Stopwatch = new Stopwatch
  watch.start()
  logger.info(s"Starting repo build")

  val repository = fpp2.buildRepository
  watch.stop()
  logger.info(s"elapsed time ${watch.getTimeString}")

  val watch2: Stopwatch = new Stopwatch
  watch2.start()
  val result = fpp2.getIhResult(repository)
  watch2.stop()

  def getResultList[R](b: repository.InhabitationBatchJob) = {
    @scala.annotation.tailrec
    def getElements(l: List[InhabitationResult[Any]], bnew: b.ResultType): List[InhabitationResult[Any]] =
      bnew match {
        case (newJob: b.ResultType, result: InhabitationResult[Any]) => getElements(result +: l, newJob)
        case a: InhabitationResult[Any] => a +: l
      }

    getElements(List.empty, b.run())
  }

  val l = getResultList(fpp2.getIhResult(repository).asInstanceOf[repository.InhabitationBatchJob])
  l.foreach(i => logger.info((if (i.isEmpty) "inhabitant not found" else "inhabitant found") + "," + i.target.toString()))

  if (l.isEmpty || l.last.isEmpty) {
    logger.info("Inhabitation empty")
    None
  } else {
    logger.info("Running inhabitant.")
    // l.last.interpretedTerms.index(0).asInstanceOf[T => S]
    val r = l.last.interpretedTerms.index(0)
    logger.debug("repository built, inhab running")
    Some(r)
  }
}

