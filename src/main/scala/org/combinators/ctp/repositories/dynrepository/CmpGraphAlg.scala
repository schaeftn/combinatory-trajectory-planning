package org.combinators.ctp.repositories.dynrepository

object CmpGraphAlg extends Enumeration {
  type EnumType = Value
  val cmp_graph_dijkstra_type,
  cmp_graph_a_star_type,
  cmp_graph_mst_type,
  cmp_graph_tsp_type = Value
}
