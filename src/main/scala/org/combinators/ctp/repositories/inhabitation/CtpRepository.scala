package org.combinators.ctp.repositories.inhabitation

import org.combinators.cls.types.{Kinding, Taxonomy}

trait CtpRepository {
  val taxList: Option[Seq[Taxonomy]] = None
  val kindingList: Option[Seq[Kinding]] = None

  val semanticTaxonomy = taxList.getOrElse(Seq.empty[Taxonomy]).foldLeft(Taxonomy.empty)((a, b) => a.merge(b))
  val kinding = kindingList.getOrElse(Seq.empty[Kinding]).foldLeft(Kinding.empty)((a, b) => a.merge(b))
}