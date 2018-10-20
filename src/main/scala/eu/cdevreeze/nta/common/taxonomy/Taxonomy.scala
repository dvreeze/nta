/*
 * Copyright 2011-2018 Chris de Vreeze
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package eu.cdevreeze.nta.common.taxonomy

import java.net.URI

import eu.cdevreeze.tqa.base.dom.TaxonomyDocument
import eu.cdevreeze.tqa.base.taxonomy.BasicTaxonomy
import eu.cdevreeze.tqa.base.taxonomybuilder.DocumentCollector
import eu.cdevreeze.tqa.docbuilder.DocumentBuilder
import eu.cdevreeze.yaidom.queryapi.BackingDocumentApi

/**
 * A taxonomy, holding a collection of DTSes (fitting entirely in that taxonomy).
 *
 * It does not know anything about the part of the taxonomy that must be validated by NTA validators.
 *
 * Note that memory usage may be quite large, so by all means use DOM-like trees that use Saxon tiny trees or
 * some other thread-safe low footprint DOM-like element implementation.
 *
 * @author Chris de Vreeze
 */
final class Taxonomy private (
  val universeTaxonomy: BasicTaxonomy,
  val dtsMap: Map[Set[URI], BasicTaxonomy]) {

  assert(dtsMap.forall { case (ep, taxo) => ep.subsetOf(taxo.taxonomyBase.taxonomyDocUriMap.keySet) })

  assert(dtsMap.values.forall(taxo =>
    taxo.taxonomyBase.taxonomyDocUriMap.keySet.subsetOf(universeTaxonomy.taxonomyBase.taxonomyDocUriMap.keySet)))

  def findDts(entrypoint: Set[URI]): Option[BasicTaxonomy] = {
    dtsMap.get(entrypoint)
  }

  def getDts(entrypoint: Set[URI]): BasicTaxonomy = {
    findDts(entrypoint).getOrElse(sys.error(s"No DTS found for entrypoint ${entrypoint.mkString(", ")}"))
  }

  def findDocument(uri: URI): Option[TaxonomyDocument] = {
    universeTaxonomy.taxonomyBase.taxonomyDocUriMap.get(uri)
  }

  def getDocument(uri: URI): TaxonomyDocument = {
    findDocument(uri).getOrElse(sys.error(s"No document found with URI '$uri'"))
  }
}

object Taxonomy {

  def build(
    universeTaxonomy: BasicTaxonomy,
    dtsDocumentCollector: DocumentCollector,
    entrypoints: Set[Set[URI]]): Taxonomy = {

    val docBuilder = new DocBuilder(universeTaxonomy)

    val dtsMap: Map[Set[URI], BasicTaxonomy] =
      entrypoints.toSeq.map { entrypointUris =>
        val dtsUris: Set[URI] =
          dtsDocumentCollector.collectTaxonomyDocuments(entrypointUris, docBuilder).map(_.uri).toSet

        val dts: BasicTaxonomy = universeTaxonomy.filteringDocumentUris(dtsUris)

        entrypointUris -> dts
      }.toMap

    new Taxonomy(universeTaxonomy, dtsMap)
  }

  /**
   * Very fast DocumentBuilder that only looks up documents in the passed "universe taxonomy", instead of parsing.
   */
  final class DocBuilder(val universeTaxonomy: BasicTaxonomy) extends DocumentBuilder {

    type BackingDoc = BackingDocumentApi

    def build(uri: URI): BackingDocumentApi = {
      universeTaxonomy.taxonomyBase.taxonomyDocUriMap
        .getOrElse(uri, sys.error(s"Missing document with URI '$uri'"))
        .backingDocument
    }
  }
}
