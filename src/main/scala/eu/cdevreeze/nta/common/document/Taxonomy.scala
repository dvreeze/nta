/*
 * Copyright 2011 Chris de Vreeze
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

package eu.cdevreeze.nta
package common
package document

import java.net.URI
import scala.collection.immutable
import eu.cdevreeze.yaidom.Document

/**
 * Taxonomy, using this term loosely as any collection of taxonomy schema documents, taxonomy linkbases, and maybe some other
 * taxonomy documents.
 *
 * @author Chris de Vreeze
 */
final class Taxonomy(
  val schemas: Map[URI, SchemaDocument],
  val linkbases: Map[URI, LinkbaseDocument],
  val otherDocuments: Map[URI, TaxonomyDocument]) extends Immutable {

  require(schemas ne null)
  require(linkbases ne null)
  require(otherDocuments ne null)

  require(
    taxonomyDocuments.size == schemas.size + linkbases.size + otherDocuments.size,
    "The sets of schemas, linkbases and other documents must be disjoint")
  require(
    taxonomyDocuments forall { case (uri, doc) => doc.localUri == uri },
    "All documents must be indexed by the local URI")

  def taxonomyDocuments: Map[URI, TaxonomyDocument] = schemas ++ linkbases ++ otherDocuments

  def ++(other: Taxonomy): Taxonomy = new Taxonomy(
    schemas = this.schemas ++ other.schemas,
    linkbases = this.linkbases ++ other.linkbases,
    otherDocuments = this.otherDocuments ++ other.otherDocuments)
}

object Taxonomy {

  def apply(docs: immutable.Seq[TaxonomyDocument]): Taxonomy = {
    val schemas = docs collect { case doc: SchemaDocument => (doc.localUri -> doc) }
    val linkbases = docs collect { case doc: LinkbaseDocument => (doc.localUri -> doc) }

    val keys = (schemas ++ linkbases).map(_._1).toSet
    val otherDocs = docs collect { case doc: TaxonomyDocument if !keys.contains(doc.localUri) => (doc.localUri -> doc) }

    new Taxonomy(
      schemas = schemas.toMap,
      linkbases = linkbases.toMap,
      otherDocuments = otherDocs.toMap)
  }
}
