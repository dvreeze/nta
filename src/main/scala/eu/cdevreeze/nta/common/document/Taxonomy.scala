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
import eu.cdevreeze.yaidom.{ Document, Elem, EName, QName }

/**
 * Taxonomy, using this term loosely as any collection of taxonomy schema documents, taxonomy linkbases, and maybe some other
 * taxonomy documents.
 *
 * @author Chris de Vreeze
 */
final class Taxonomy(
  val schemas: Map[URI, SchemaDocument],
  val linkbases: Map[URI, LinkbaseDocument],
  val otherDocuments: Map[URI, TaxonomyDocument],
  val addedSubstitutionGroups: Set[SubstitutionGroup]) extends Immutable {

  require(schemas ne null)
  require(linkbases ne null)
  require(otherDocuments ne null)
  require(addedSubstitutionGroups ne null)

  require(
    taxonomyDocuments.size == schemas.size + linkbases.size + otherDocuments.size,
    "The sets of schemas, linkbases and other documents must be disjoint")
  require(
    taxonomyDocuments forall { case (uri, doc) => doc.localUri == uri },
    "All documents must be indexed by the local URI")

  def taxonomyDocuments: Map[URI, TaxonomyDocument] = schemas ++ linkbases ++ otherDocuments

  def substitutionGroups: Set[SubstitutionGroup] = SubstitutionGroup.wellKnownSubstitutionGroups ++ addedSubstitutionGroups

  def ++(other: Taxonomy): Taxonomy = new Taxonomy(
    schemas = this.schemas ++ other.schemas,
    linkbases = this.linkbases ++ other.linkbases,
    otherDocuments = this.otherDocuments ++ other.otherDocuments,
    addedSubstitutionGroups = this.addedSubstitutionGroups ++ other.addedSubstitutionGroups)

  def withAddedSubstitutionGroups(newAddedSubstitutionGroups: Set[SubstitutionGroup]) = new Taxonomy(
    schemas = schemas.toMap,
    linkbases = linkbases.toMap,
    otherDocuments = otherDocuments.toMap,
    addedSubstitutionGroups = newAddedSubstitutionGroups)

  /**
   * Very expensive method to find all substitution group names used. Calling this method helps in identifying the
   * added substitution groups (by the taxonomy). Then an adapted copy of the taxonomy can be made, by calling method
   * `withAddedSubstitutionGroups`. The result would be a useful taxonomy that knows its items and tuples.
   */
  def findSubstitutionGroupNames: Set[EName] = {
    val rawSubstitutionGroupNamesPerSchema: Map[URI, Set[EName]] = schemas mapValues { doc =>
      val tns = doc.targetNamespaceOption.getOrElse(sys.error("Expected TNS. URI: %s".format(doc.localUri)))

      val elementDecls = doc.topLevelElementDeclarations

      val substitutionGroups = elementDecls flatMap { e =>
        val attrOption = e.attributeOption(EName("substitutionGroup"))

        if (attrOption.isEmpty) None
        else {
          val name = e.scope.resolveQName(QName(attrOption.get)).getOrElse(sys.error("Could not resolve %s".format(attrOption.get)))
          Some(name)
        }
      }
      substitutionGroups.toSet
    }

    val rawSubstitutionGroupNames: Set[EName] = rawSubstitutionGroupNamesPerSchema.values.toSet.flatten
    rawSubstitutionGroupNames
  }
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
      otherDocuments = otherDocs.toMap,
      addedSubstitutionGroups = Set())
  }
}
