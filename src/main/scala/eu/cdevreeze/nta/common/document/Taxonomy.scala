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
 * If the taxonomy is to be useful in that it knows its item and tuple concepts, it is important that the passed
 * "added substitution groups" (added means: not already a "well-known" substitution group, such as xbrli:item, xbrli:tuple)
 * are complete. One way to achieve that is calling method `withFoundSubstitutionGroups`, and using the resulting Taxonomy
 * instead of this one.
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

  /**
   * Returns a copy of this Taxonomy in which the substitution groups are the ones passed as parameter
   * (besides the well-known ones, of course).
   */
  def withAddedSubstitutionGroups(newAddedSubstitutionGroups: Set[SubstitutionGroup]) = new Taxonomy(
    schemas = schemas.toMap,
    linkbases = linkbases.toMap,
    otherDocuments = otherDocuments.toMap,
    addedSubstitutionGroups = newAddedSubstitutionGroups)

  /**
   * Returns a copy of this Taxonomy in which the substitution groups are the ones found in this taxonomy
   * (of course including the well-known ones). Calling this method makes the result Taxonomy useful, because
   * it knows its item and tuple concepts. Note that this is a very expensive method, not to be called repeatedly.
   */
  def withFoundSubstitutionGroups: Taxonomy = withAddedSubstitutionGroups(findSubstitutionGroups)

  /**
   * Very expensive method to find all substitution group names used. Calling this method helps in identifying the
   * added substitution groups (by the taxonomy). Then an adapted copy of the taxonomy can be made, by calling method
   * `withAddedSubstitutionGroups`. The result would be a useful taxonomy that knows its items and tuples.
   */
  def findSubstitutionGroupNames: Set[EName] = {
    val substitutionGroupNamesPerSchema: Map[URI, Set[EName]] = schemas mapValues { doc =>
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

    val substitutionGroupNames: Set[EName] = substitutionGroupNamesPerSchema.values.toSet.flatten
    substitutionGroupNames
  }

  /**
   * Very expensive method to find all substitution groups used. Calling this method helps in identifying the
   * added substitution groups (by the taxonomy). Then an adapted copy of the taxonomy can be made, by calling method
   * `withAddedSubstitutionGroups`. The result would be a useful taxonomy that knows its items and tuples.
   */
  def findSubstitutionGroups: Set[SubstitutionGroup] = {
    val substGroupElmDecls = findSubstitutionGroupElementDeclarations
    findSubstitutionGroups(substGroupElmDecls, SubstitutionGroup.wellKnownSubstitutionGroups)
  }

  /**
   * Very expensive method to find all element declarations of substitution groups used. Used by method
   * `findSubstitutionGroups`, but possibly sometimes also useful in application code.
   */
  def findSubstitutionGroupElementDeclarations: Map[URI, immutable.Seq[Elem]] = {
    val substGroupNames: Set[EName] = findSubstitutionGroupNames

    val substGroupElmDecls: Map[URI, immutable.Seq[Elem]] = schemas mapValues { doc: SchemaDocument =>
      val tns = doc.targetNamespaceOption.getOrElse(sys.error("Expected TNS. URI: %s".format(doc.localUri)))

      doc.topLevelElementDeclarations filter { e =>
        val localName = e.attribute(EName("name"))
        val ename = EName(tns, localName)

        substGroupNames.contains(ename)
      }
    } filter { case (uri, elms) => !elms.isEmpty }

    substGroupElmDecls
  }

  private def findSubstitutionGroups(
    substGroupElmDecls: Map[URI, immutable.Seq[Elem]],
    foundGroups: Set[SubstitutionGroup]): Set[SubstitutionGroup] = {

    val matchingSubstGroupElmDecls = substGroupElmDecls map {
      case (uri, elms) =>
        val doc = schemas(uri)
        val tns = doc.targetNamespaceOption.getOrElse(sys.error("Expected TNS. URI: %s".format(doc.localUri)))

        val resultElmDecls = elms filter { e =>
          val substGroup = e.attribute(EName("substitutionGroup"))
          val substGroupEName = e.scope.resolveQName(QName(substGroup)).getOrElse(sys.error("Could not resolve %s".format(substGroup)))
          foundGroups.map(_.name).contains(substGroupEName)
        }
        (uri -> resultElmDecls)
    }

    if (matchingSubstGroupElmDecls.values.flatten.isEmpty) foundGroups
    else {
      val newGroups: Set[SubstitutionGroup] = {
        val result = matchingSubstGroupElmDecls.toSeq flatMap {
          case (uri, elms) =>
            val doc = schemas(uri)
            val tns = doc.targetNamespaceOption.getOrElse(sys.error("Expected TNS. URI: %s".format(doc.localUri)))

            val result = elms map { e =>
              val parentSubstGroupQName = QName(e.attribute(EName("substitutionGroup")))
              val parentSubstGroupEName = e.scope.resolveQName(parentSubstGroupQName).getOrElse(sys.error("Could not resolve %s".format(parentSubstGroupQName)))
              val parentSubstGroupOption = foundGroups.find(_.name == parentSubstGroupEName)
              assert(parentSubstGroupOption.isDefined)
              val parentSubstGroup = parentSubstGroupOption.get

              val ename = EName(tns, e.attribute(EName("name")))

              ename :: parentSubstGroup
            }
            result.toSet
        }
        result.toSet
      }

      val remainingSubstGroupElmDecls: Map[URI, immutable.Seq[Elem]] = substGroupElmDecls map {
        case (uri, elms) =>
          val filteredElms = elms filter { e => !matchingSubstGroupElmDecls(uri).contains(e) }
          (uri -> filteredElms)
      }

      // Recursive call
      findSubstitutionGroups(remainingSubstGroupElmDecls, foundGroups ++ newGroups)
    }
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
