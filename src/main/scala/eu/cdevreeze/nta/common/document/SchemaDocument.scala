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
import eu.cdevreeze.yaidom.{ Document, Elem, EName, QName, ElemPath }
import SchemaDocument._

/**
 * XML Schema document. XML Schema is not modeled in this class, but there are some helper methods to retrieve element declarations, etc.
 *
 * Note that an XML Schema can be comprised of several schema documents. Each instance of this class holds only one such document.
 *
 * TODO Consider introducing a class for element declarations, which keeps the "identity" (and TNS) of the Schema document, and the
 * ElemPath within that document. Also consider making a parent class for "schema data", which mixes in traits like
 * yaidom's ElemLike. Note that an element declaration typically needs the following context to be useful: URI of the containing
 * schema document, target namespace, and "path" from root to the element declaration.
 *
 * @author Chris de Vreeze
 */
final class SchemaDocument(
  override val originalUri: URI,
  override val localUri: URI,
  override val doc: Document) extends TaxonomyDocument {

  require(originalUri ne null)
  require(localUri ne null)
  require(doc ne null)

  require(originalUri.isAbsolute, "The original URI '%s' is not absolute".format(originalUri))
  require(localUri.isAbsolute, "The local URI '%s' is not absolute".format(localUri))
  require(originalUri.getFragment eq null, "The original URI '%s' has a fragment".format(originalUri))
  require(localUri.getFragment eq null, "The local URI '%s' has a fragment".format(localUri))

  require(doc.documentElement.resolvedName == EName(NS, "schema"))

  def targetNamespaceOption: Option[String] = doc.documentElement.attributeOption(EName("targetNamespace"))

  def imports: immutable.IndexedSeq[Elem] = {
    doc.documentElement filterChildElems { e => e.resolvedName == EName(NS, "import") }
  }

  def includes: immutable.IndexedSeq[Elem] = {
    doc.documentElement filterChildElems { e => e.resolvedName == EName(NS, "include") }
  }

  def topLevelElementDeclarations: immutable.IndexedSeq[Elem] = {
    doc.documentElement filterChildElems { e => e.resolvedName == EName(NS, "element") }
  }

  /**
   * Returns the likely concept declarations, which are the (top level) element declarations that
   * have a name, substitutionGroup and type. To qualify as concept declarations, the substitutionGroup
   * must be xbrli:item or xbrli:tuple, or have these substitution groups in their ancestry.
   *
   * Only top-level element declarations are returned, as per NTA rule 2.2.2.01.
   */
  def probableConceptDeclarations: immutable.IndexedSeq[Elem] = {
    topLevelElementDeclarations filter { e =>
      e.attributeOption(EName("name")).isDefined &&
        e.attributeOption(EName("substitutionGroup")).isDefined &&
        e.attributeOption(EName("type")).isDefined
    }
  }

  /**
   * Returns the likely concept declarations that are not themselves substitution group declarations in the given
   * parameter set of substitution groups.
   */
  def conceptDeclarations(substitutionGroups: Set[SubstitutionGroup]): immutable.IndexedSeq[Elem] = {
    require(SubstitutionGroup.wellKnownSubstitutionGroups.subsetOf(substitutionGroups),
      "Some missing well-known substitution groups")

    val substitutionGroupNames = substitutionGroups map { _.name }

    val likelyConceptDecls = probableConceptDeclarations

    likelyConceptDecls filter { e =>
      val ename = EName(targetNamespaceOption, e.attribute(EName("name")))
      !substitutionGroupNames.contains(ename)
    }
  }

  def elementDeclarationPaths: immutable.IndexedSeq[ElemPath] = {
    doc.documentElement filterElemPaths { e => e.resolvedName == EName(NS, "element") }
  }

  def elementDeclarationsWithPaths: immutable.IndexedSeq[(ElemPath, Elem)] = {
    val paths = elementDeclarationPaths
    val result = paths map { p => (p -> doc.documentElement.getWithElemPath(p)) }
    result
  }

  def uriOption(elm: Elem): Option[URI] = {
    val idOption = elm.attributeOption(EName("id"))
    idOption map { id => new URI(localUri.getScheme, localUri.getSchemeSpecificPart, id) }
  }

  def topLevelElementDeclarationsByUris: Map[URI, Elem] = {
    val result = topLevelElementDeclarations flatMap { e =>
      uriOption(e) map { uri => (uri -> e) }
    }
    result.toMap
  }

  def probableConceptDeclarationsByUris: Map[URI, Elem] = {
    val result = probableConceptDeclarations flatMap { e =>
      uriOption(e) map { uri => (uri -> e) }
    }
    result.toMap
  }

  def conceptDeclarationsByUris(substitutionGroups: Set[SubstitutionGroup]): Map[URI, Elem] = {
    val result = conceptDeclarations(substitutionGroups) flatMap { e =>
      uriOption(e) map { uri => (uri -> e) }
    }
    result.toMap
  }
}

object SchemaDocument {

  val NS = "http://www.w3.org/2001/XMLSchema"
}
