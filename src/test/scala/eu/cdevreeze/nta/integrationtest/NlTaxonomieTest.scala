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
package integrationtest

import java.{ util => jutil, io => jio }
import java.net.URI
import scala.collection.immutable
import org.junit.{ Test, Before, Ignore }
import org.junit.runner.RunWith
import org.scalatest.{ FunSuite, BeforeAndAfterAll }
import org.scalatest.junit.JUnitRunner
import eu.cdevreeze.yaidom._
import eu.cdevreeze.yaidom.xlink._
import common.document._
import common.parse.TaxonomyParser

/**
 * Test case checking some properties of the NL Taxonomie (XBRL 6.0). By doing this exercise, some classes for taxonomy
 * documents are to some extent tested, we learn something about the NL taxonomie (6.0), and we develop some feel for
 * what higher-level abstractions may or may not be needed when processing XBRL taxonomies.
 *
 * @author Chris de Vreeze
 */
@RunWith(classOf[JUnitRunner])
class NlTaxonomieTest extends FunSuite with BeforeAndAfterAll with TaxonomyParser {

  @volatile private var taxonomy: Taxonomy = _

  override def beforeAll(): Unit = {
    val rootDir = new jio.File(classOf[NlTaxonomieTest].getResource("/extracted-taxonomies/www.nltaxonomie.nl/6.0").toURI)
    require(
      rootDir.isDirectory && rootDir.exists,
      "Expected root directory %s. Extract file /compressed-taxonomies/www.nltaxonomie.nl-6.0.zip into this directory".format(rootDir.getPath))

    def localUriToOriginalUri(localUri: URI): URI = {
      val localUriString = localUri.toString
      val idx = localUriString.indexOf("/www.nltaxonomie.nl/6.0")
      require(idx > 0, "Expected '6.0' URI, but found '%s'".format(localUriString))
      new URI("http://" + (localUriString.drop(idx + 1)))
    }

    taxonomy = parse(rootDir)(localUriToOriginalUri)

    logger.info("Found %d schema documents".format(taxonomy.schemas.size))

    logger.info("Found %d linkbase documents".format(taxonomy.linkbases.size))
  }

  override def afterAll(): Unit = {
  }

  test("Schemas must have a targetNamespace") {
    val offendingSchemas = taxonomy.schemas filter { case (uri, doc) => doc.targetNamespaceOption.isEmpty }

    expect(Set()) {
      offendingSchemas.keySet
    }
  }

  test("Schema element definitions (top level) always have IDs") {
    val offendingElemDefs: Map[URI, Seq[Elem]] = taxonomy.schemas flatMap {
      case (uri, doc) =>
        val topLevelElmDefsWithoutId = doc.topLevelElementDeclarations filter { e => e.attributeOption(EName("id")).isEmpty }
        if (topLevelElmDefsWithoutId.isEmpty) None else Some(uri -> topLevelElmDefsWithoutId)
    }

    // Alas, there is one schema file in which a top level element definition has no id attribute...
    val expectedOffendingSchemaUris = taxonomy.schemas.keySet filter { _.getPath.endsWith("xbrl-syntax-extension.xsd") }
    expect(1) {
      expectedOffendingSchemaUris.size
    }

    // Mind the filtering using a Set as Boolean function!
    val expectedOffendingSchema = taxonomy.schemas.filterKeys(expectedOffendingSchemaUris).values.head
    assert(expectedOffendingSchema.topLevelElementDeclarations exists (e => e.attributeOption(EName("id")).isEmpty))

    expect(expectedOffendingSchemaUris) {
      offendingElemDefs.keySet
    }
  }

  test("Nested schema element definitions have no IDs") {
    val nestedElemDefs: Map[URI, Seq[Elem]] = taxonomy.schemas mapValues { doc =>
      val nestedElmDefsWithPaths = doc.elementDeclarationsWithPaths filter { case (p, e) => p.entries.count(_.elementName.localPart == "element") >= 2 }
      val nestedElmDefs: immutable.IndexedSeq[Elem] = nestedElmDefsWithPaths map { _._2 }
      nestedElmDefs
    } filter { case (uri, elms) => !elms.isEmpty }

    logger.info("Found %d schemas with nested element declarations".format(nestedElemDefs.size))
    assert(nestedElemDefs.size >= 1)

    val offendingElemDefs: Map[URI, Seq[Elem]] = nestedElemDefs mapValues { elms: Seq[Elem] =>
      val offendingNestedElmDefs = elms filter { e => e.attributeOption(EName("id")).isDefined }
      offendingNestedElmDefs
    } filter { case (uri, elms) => !elms.isEmpty }

    expect(Set()) {
      offendingElemDefs.keySet
    }
  }

  test("Linkbases are all of a known linkbase type (looking at child elements)") {
    val linkbaseChildElmNames: Map[URI, Set[EName]] = taxonomy.linkbases mapValues { doc =>
      val childElmNames = doc.doc.documentElement.allChildElems map { e => e.resolvedName }
      childElmNames.toSet
    }

    val expectedChildElmNames: Set[EName] = {
      val ns = LinkbaseDocument.NS
      val nsGen = "http://xbrl.org/2008/generic"

      Set(
        EName(ns, "roleRef"), EName(ns, "arcroleRef"), EName(nsGen, "link"),
        EName(ns, "presentationLink"), EName(ns, "labelLink"), EName(ns, "referenceLink"), EName(ns, "definitionLink"))
    }

    val offendingENames: Map[URI, Set[EName]] = linkbaseChildElmNames mapValues { enames: Set[EName] =>
      enames diff expectedChildElmNames
    } filter { case (uri, enames) => !enames.isEmpty }

    expect(Set()) {
      offendingENames.keySet
    }
  }

  test("All locators and resources in linkbases have a (non-empty) label") {
    val offendingResourcesAndLocators: Map[URI, immutable.IndexedSeq[xlink.XLink]] = taxonomy.linkbases mapValues { doc =>
      val allXLinkElms = doc.doc.documentElement filterElems { e => XLink.mustBeXLink(e) }
      val resources = allXLinkElms collect { case e if XLink.mustBeResource(e) => Resource(e) }
      val locators = allXLinkElms collect { case e if XLink.mustBeLocator(e) => Locator(e) }

      val offendingResources = resources filter { res => res.labelOption.getOrElse("").isEmpty }
      val offendingLocators = locators filter { loc => loc.labelOption.getOrElse("").isEmpty }

      val offendingXLinks: immutable.IndexedSeq[XLink] = offendingResources ++ offendingLocators
      offendingXLinks
    } filter { case (uri, xlinks) => !xlinks.isEmpty }

    expect(Set()) {
      offendingResourcesAndLocators.keySet
    }
  }

  test("Only top level element declarations have substitution groups") {
    val offendingElmDecls: Map[URI, immutable.Seq[Elem]] = taxonomy.schemas mapValues { doc =>
      val nestedElmDecls = doc.elementDeclarationsWithPaths filter {
        case (p, e) =>
          p.entries.map(_.elementName).count(_ == EName(SchemaDocument.NS, "element")) >= 2
      } map { _._2 }

      nestedElmDecls filter { e => e.attributeOption(EName("substitutionGroup")).isDefined }
    } filter { case (uri, elms) => !elms.isEmpty }

    expect(Map()) {
      offendingElmDecls
    }
  }

  test("Nested element declarations all have a ref attribute") {
    val offendingElmDecls: Map[URI, immutable.Seq[Elem]] = taxonomy.schemas mapValues { doc =>
      val nestedElmDecls = doc.elementDeclarationsWithPaths filter {
        case (p, e) =>
          p.entries.map(_.elementName).count(_ == EName(SchemaDocument.NS, "element")) >= 2
      } map { _._2 }

      nestedElmDecls filter { e => e.attributeOption(EName("ref")).isEmpty }
    } filter { case (uri, elms) => !elms.isEmpty }

    expect(Map()) {
      offendingElmDecls
    }
  }

  test("Only known substitution group names") {
    val substitutionGroups: Map[URI, Set[EName]] = taxonomy.schemas mapValues { doc =>
      val elementDecls = doc.elementDeclarationsWithPaths.map(_._2)
      val substitutionGroups = elementDecls flatMap { e =>
        val attrOption = e.attributeOption(EName("substitutionGroup"))
        attrOption flatMap { a => e.scope.resolveQNameOption(QName(a)) }
      }
      substitutionGroups.toSet
    }

    val expectedGroups: Set[EName] = Set(
      EName("{http://www.xbrl.org/2003/instance}item"),
      EName("{http://www.xbrl.org/2003/instance}tuple"),
      EName("{http://xbrl.org/2005/xbrldt}hypercubeItem"),
      EName("{http://xbrl.org/2005/xbrldt}dimensionItem"),
      EName("{http://www.nltaxonomie.nl/6.0/basis/sbr/xbrl/xbrl-syntax-extension}domainItem"),
      EName("{http://www.nltaxonomie.nl/6.0/basis/sbr/xbrl/xbrl-syntax-extension}domainMemberItem"),
      EName("{http://www.nltaxonomie.nl/6.0/basis/sbr/xbrl/xbrl-syntax-extension}presentationItem"),
      EName("{http://www.nltaxonomie.nl/6.0/basis/sbr/xbrl/xbrl-syntax-extension}presentationTuple"),
      EName("{http://www.nltaxonomie.nl/6.0/basis/sbr/xbrl/xbrl-syntax-extension}specificationTuple"),
      EName("{http://www.nltaxonomie.nl/6.0/basis/sbr/xbrl/xbrl-syntax-extension}primaryDomainItem"),
      EName("{http://www.xbrl.org/2003/XLink}resource"))

    val unexpectedSubstitutionGroups: Map[URI, Set[EName]] = substitutionGroups mapValues { (groups: Set[EName]) =>
      groups diff expectedGroups
    } filter { case (uri, groups) => !groups.isEmpty }

    expect(Set()) {
      unexpectedSubstitutionGroups.keySet
    }
  }

  test("Only known substitution group names found by taxonomy itself") {
    val substitutionGroups: Set[EName] = taxonomy.findSubstitutionGroupNames

    val expectedGroupNames: Set[EName] = Set(
      EName("{http://www.xbrl.org/2003/instance}item"),
      EName("{http://www.xbrl.org/2003/instance}tuple"),
      EName("{http://xbrl.org/2005/xbrldt}hypercubeItem"),
      EName("{http://xbrl.org/2005/xbrldt}dimensionItem"),
      EName("{http://www.nltaxonomie.nl/6.0/basis/sbr/xbrl/xbrl-syntax-extension}domainItem"),
      EName("{http://www.nltaxonomie.nl/6.0/basis/sbr/xbrl/xbrl-syntax-extension}domainMemberItem"),
      EName("{http://www.nltaxonomie.nl/6.0/basis/sbr/xbrl/xbrl-syntax-extension}presentationItem"),
      EName("{http://www.nltaxonomie.nl/6.0/basis/sbr/xbrl/xbrl-syntax-extension}presentationTuple"),
      EName("{http://www.nltaxonomie.nl/6.0/basis/sbr/xbrl/xbrl-syntax-extension}specificationTuple"),
      EName("{http://www.nltaxonomie.nl/6.0/basis/sbr/xbrl/xbrl-syntax-extension}primaryDomainItem"),
      EName("{http://www.xbrl.org/2003/XLink}resource"))

    expect(expectedGroupNames) {
      substitutionGroups
    }
  }

  test("Only known substitution groups found by taxonomy itself") {
    val substitutionGroups: Set[SubstitutionGroup] = taxonomy.findSubstitutionGroups

    // This time not finding {http://www.xbrl.org/2003/XLink}resource
    val expectedGroupNames: Set[EName] = Set(
      EName("{http://www.xbrl.org/2003/instance}item"),
      EName("{http://www.xbrl.org/2003/instance}tuple"),
      EName("{http://xbrl.org/2005/xbrldt}hypercubeItem"),
      EName("{http://xbrl.org/2005/xbrldt}dimensionItem"),
      EName("{http://www.nltaxonomie.nl/6.0/basis/sbr/xbrl/xbrl-syntax-extension}domainItem"),
      EName("{http://www.nltaxonomie.nl/6.0/basis/sbr/xbrl/xbrl-syntax-extension}domainMemberItem"),
      EName("{http://www.nltaxonomie.nl/6.0/basis/sbr/xbrl/xbrl-syntax-extension}presentationItem"),
      EName("{http://www.nltaxonomie.nl/6.0/basis/sbr/xbrl/xbrl-syntax-extension}presentationTuple"),
      EName("{http://www.nltaxonomie.nl/6.0/basis/sbr/xbrl/xbrl-syntax-extension}specificationTuple"),
      EName("{http://www.nltaxonomie.nl/6.0/basis/sbr/xbrl/xbrl-syntax-extension}primaryDomainItem"))

    expect(expectedGroupNames) {
      substitutionGroups.map(_.name)
    }

    val domainItemSubstGroup = {
      val result = substitutionGroups find { grp => grp.name == EName("{http://www.nltaxonomie.nl/6.0/basis/sbr/xbrl/xbrl-syntax-extension}domainItem") }
      result.get
    }

    expect(Some(SubstitutionGroup.Item)) {
      domainItemSubstGroup.parentGroupOption
    }

    expect(None) {
      SubstitutionGroup.Item.parentGroupOption
    }

    val specificationTupleSubstGroup = {
      val result = substitutionGroups find { grp => grp.name == EName("{http://www.nltaxonomie.nl/6.0/basis/sbr/xbrl/xbrl-syntax-extension}specificationTuple") }
      result.get
    }

    expect(Some(SubstitutionGroup.Tuple)) {
      specificationTupleSubstGroup.parentGroupOption
    }

    expect(None) {
      SubstitutionGroup.Tuple.parentGroupOption
    }
  }

  test("Syntax extensions are for tuples and items only") {
    val extensionDocOption: Option[SchemaDocument] = {
      val result = taxonomy.schemas find { case (uri, doc) => uri.toString.endsWith("xbrl-syntax-extension.xsd") }
      result map { _._2 }
    }

    assert(extensionDocOption.isDefined)

    val extensionDoc: SchemaDocument = extensionDocOption.get

    val ns = "http://www.nltaxonomie.nl/6.0/basis/sbr/xbrl/xbrl-syntax-extension"

    expect(Some(ns)) {
      extensionDoc.targetNamespaceOption
    }

    def substitutionGroupOption(e: Elem): Option[EName] = {
      val attrValue = e.attributeOption(EName("substitutionGroup"))
      attrValue flatMap { a => e.scope.resolveQNameOption(QName(a)) }
    }

    val searchedItemOrTupleSubstitutionGroupDecls = extensionDoc.topLevelElementDeclarations filter { e =>
      val isItemOrTupleDecl =
        substitutionGroupOption(e) == Some(EName("{http://www.xbrl.org/2003/instance}item")) ||
          substitutionGroupOption(e) == Some(EName("{http://www.xbrl.org/2003/instance}tuple"))

      val isAbstract = e.attributeOption(EName("abstract")) == Some("true")

      isItemOrTupleDecl && isAbstract
    }

    val definedItemOrTupleSubstitutionGroups = {
      val result = searchedItemOrTupleSubstitutionGroupDecls map { e => EName(ns, e.attribute(EName("name"))) }
      result.toSet
    }

    val expectedSubstitutionGroups = Set(
      EName(ns, "domainItem"),
      EName(ns, "domainMemberItem"),
      EName(ns, "presentationItem"),
      EName(ns, "presentationTuple"),
      EName(ns, "specificationTuple"),
      EName(ns, "primaryDomainItem"))

    expect(expectedSubstitutionGroups) {
      definedItemOrTupleSubstitutionGroups
    }
  }
}
