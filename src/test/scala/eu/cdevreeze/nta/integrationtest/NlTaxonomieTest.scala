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

/**
 * Test case checking some properties of the NL Taxonomie (XBRL 6.0). By doing this exercise, some classes for taxonomy
 * documents are to some extent tested, we learn something about the NL taxonomie (6.0), and we develop some feel for
 * what higher-level abstractions may or may not be needed when processing XBRL taxonomies.
 *
 * @author Chris de Vreeze
 */
@RunWith(classOf[JUnitRunner])
class NlTaxonomieTest extends FunSuite with BeforeAndAfterAll {

  private val logger: jutil.logging.Logger = jutil.logging.Logger.getLogger("eu.cdevreeze.nta.integrationtest")

  private val docParser = parse.DocumentParserUsingDom.newInstance

  @volatile private var taxonomyDocs: Map[URI, TaxonomyDocument] = _
  @volatile private var schemaDocs: Map[URI, SchemaDocument] = _
  @volatile private var linkbaseDocs: Map[URI, LinkbaseDocument] = _

  override def beforeAll(): Unit = {
    val rootDir = new jio.File(classOf[NlTaxonomieTest].getResource("/extracted-taxonomies/www.nltaxonomie.nl/6.0").toURI)
    require(
      rootDir.isDirectory && rootDir.exists,
      "Expected root directory %s. Extract file /compressed-taxonomies/www.nltaxonomie.nl-6.0.zip into this directory".format(rootDir.getPath))

    val files = findAllFiles(rootDir)
    logger.info("Found %d taxonomy files".format(files.size))

    val docs: Map[URI, TaxonomyDocument] = {
      val result = files.par flatMap { f: jio.File =>
        try {
          val doc = docParser.parse(new jio.FileInputStream(f))

          val localUri = f.toURI
          val originalUri = {
            val localUriString = localUri.toString
            val idx = localUriString.indexOf("/www.nltaxonomie.nl/6.0")
            require(idx > 0, "Expected '6.0' URI, but found '%s'".format(localUriString))
            new URI("http://" + (localUriString.drop(idx + 1)))
          }

          if (doc.documentElement.resolvedName.namespaceUriOption == Some(SchemaDocument.NS))
            Some(localUri -> new SchemaDocument(originalUri, localUri, doc))
          else if (doc.documentElement.resolvedName.namespaceUriOption == Some(LinkbaseDocument.NS))
            Some(localUri -> new LinkbaseDocument(originalUri, localUri, doc))
          else None
        } catch {
          case e: Exception =>
            logger.warning(e.toString)
            None
        }
      }
      result.seq.toMap
    }
    logger.info("Found %d taxonomy documents".format(docs.size))

    require(!docs.isEmpty, "Expected at least one taxonomy file")
    taxonomyDocs = docs

    // Mind the pattern matching on types inside the pair
    schemaDocs = docs collect { case (uri: URI, d: SchemaDocument) => (uri -> d) }
    logger.info("Found %d schema documents".format(schemaDocs.size))

    // Mind the pattern matching on types inside the pair
    linkbaseDocs = docs collect { case (uri: URI, d: LinkbaseDocument) => (uri -> d) }
    logger.info("Found %d linkbase documents".format(linkbaseDocs.size))
  }

  override def afterAll(): Unit = {
  }

  test("Schemas must have a targetNamespace") {
    val offendingSchemas = schemaDocs filter { case (uri, doc) => doc.targetNamespaceOption.isEmpty }

    expect(Set()) {
      offendingSchemas.keySet
    }
  }

  test("Schema element definitions (top level) always have IDs") {
    val offendingElemDefs: Map[URI, Seq[Elem]] = schemaDocs flatMap {
      case (uri, doc) =>
        val topLevelElmDefsWithoutId = doc.topLevelElementDeclarations filter { e => e.attributeOption(EName("id")).isEmpty }
        if (topLevelElmDefsWithoutId.isEmpty) None else Some(uri -> topLevelElmDefsWithoutId)
    }

    // Alas, there is one schema file in which a top level element definition has no id attribute...
    val expectedOffendingSchemaUris = schemaDocs.keySet filter { _.getPath.endsWith("xbrl-syntax-extension.xsd") }
    expect(1) {
      expectedOffendingSchemaUris.size
    }

    // Mind the filtering using a Set as Boolean function!
    val expectedOffendingSchema = schemaDocs.filterKeys(expectedOffendingSchemaUris).values.head
    assert(expectedOffendingSchema.topLevelElementDeclarations exists (e => e.attributeOption(EName("id")).isEmpty))

    expect(expectedOffendingSchemaUris) {
      offendingElemDefs.keySet
    }
  }

  test("Nested schema element definitions have no IDs") {
    val nestedElemDefs: Map[URI, Seq[Elem]] = schemaDocs mapValues { doc =>
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
    val linkbaseChildElmNames: Map[URI, Set[EName]] = linkbaseDocs mapValues { doc =>
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
    val offendingResourcesAndLocators: Map[URI, immutable.IndexedSeq[xlink.XLink]] = linkbaseDocs mapValues { doc =>
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

  private def findAllFiles(dir: jio.File): immutable.Seq[jio.File] = {
    require(dir.isDirectory, "Not a directory: %s".format(dir.getPath))

    dir.listFiles.toIndexedSeq flatMap { f: jio.File =>
      if (f.isFile) Vector(f)
      else if (f.isDirectory) findAllFiles(f)
      else Vector()
    }
  }
}
