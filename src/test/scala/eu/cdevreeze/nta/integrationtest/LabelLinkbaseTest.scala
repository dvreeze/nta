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
 * Test case checking label linkbase support against the NL Taxonomie (XBRL 6.0).
 *
 * @author Chris de Vreeze
 */
@RunWith(classOf[JUnitRunner])
class LabelLinkbaseTest extends FunSuite with BeforeAndAfterAll with TaxonomyParser {

  @volatile private var taxonomy: Taxonomy = _

  override def beforeAll(): Unit = {
    val rootDir = new jio.File(classOf[LabelLinkbaseTest].getResource("/extracted-taxonomies/www.nltaxonomie.nl/6.0").toURI)
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

  test("Concept labels are found") {
    val schema: SchemaDocument = {
      val schemaMap = taxonomy.schemas filterKeys { uri => uri.toString.endsWith("/bd-domeinleden.xsd") }
      require(schemaMap.size == 1)
      schemaMap.head._2
    }

    val labelLinkbase: LabelLinkbaseDocument = {
      val linkbaseMap = taxonomy.linkbases filterKeys { uri => uri.toString.endsWith("/bd-domeinleden-lab-nl.xml") }
      require(linkbaseMap.size == 1)
      linkbaseMap.head._2.asInstanceOf[LabelLinkbaseDocument]
    }

    val conceptUris: Map[URI, Elem] = schema.topLevelElementDeclarationsByUris

    expect(1) {
      labelLinkbase.extendedLinks.size
    }

    val extendedLink = labelLinkbase.extendedLinks.head

    val labelUris: Map[URI, xlink.Resource] = labelLinkbase.standardConceptLabelsByConceptUris(extendedLink)

    assert(!conceptUris.keySet.intersect(labelUris.keySet).isEmpty)

    val matchingConceptUris: Map[URI, Elem] = conceptUris.filterKeys(labelUris.keySet)
    val matchingLabelUris: Map[URI, xlink.Resource] = labelUris.filterKeys(conceptUris.keySet)

    // Check label 'Bedrijf' for concept 'Company'

    val companyConceptUris: Set[URI] = matchingConceptUris.keySet filter { uri => uri.getFragment == "bd-dom-tr_Company" }

    expect(1) {
      companyConceptUris.size
    }

    val companyConceptUri = companyConceptUris.iterator.next

    val companyElemDeclOption: Option[Elem] = {
      val result = matchingConceptUris.filterKeys(companyConceptUris) map { _._2 }
      result.headOption
    }

    expect(Some("Company")) {
      companyElemDeclOption flatMap { _.attributeOption(EName("name")) }
    }

    val companyLabelOption: Option[xlink.Resource] = {
      val resources = matchingLabelUris.filterKeys(companyConceptUris) map { _._2 }
      resources.headOption
    }
    // Yes, attributes in the "xml" namespace are picked up nicely by yaidom
    val companyLabelLangOption = companyLabelOption flatMap { _.wrappedElem \@ "lang" }
    val companyLabelTextOption = companyLabelOption map { _.wrappedElem.text }

    expect(Some("nl")) {
      companyLabelLangOption
    }

    expect(Some("nl")) {
      companyLabelOption flatMap { _.wrappedElem \@ EName("http://www.w3.org/XML/1998/namespace", "lang") }
    }

    expect(Some("Bedrijf")) {
      companyLabelTextOption
    }

    // Check label 'Huidig jaar' for concept 'Current'

    val currentConceptUris: Set[URI] = matchingConceptUris.keySet filter { uri => uri.getFragment == "bd-dom-tr_Current" }

    expect(1) {
      currentConceptUris.size
    }

    val currentConceptUri = currentConceptUris.iterator.next

    val currentElemDeclOption: Option[Elem] = {
      val result = matchingConceptUris.filterKeys(currentConceptUris) map { _._2 }
      result.headOption
    }

    expect(Some("Current")) {
      currentElemDeclOption flatMap { _.attributeOption(EName("name")) }
    }

    val currentLabelOption: Option[xlink.Resource] = {
      val resources = matchingLabelUris.filterKeys(currentConceptUris) map { _._2 }
      resources.headOption
    }
    // Yes, attributes in the "xml" namespace are picked up nicely by yaidom
    val currentLabelLangOption = currentLabelOption flatMap { _.wrappedElem \@ "lang" }
    val currentLabelTextOption = currentLabelOption map { _.wrappedElem.text }

    expect(Some("nl")) {
      currentLabelLangOption
    }

    expect(Some("nl")) {
      currentLabelOption flatMap { _.wrappedElem \@ EName("http://www.w3.org/XML/1998/namespace", "lang") }
    }

    expect(Some("Huidig jaar")) {
      currentLabelTextOption
    }
  }
}
