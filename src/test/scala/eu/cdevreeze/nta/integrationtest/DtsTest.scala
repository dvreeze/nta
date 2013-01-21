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
import common.dts.DTSFinder

/**
 * Test case that tests DTS discovery.
 *
 * @author Chris de Vreeze
 */
@RunWith(classOf[JUnitRunner])
class DtsTest extends FunSuite with BeforeAndAfterAll with TaxonomyParser {

  @volatile private var taxonomy: Taxonomy = _

  override def beforeAll(): Unit = {
    val rootDir = new jio.File(classOf[NlTaxonomieTest].getResource("/extracted-taxonomies/www.nltaxonomie.nl/7.0").toURI)
    require(
      rootDir.isDirectory && rootDir.exists,
      "Expected root directory %s. Extract file /compressed-taxonomies/www.nltaxonomie.nl-7.0.zip into this directory".format(rootDir.getPath))

    def localUriToOriginalUri(localUri: URI): URI = {
      val localUriString = localUri.toString
      val idx = localUriString.indexOf("/www.nltaxonomie.nl/7.0")
      require(idx > 0, "Expected '7.0' URI, but found '%s'".format(localUriString))
      new URI("http://" + (localUriString.drop(idx + 1)))
    }

    val rawTaxonomy = parse(rootDir)(localUriToOriginalUri)
    // Let's make the Taxonomy useful
    taxonomy = rawTaxonomy.withFoundSubstitutionGroups

    logger.info("Found %d schema documents".format(taxonomy.schemas.size))

    logger.info("Found %d linkbase documents".format(taxonomy.linkbases.size))
  }

  override def afterAll(): Unit = {
  }

  test("Test DTS from entry point http://www.nltaxonomie.nl/7.0/report/bd/entrypoints/rpt-bd-ih-sba-2012.xsd") {
    val taxonomyDocs = taxonomy.taxonomyDocuments mapValues { _.doc }
    val dtsFinder = new DTSFinder(taxonomyDocs)

    val entryPointUriOption =
      taxonomyDocs.keySet find { uri => stripUntilVersion(uri.toString) == "/7.0/report/bd/entrypoints/rpt-bd-ih-sba-2012.xsd" }
    val entryPointUri = entryPointUriOption.get

    val result = dtsFinder.findDTS(entryPointUri)

    val someExpectedPaths: Set[String] =
      Set(
        "/7.0/report/bd/linkroles/bd-ih-service-berichten-aanslag-pre.xml",
        "/7.0/report/bd/linkroles/bd-ih-sba-lineitems.xml",
        "/7.0/basis/bd/items/bd-aanslag.xsd",
        "/7.0/basis/bd/items/bd-burgers.xsd",
        "/7.0/domein/bd/tuples/bd-asl-tuples.xsd",
        "/7.0/basis/sbr/items/nl-common-data.xsd")

    expect(someExpectedPaths) {
      val dtsUris = result.dts.keySet map { uri => stripUntilVersion(uri.toString) }
      dtsUris intersect someExpectedPaths
    }
  }

  test("Test DTS from entry point http://www.nltaxonomie.nl/7.0/domein/bd/tuples/bd-asl-tuples.xsd") {
    val taxonomyDocs = taxonomy.taxonomyDocuments mapValues { _.doc }
    val dtsFinder = new DTSFinder(taxonomyDocs)

    val entryPointUriOption =
      taxonomyDocs.keySet find { uri => stripUntilVersion(uri.toString) == "/7.0/domein/bd/tuples/bd-asl-tuples.xsd" }
    val entryPointUri = entryPointUriOption.get

    val result = dtsFinder.findDTS(entryPointUri)

    val someExpectedPaths: Set[String] =
      Set(
        "/7.0/basis/bd/items/bd-aanslag.xsd",
        "/7.0/basis/bd/items/bd-burgers.xsd",
        "/7.0/basis/sbr/items/nl-common-data.xsd")

    expect(someExpectedPaths) {
      val dtsUris = result.dts.keySet map { uri => stripUntilVersion(uri.toString) }
      dtsUris intersect someExpectedPaths
    }
  }

  private def stripUntilVersion(uri: String): String = {
    val nlTaxString = "www.nltaxonomie.nl"
    require(uri.contains(nlTaxString))
    val i = uri.indexOf(nlTaxString)
    uri.drop(i).drop(nlTaxString.length)
  }
}
