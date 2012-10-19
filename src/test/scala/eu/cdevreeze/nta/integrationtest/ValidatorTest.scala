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
import common.validate._
import common.parse.TaxonomyParser
import rule._

/**
 * Test case that tests validations against the NL taxonomie version 6.0.
 *
 * @author Chris de Vreeze
 */
@RunWith(classOf[JUnitRunner])
class ValidatorTest extends FunSuite with BeforeAndAfterAll with TaxonomyParser {

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

  test("Test 2.2.0.05") {
    val validator = new Validator_2_2_0_05
    performSchemaTest(validator)
  }

  test("Test 2.2.0.06") {
    val validator = new Validator_2_2_0_06
    performSchemaTest(validator)
  }

  test("Test 2.2.0.08") {
    val validator = new Validator_2_2_0_08
    performSchemaTest(validator)
  }

  test("Test 2.2.0.09") {
    val validator = new Validator_2_2_0_09
    performSchemaTest(validator)
  }

  test("Test 2.2.0.10") {
    val validator = new Validator_2_2_0_10
    performSchemaTest(validator)
  }

  ignore("Test 2.2.0.11") {
    val validator = new Validator_2_2_0_11
    performSchemaTest(validator)
  }

  test("Test 2.2.0.12") {
    val validator = new Validator_2_2_0_12
    performSchemaTest(validator)
  }

  test("Test 2.2.0.14") {
    val validator = new Validator_2_2_0_14
    val offendingSchemas = taxonomy.schemas filter { case (uri, doc) => !validator.validate(doc)(taxonomy).isValid }

    // There are many offending schemas, with one or more xs:import child elements...
    val expectedOffendingSchemas = taxonomy.schemas filter {
      case (uri, doc) =>
        val ns = SchemaDocument.NS
        val matchingElms = doc.doc.documentElement filterChildElems { e => e.resolvedName == EName(ns, "import") }
        !matchingElms.isEmpty
    }

    expect(expectedOffendingSchemas.keySet) {
      offendingSchemas.keySet
    }
  }

  test("Test 2.2.0.18") {
    val validator = new Validator_2_2_0_18
    performSchemaTest(validator)
  }

  test("Test 2.2.0.22") {
    val validator = new Validator_2_2_0_22
    val offendingSchemas = taxonomy.schemas filter { case (uri, doc) => !validator.validate(doc)(taxonomy).isValid }

    // There is an offending schema...
    val expectedOffendingSchemas = taxonomy.schemas filter { case (uri, doc) => uri.toString.endsWith("/basis/bd/types/bd-codes.xsd") }
    expect(expectedOffendingSchemas.keySet) {
      offendingSchemas.keySet
    }
  }

  private def performSchemaTest(validator: Validator[SchemaDocument, Taxonomy]) {
    val offendingSchemas = taxonomy.schemas filter { case (uri, doc) => !validator.validate(doc)(taxonomy).isValid }

    expect(Set()) {
      offendingSchemas.keySet
    }
  }
}
