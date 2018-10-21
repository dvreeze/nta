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

package eu.cdevreeze.nta.ntarule.rules_2_02

import java.net.URI

import scala.collection.immutable

import eu.cdevreeze.nta.common.taxonomy.Taxonomy
import eu.cdevreeze.nta.common.validator.Result
import eu.cdevreeze.nta.common.validator.TaxonomyDocumentValidator
import eu.cdevreeze.nta.common.validator.TaxonomyValidatorFactory
import eu.cdevreeze.nta.common.validator.ValidationScope
import eu.cdevreeze.nta.ntarule.NtaRuleConfigWrapper
import eu.cdevreeze.nta.ntarule.NtaRules
import eu.cdevreeze.tqa.base.dom.TaxonomyDocument
import eu.cdevreeze.tqa.base.dom.XsdSchema

/**
 * Validator of rule 2.02.00.19. The rule says that in the schema document all namespace declarations must occur only in
 * the root element.
 *
 * Implementation note: we can only check that "significant" namespace declarations occur only in the root
 * element, because "superfluous" namespace declarations are lost after parsing.
 *
 * @author Chris de Vreeze
 */
final class Validator_2_02_00_19(val excludedDocumentUris: Set[URI]) extends TaxonomyDocumentValidator {

  def ruleName: String = NtaRules.extractRuleName(getClass)

  def validateDocument(
    doc: TaxonomyDocument,
    taxonomy: Taxonomy,
    validationScope: ValidationScope): immutable.IndexedSeq[Result] = {

    require(acceptForValidation(doc, taxonomy), s"Document ${doc.uri} should not be validated")

    val invalidElems = doc.documentElement.filterElems(e => e.backingElem.namespaces.nonEmpty)

    if (invalidElems.isEmpty) {
      immutable.IndexedSeq()
    } else {
      immutable.IndexedSeq(Result.makeErrorResult(
        ruleName,
        "namespace-declarations-below-root-not-allowed",
        s"No namespace declarations allowed on non-root elements but found ${invalidElems.size} such elements in '${doc.uri}'"))
    }
  }

  def acceptForValidation(doc: TaxonomyDocument, taxonomy: Taxonomy): Boolean = {
    doc.documentElement.isInstanceOf[XsdSchema]
  }
}

object Validator_2_02_00_19 extends TaxonomyValidatorFactory {

  type Validator = Validator_2_02_00_19

  type CfgWrapper = NtaRuleConfigWrapper

  def ruleName: String = {
    NtaRules.extractRuleName(classOf[Validator_2_02_00_19])
  }

  def create(configWrapper: NtaRuleConfigWrapper): Validator_2_02_00_19 = {
    new Validator_2_02_00_19(
      configWrapper.excludedDocumentUrisForRule(ruleName))
  }
}
