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
import eu.cdevreeze.tqa.ENames
import eu.cdevreeze.tqa.base.dom.TaxonomyDocument
import eu.cdevreeze.tqa.base.dom.XsdSchema

/**
 * Validator of rule 2.02.00.09. The rule says that the the schema document must have a @attributeFormDefault attribute
 * with value unqualified and a @elementFormDefault attribute with value qualified.
 *
 * @author Chris de Vreeze
 */
final class Validator_2_02_00_09(val excludedDocumentUris: Set[URI]) extends TaxonomyDocumentValidator {

  def ruleName: String = NtaRules.extractRuleName(getClass)

  def validateDocument(
    doc: TaxonomyDocument,
    taxonomy: Taxonomy,
    validationScope: ValidationScope): immutable.IndexedSeq[Result] = {

    require(isTypeOfDocumentToValidate(doc, taxonomy), s"Document ${doc.uri} should not be validated")

    val elementFormDefaultOption = doc.documentElement.attributeOption(ENames.ElementFormDefaultEName)
    val attributeFormDefaultOption = doc.documentElement.attributeOption(ENames.AttributeFormDefaultEName)

    val elementFormErrors =
      if (elementFormDefaultOption.isEmpty) {
        immutable.IndexedSeq(Result.makeErrorResult(
          ruleName,
          "missing-element-form-default",
          s"Missing elementFormDefault in '${doc.uri}'"))
      } else if (elementFormDefaultOption.contains("qualified")) {
        immutable.IndexedSeq()
      } else {
        immutable.IndexedSeq(Result.makeErrorResult(
          ruleName,
          "wrong-element-form-default",
          s"Wrong elementFormDefault in '${doc.uri}'"))
      }

    val attributeFormErrors =
      if (attributeFormDefaultOption.isEmpty) {
        immutable.IndexedSeq(Result.makeErrorResult(
          ruleName,
          "missing-attribute-form-default",
          s"Missing attributeFormDefault in '${doc.uri}'"))
      } else if (attributeFormDefaultOption.contains("unqualified")) {
        immutable.IndexedSeq()
      } else {
        immutable.IndexedSeq(Result.makeErrorResult(
          ruleName,
          "wrong-attribute-form-default",
          s"Wrong attributeFormDefault in '${doc.uri}'"))
      }

    elementFormErrors ++ attributeFormErrors
  }

  def isTypeOfDocumentToValidate(doc: TaxonomyDocument, taxonomy: Taxonomy): Boolean = {
    doc.documentElement.isInstanceOf[XsdSchema]
  }
}

object Validator_2_02_00_09 extends TaxonomyValidatorFactory {

  type Validator = Validator_2_02_00_09

  type CfgWrapper = NtaRuleConfigWrapper

  def ruleName: String = {
    NtaRules.extractRuleName(classOf[Validator_2_02_00_09])
  }

  def create(configWrapper: NtaRuleConfigWrapper): Validator_2_02_00_09 = {
    new Validator_2_02_00_09(
      configWrapper.excludedDocumentUrisForRule(ruleName))
  }
}
