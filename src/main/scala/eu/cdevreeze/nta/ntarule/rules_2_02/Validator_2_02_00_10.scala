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
import eu.cdevreeze.nta.ntarule.NtaRules
import eu.cdevreeze.tqa.base.dom.TaxonomyDocument
import eu.cdevreeze.tqa.base.dom.XsdSchema
import eu.cdevreeze.yaidom.core.EName
import eu.cdevreeze.nta.ntarule.NtaRuleConfigWrapper
import eu.cdevreeze.nta.ntarule.NtaRuleConfigWrapper

/**
 * Validator of rule 2.02.00.10. The rule says that the the schema document must not have blockDefault, finalDefault and
 * version attributes.
 *
 * @author Chris de Vreeze
 */
final class Validator_2_02_00_10(val excludedDocumentUris: Set[URI]) extends TaxonomyDocumentValidator {

  import Validator_2_02_00_10._

  def ruleName: String = NtaRules.extractRuleName(getClass)

  def validateDocument(
    doc: TaxonomyDocument,
    validationScope: ValidationScope,
    taxonomy: Taxonomy): immutable.IndexedSeq[Result] = {

    require(acceptForValidation(doc, taxonomy), s"Document ${doc.uri} should not be validated")

    val blockDefaultOption = doc.documentElement.attributeOption(BlockDefaultEName)
    val finalDefaultOption = doc.documentElement.attributeOption(FinalDefaultEName)
    val versionOption = doc.documentElement.attributeOption(VersionEName)

    val blockDefaultErrors = blockDefaultOption.toIndexedSeq.map(_ => Result.makeErrorResult(
      ruleName,
      "block-default-not-allowed",
      s"Attribute blockDefault not allowed in '${doc.uri}'"))

    val finalDefaultErrors = finalDefaultOption.toIndexedSeq.map(_ => Result.makeErrorResult(
      ruleName,
      "final-default-not-allowed",
      s"Attribute finalDefault not allowed in '${doc.uri}'"))

    val versionErrors = versionOption.toIndexedSeq.map(_ => Result.makeErrorResult(
      ruleName,
      "version-not-allowed",
      s"Attribute version not allowed in '${doc.uri}'"))

    blockDefaultErrors ++ finalDefaultErrors ++ versionErrors
  }

  def acceptForValidation(doc: TaxonomyDocument, taxonomy: Taxonomy): Boolean = {
    doc.documentElement.isInstanceOf[XsdSchema]
  }
}

object Validator_2_02_00_10 extends TaxonomyValidatorFactory {

  type Validator = Validator_2_02_00_10

  type CfgWrapper = NtaRuleConfigWrapper

  def ruleName: String = {
    NtaRules.extractRuleName(classOf[Validator_2_02_00_10])
  }

  def create(configWrapper: NtaRuleConfigWrapper): Validator_2_02_00_10 = {
    new Validator_2_02_00_10(
      configWrapper.excludedDocumentUrisForRule(ruleName))
  }

  private val BlockDefaultEName = EName("blockDefault")
  private val FinalDefaultEName = EName("finalDefault")
  private val VersionEName = EName("version")
}
