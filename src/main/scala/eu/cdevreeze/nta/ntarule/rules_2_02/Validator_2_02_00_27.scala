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
import scala.reflect.classTag

import eu.cdevreeze.nta.common.taxonomy.Taxonomy
import eu.cdevreeze.nta.common.validator.Result
import eu.cdevreeze.nta.common.validator.TaxonomyDocumentValidator
import eu.cdevreeze.nta.common.validator.TaxonomyValidatorFactory
import eu.cdevreeze.nta.common.validator.ValidationScope
import eu.cdevreeze.nta.ntarule.NtaRuleConfigWrapper
import eu.cdevreeze.nta.ntarule.NtaRules
import eu.cdevreeze.tqa.base.dom.LinkbaseRef
import eu.cdevreeze.tqa.base.dom.TaxonomyDocument
import eu.cdevreeze.tqa.base.dom.XsdSchema

/**
 * Validator of rule 2.02.00.27. The rule says that schema must have unique linkbaseRefs.
 *
 * @author Chris de Vreeze
 */
final class Validator_2_02_00_27(val excludedDocumentUris: Set[URI]) extends TaxonomyDocumentValidator {

  def ruleName: String = NtaRules.extractRuleName(getClass)

  def validateDocument(
    doc: TaxonomyDocument,
    validationScope: ValidationScope,
    taxonomy: Taxonomy): immutable.IndexedSeq[Result] = {

    require(acceptForValidation(doc, taxonomy), s"Document ${doc.uri} should not be validated")

    val linkbaseRefs = doc.documentElement.findAllElemsOfType(classTag[LinkbaseRef])

    val linkbaseRefGroups = linkbaseRefs.groupBy(_.resolvedHref)

    val offendingHrefs = linkbaseRefGroups.filter(_._2.size > 1).keySet

    if (offendingHrefs.isEmpty) {
      immutable.IndexedSeq()
    } else {
      immutable.IndexedSeq(Result.makeErrorResult(
        ruleName,
        "duplicate-linkbaserefs-not-allowed",
        s"Duplicate linkbaseRefs not allowed but found duplicates ${offendingHrefs.mkString(", ")} in '${doc.uri}'"))
    }
  }

  def acceptForValidation(doc: TaxonomyDocument, taxonomy: Taxonomy): Boolean = {
    doc.documentElement.isInstanceOf[XsdSchema]
  }
}

object Validator_2_02_00_27 extends TaxonomyValidatorFactory {

  type Validator = Validator_2_02_00_27

  type CfgWrapper = NtaRuleConfigWrapper

  def ruleName: String = {
    NtaRules.extractRuleName(classOf[Validator_2_02_00_27])
  }

  def create(configWrapper: NtaRuleConfigWrapper): Validator_2_02_00_27 = {
    new Validator_2_02_00_27(
      configWrapper.excludedDocumentUrisForRule(ruleName))
  }
}
