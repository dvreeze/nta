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
import eu.cdevreeze.nta.common.validator.DtsSetValidator
import eu.cdevreeze.nta.common.validator.Result
import eu.cdevreeze.nta.common.validator.TaxonomyValidatorFactory
import eu.cdevreeze.nta.common.validator.ValidationScope
import eu.cdevreeze.nta.ntarule.NtaRuleConfigWrapper
import eu.cdevreeze.nta.ntarule.NtaRules
import eu.cdevreeze.tqa.base.common.StandardLabelRoles
import eu.cdevreeze.yaidom.core.EName

/**
 * Validator of rule 2.02.02.26. All concepts must have a standard label in the local language (NL).
 *
 * @author Chris de Vreeze
 */
final class Validator_2_02_02_26(val localLanguageCode: String) extends DtsSetValidator {

  def ruleName: String = NtaRules.extractRuleName(getClass)

  def excludedEntrypointDocumentUris: Set[URI] = Set() // TODO

  def validateDtsSet(
    entrypoints: Set[Set[URI]],
    validationScope: ValidationScope,
    taxonomy: Taxonomy): immutable.IndexedSeq[Result] = {

    val combinedDtsUris: Set[URI] = taxonomy.filterEntrypointsReturningCombinedDtsAsUriSet(entrypoints)

    val concepts: Set[EName] =
      taxonomy.universeTaxonomy.findAllConceptDeclarations
        .filter(e => combinedDtsUris.contains(e.globalElementDeclaration.docUri))
        .filter(e => validationScope.matches(e.globalElementDeclaration.docUri))
        .map(_.targetEName).toSet

    val violatingConcepts: Set[EName] =
      concepts.filter { concept =>
        taxonomy.universeTaxonomy.filterOutgoingConceptLabelRelationships(concept) { rel =>
          rel.resourceRole == StandardLabelRoles.StandardLabel &&
            rel.language == localLanguageCode &&
            combinedDtsUris.contains(rel.arc.docUri) &&
            validationScope.matches(rel.arc.docUri)
        }.isEmpty
      }

    violatingConcepts.toIndexedSeq.map { concept =>
      Result.makeErrorResult(
        ruleName,
        "missing-standard-label-in-local-language",
        s"Missing standard label in local language $localLanguageCode for concept $concept")
    }
  }
}

object Validator_2_02_02_26 extends TaxonomyValidatorFactory {

  type Validator = Validator_2_02_02_26

  type CfgWrapper = NtaRuleConfigWrapper

  def create(configWrapper: NtaRuleConfigWrapper): Validator_2_02_02_26 = {
    new Validator_2_02_02_26(configWrapper.localLanguageCode)
  }
}
