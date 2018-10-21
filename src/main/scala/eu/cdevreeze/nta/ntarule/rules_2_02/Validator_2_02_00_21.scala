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
import eu.cdevreeze.nta.common.validator.DtsSetValidator
import eu.cdevreeze.nta.common.validator.Result
import eu.cdevreeze.nta.common.validator.TaxonomyValidatorFactory
import eu.cdevreeze.nta.common.validator.ValidationScope
import eu.cdevreeze.nta.ntarule.NtaRuleConfigWrapper
import eu.cdevreeze.nta.ntarule.NtaRules
import eu.cdevreeze.tqa.base.relationship.DefinitionRelationship
import eu.cdevreeze.tqa.base.relationship.PresentationRelationship
import eu.cdevreeze.yaidom.core.EName

/**
 * Validator of rule 2.02.00.21. All concepts (in the NT) must be used in at least one P-link or D-link
 * in at least one DTS.
 *
 * The rule description speaks about elements instead of just concepts, but how does that relate to P-arcs
 * and D-arcs? So here we check concepts and not all declared elements.
 *
 * @author Chris de Vreeze
 */
final class Validator_2_02_00_21(val excludedEntrypointDocumentUris: Set[URI]) extends DtsSetValidator {

  def ruleName: String = NtaRules.extractRuleName(getClass)

  def validateDtsSet(
    entrypoints: Set[Set[URI]],
    taxonomy: Taxonomy,
    validationScope: ValidationScope): immutable.IndexedSeq[Result] = {

    val combinedDtsUris: Set[URI] = taxonomy.filterEntrypointsReturningCombinedDtsAsUriSet(entrypoints)

    val concepts: Set[EName] =
      taxonomy.universeTaxonomy.findAllConceptDeclarations
        .filter(e => validationScope.matches(e.globalElementDeclaration.docUri))
        .map(_.targetEName).toSet

    val conceptsInPLinks: Set[EName] =
      taxonomy.universeTaxonomy
        .filterPresentationRelationshipsOfType(classTag[PresentationRelationship]) { rel =>
          combinedDtsUris.contains(rel.docUri) && validationScope.matches(rel.docUri)
        }
        .flatMap(rel => List(rel.sourceConceptEName, rel.targetConceptEName))
        .toSet

    val conceptsInDLinks: Set[EName] =
      taxonomy.universeTaxonomy
        .filterInterConceptRelationshipsOfType(classTag[DefinitionRelationship]) { rel =>
          combinedDtsUris.contains(rel.docUri) && validationScope.matches(rel.docUri)
        }
        .flatMap(rel => List(rel.sourceConceptEName, rel.targetConceptEName))
        .toSet

    val violatingConcepts: Set[EName] = concepts.diff(conceptsInPLinks).diff(conceptsInDLinks)

    violatingConcepts.toIndexedSeq.map { concept =>
      Result.makeErrorResult(
        ruleName,
        "unused-concept-in-linkbases",
        s"Missing P-link and D-link references for concept $concept")
    }
  }
}

object Validator_2_02_00_21 extends TaxonomyValidatorFactory {

  type Validator = Validator_2_02_00_21

  type CfgWrapper = NtaRuleConfigWrapper

  def ruleName: String = {
    NtaRules.extractRuleName(classOf[Validator_2_02_00_21])
  }

  def create(configWrapper: NtaRuleConfigWrapper): Validator_2_02_00_21 = {
    new Validator_2_02_00_21(
      configWrapper.excludedEntrypointDocumentUrisForRule(ruleName))
  }
}
