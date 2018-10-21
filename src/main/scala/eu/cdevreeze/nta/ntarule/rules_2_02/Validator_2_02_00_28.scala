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
import eu.cdevreeze.nta.common.validator.DtsValidator
import eu.cdevreeze.nta.common.validator.Result
import eu.cdevreeze.nta.common.validator.TaxonomyValidatorFactory
import eu.cdevreeze.nta.common.validator.ValidationScope
import eu.cdevreeze.nta.ntarule.NtaRuleConfigWrapper
import eu.cdevreeze.nta.ntarule.NtaRules

/**
 * Validator of rule 2.02.00.28. The rule says that each DTS must have no arcs with the same ELR, source
 * and target.
 *
 * @author Chris de Vreeze
 */
final class Validator_2_02_00_28(val excludedEntrypointDocumentUris: Set[URI]) extends DtsValidator {

  def ruleName: String = NtaRules.extractRuleName(getClass)

  def validateDts(
    entrypoint: Set[URI],
    taxonomy: Taxonomy,
    validationScope: ValidationScope): immutable.IndexedSeq[Result] = {

    val dts = taxonomy.getDts(entrypoint)

    val groupedRelationships =
      dts.relationships.groupBy { rel =>
        (rel.elr, rel.sourceElem.key, rel.targetElem.key)
      }

    val violatingGroups =
      groupedRelationships.filter(_._2.size > 1).keySet

    violatingGroups.toIndexedSeq.map {
      case key @ (elr, sourceKey, targetKey) =>
        val arcs = groupedRelationships(key).map(_.arc)

        Result.makeErrorResult(
          ruleName,
          "duplicate-arcs",
          s"Duplicate arcs (w.r.t. source, target and ELR) found for entrypoint ${entrypoint.mkString(", ")}. " +
            s"Arcs: ${arcs.map(_.key).mkString(", ")}")
    }
  }
}

object Validator_2_02_00_28 extends TaxonomyValidatorFactory {

  type Validator = Validator_2_02_00_28

  type CfgWrapper = NtaRuleConfigWrapper

  def ruleName: String = {
    NtaRules.extractRuleName(classOf[Validator_2_02_00_28])
  }

  def create(configWrapper: NtaRuleConfigWrapper): Validator_2_02_00_28 = {
    new Validator_2_02_00_28(
      configWrapper.excludedEntrypointDocumentUrisForRule(ruleName))
  }
}
