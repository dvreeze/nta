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

package eu.cdevreeze.nta.common.validator

import scala.collection.immutable

import eu.cdevreeze.nta.common.taxonomy.Taxonomy

/**
 * Taxonomy validator contract.
 *
 * "Pre-validators" such as those that fend off XML security attacks or that check for BOM characters cannot
 * be implemented (naturally) with this contract.
 *
 * Note that due to the notion of a validation scope, this contract also works for ad-hoc extension taxonomies,
 * where only the extension part of the taxonomy should be validated.
 *
 * @author Chris de Vreeze
 */
trait TaxonomyValidator {

  /**
   * Returns the unique rule name, such as "2.02.00.05".
   */
  def ruleName: String

  /**
   * Validates the given taxonomy, but restricted to the given validation scope.
   *
   * It depends on the kind of validator how the validation scope is used precisely, but in general it should be
   * respected by validators.
   */
  def validate(validationScope: ValidationScope,taxonomy: Taxonomy): immutable.IndexedSeq[Result]
}
