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

import java.net.URI

import scala.collection.immutable

import eu.cdevreeze.nta.common.taxonomy.Taxonomy

/**
 * DTS set validator contract.
 *
 * @author Chris de Vreeze
 */
trait DtsSetValidator extends TaxonomyValidator {

  /**
   * Returns the URIs of entrypoint documents that are excluded from validation, although they may match the
   * validation scope.
   */
  def excludedEntrypointDocumentUris: Set[URI]

  /**
   * Validates the combined DTS set. This method is called only once by the validate method. The passed "combined
   * entrypoint" is the set of all entrypoints, minus partially or wholly excluded entrypoints and minus
   * entrypoints not matching the validation scope. It is the responsibility of the implementer of this method
   * to honor the validation scope as well!
   *
   * In the implementation of this method, use method `Taxonomy.filterEntrypointsReturningCombinedDtsAsUriSet`
   * to get the "combined DTS" for the given combined entrypoints.
   */
  def validateDtsSet(
    entrypoints: Set[Set[URI]],
    validationScope: ValidationScope,
    taxonomy: Taxonomy): immutable.IndexedSeq[Result]

  final def validate(validationScope: ValidationScope, taxonomy: Taxonomy): immutable.IndexedSeq[Result] = {
    val entrypoints: Set[Set[URI]] =
      taxonomy.dtsMap.keySet
        .filter(ep => ep.forall(uri => validationScope.matches(uri)))
        .filter(ep => ep.intersect(excludedEntrypointDocumentUris).isEmpty)

    validateDtsSet(entrypoints, validationScope, taxonomy)
  }
}
