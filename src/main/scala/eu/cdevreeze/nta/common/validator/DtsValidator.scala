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
 * DTS validator contract. The validate method uses the validation scope and the excluded entrypoint document URIs
 * in order to determine which method calls to method validateDts must be done.
 *
 * @author Chris de Vreeze
 */
trait DtsValidator extends TaxonomyValidator {

  /**
   * Returns the URIs of entrypoint documents that are excluded from validation, although they may match the
   * validation scope.
   */
  def excludedEntrypointDocumentUris: Set[URI]

  /**
   * Validates one DTS. For each (non-excluded) entrypoint (entirely) in the validation scope, this method is
   * called once. It is the responsibility of the implementer of this method to honor the validation scope as well!
   *
   * In the implementation of this method, use method `Taxonomy.getDts` to get the DTS for the given entrypoint.
   */
  def validateDts(
    entrypoint: Set[URI],
    taxonomy: Taxonomy,
    validationScope: ValidationScope): immutable.IndexedSeq[Result]

  final def validate(taxonomy: Taxonomy, validationScope: ValidationScope): immutable.IndexedSeq[Result] = {
    taxonomy.dtsMap.keySet.toIndexedSeq
      .filter(ep => ep.forall(uri => validationScope.matches(uri)))
      .filter(ep => ep.intersect(excludedEntrypointDocumentUris).isEmpty)
      .flatMap(ep => validateDts(ep, taxonomy, validationScope))
  }
}
