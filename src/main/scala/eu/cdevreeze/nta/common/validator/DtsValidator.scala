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
 * DTS validator contract.
 *
 * @author Chris de Vreeze
 */
trait DtsValidator extends TaxonomyValidator {

  /**
   * Validates one DTS. For each entrypoint (entirely) in the validation scope, this method is called once. Note that
   * the validation scope is only used to filter the entrypoints, not the complete DTSes!
   */
  def validateDts(entrypoint: Set[URI], taxonomy: Taxonomy): immutable.IndexedSeq[Result]

  final def validate(validationScope: Set[URI], taxonomy: Taxonomy): immutable.IndexedSeq[Result] = {
    taxonomy.dtsMap.keySet.toIndexedSeq
      .filter(ep => ep.subsetOf(validationScope))
      .flatMap(ep => validateDts(ep, taxonomy))
  }
}
