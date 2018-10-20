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
import eu.cdevreeze.tqa.base.dom.TaxonomyDocument

/**
 * Taxonomy document validator contract.
 *
 * @author Chris de Vreeze
 */
trait TaxonomyDocumentValidator extends TaxonomyValidator {

  /**
   * Validates one taxonomy document. For each (accepted) document URI in the validation scope, the validator will
   * call this method once.
   */
  def validateDocument(doc: TaxonomyDocument, taxonomy: Taxonomy): immutable.IndexedSeq[Result]

  /**
   * Returns true if the given document must be validated. This has nothing to do with filtering of taxonomy
   * documents, but is used to apply the validation only to the documents of the right "type". For example,
   * schema document validators should only accept schema documents, and not linkbase documents.
   *
   * Without this method, schema document validators would return Ok results for linkbase documents, instead
   * of just ignoring them.
   */
  def acceptForValidation(doc: TaxonomyDocument, taxonomy: Taxonomy): Boolean

  final def validate(validationScope: Set[URI], taxonomy: Taxonomy): immutable.IndexedSeq[Result] = {
    validationScope.toIndexedSeq
      .map(uri => taxonomy.getDocument(uri))
      .filter(doc => acceptForValidation(doc, taxonomy))
      .flatMap(doc => validateDocument(doc, taxonomy))
  }
}
