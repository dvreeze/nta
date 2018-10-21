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
 * Taxonomy document validator contract. The validate method uses the validation scope, calls method
 * isTypeOfDocumentToValidate, and uses the excluded document URIs in order to determine which method calls to
 * method validateDocument must be done.
 *
 * @author Chris de Vreeze
 */
trait TaxonomyDocumentValidator extends TaxonomyValidator {

  /**
   * Returns the URIs of documents that are excluded from validation, although they may match the validation scope
   * and they may be accepted for validation.
   */
  def excludedDocumentUris: Set[URI]

  /**
   * Validates one taxonomy document. For each (non-excluded and accepted) document URI in the validation scope,
   * the validator will call this method once. It is the responsibility of the implementer of this method to honor
   * the validation scope as well!
   */
  def validateDocument(
    doc: TaxonomyDocument,
    taxonomy: Taxonomy,
    validationScope: ValidationScope): immutable.IndexedSeq[Result]

  /**
   * Returns true if the given document must be validated, according to the "type of document" it is, and
   * ignoring the excluded documents and validation scope (which is not even passed as parameter).
   *
   * For example, schema document validators should only accept schema documents, and not linkbase documents.
   * As another example, entrypoint schema document validators should only accept entrypoint schema documents, and
   * nothing else.
   */
  def isTypeOfDocumentToValidate(doc: TaxonomyDocument, taxonomy: Taxonomy): Boolean

  final def validate(taxonomy: Taxonomy, validationScope: ValidationScope): immutable.IndexedSeq[Result] = {
    taxonomy.findAllDocumentUris.toIndexedSeq
      .filter(uri => validationScope.matches(uri))
      .filterNot(excludedDocumentUris)
      .map(uri => taxonomy.getDocument(uri))
      .filter(doc => isTypeOfDocumentToValidate(doc, taxonomy))
      .flatMap(doc => validateDocument(doc, taxonomy, validationScope))
  }
}
