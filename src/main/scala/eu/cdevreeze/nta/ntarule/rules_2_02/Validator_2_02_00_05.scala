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

import scala.collection.immutable

import com.typesafe.config.Config

import eu.cdevreeze.nta.common.taxonomy.Taxonomy
import eu.cdevreeze.nta.common.validator.Level
import eu.cdevreeze.nta.common.validator.Result
import eu.cdevreeze.nta.common.validator.TaxonomyDocumentValidator
import eu.cdevreeze.nta.common.validator.TaxonomyValidatorFactory
import eu.cdevreeze.tqa.base.dom.TaxonomyCommentNode
import eu.cdevreeze.tqa.base.dom.TaxonomyDocument
import eu.cdevreeze.tqa.base.dom.XsdSchema

/**
 * Validator of rule 2.2.0.05. The rule says that there must be at most one comment in the schema document.
 *
 * The actual check that has been implemented checks that in the element tree there are no comments. There
 * may be a top-level comment outside the document element, as direct document child, but not anywhere else.
 *
 * @author Chris de Vreeze
 */
final class Validator_2_02_00_05 extends TaxonomyDocumentValidator {

  def validateDocument(doc: TaxonomyDocument, taxonomy: Taxonomy): immutable.IndexedSeq[Result] = {
    require(acceptForValidation(doc, taxonomy), s"Document ${doc.uri} should not be validated")

    val offendingComments =
      doc.documentElement.findAllElemsOrSelf.flatMap(_.children) collect { case c: TaxonomyCommentNode => c }

    if (offendingComments.isEmpty) {
      immutable.IndexedSeq()
    } else {
      immutable.IndexedSeq(Result(
        "2.02.00.05",
        Level.Error,
        s"No comments in element tree allowed but found ${offendingComments.size} such comments in '${doc.uri}'"))
    }
  }

  def acceptForValidation(doc: TaxonomyDocument, taxonomy: Taxonomy): Boolean = {
    doc.documentElement.isInstanceOf[XsdSchema]
  }
}

object Validator_2_02_00_05 extends TaxonomyValidatorFactory {

  type Validator = Validator_2_02_00_05

  def create(config: Config): Validator_2_02_00_05 = {
    new Validator_2_02_00_05
  }
}
