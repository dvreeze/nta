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
import eu.cdevreeze.tqa.ENames
import eu.cdevreeze.tqa.base.dom.TaxonomyDocument
import eu.cdevreeze.tqa.base.dom.XsdSchema

/**
 * Validator of rule 2.2.0.08. The rule says that the schema document must have a @targetNamespace attribute.
 *
 * @author Chris de Vreeze
 */
final class Validator_2_02_00_08 extends TaxonomyDocumentValidator {

  def validateDocument(doc: TaxonomyDocument, taxonomy: Taxonomy): immutable.IndexedSeq[Result] = {
    if (doc.documentElement.attributeOption(ENames.TargetNamespaceEName).isDefined) {
      immutable.IndexedSeq()
    } else {
      immutable.IndexedSeq(Result(
        "2.02.00.08",
        Level.Error,
        s"Target namespace attribute required but found none in '${doc.uri}'"))
    }
  }

  def acceptForValidation(doc: TaxonomyDocument, taxonomy: Taxonomy): Boolean = {
    doc.documentElement.isInstanceOf[XsdSchema]
  }
}

object Validator_2_02_00_08 extends TaxonomyValidatorFactory {

  type Validator = Validator_2_02_00_08

  def create(config: Config): Validator_2_02_00_08 = {
    new Validator_2_02_00_08
  }
}
