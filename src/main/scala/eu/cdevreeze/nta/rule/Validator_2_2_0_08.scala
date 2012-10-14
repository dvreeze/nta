/*
 * Copyright 2011 Chris de Vreeze
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

package eu.cdevreeze.nta
package rule

import common.document.{ SchemaDocument, Taxonomy }
import common.validate.{ Validator, ValidationResult }
import eu.cdevreeze.yaidom.EName

/**
 * Validator of rule 2.2.0.08. The rule says that the the schema document must have a @targetNamespace attribute.
 *
 * @author Chris de Vreeze
 */
final class Validator_2_2_0_08 extends Validator[SchemaDocument, Taxonomy] {

  def validate(doc: SchemaDocument)(context: Taxonomy): ValidationResult[SchemaDocument] = {
    if (doc.doc.documentElement.attributeOption(EName("targetNamespace")).isDefined) ValidationResult.validResult(doc)
    else {
      new ValidationResult(doc, false, Vector("Missing @targetNamespace"))
    }
  }
}
