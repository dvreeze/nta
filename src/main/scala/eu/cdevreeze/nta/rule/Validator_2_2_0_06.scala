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

import common.document.SchemaDocument
import common.validate.{ Validator, ValidationResult }
import eu.cdevreeze.yaidom._

/**
 * Validator of rule 2.2.0.06. The rule says that there must be only prefixed element nodes in the schema document.
 *
 * @author Chris de Vreeze
 */
final class Validator_2_2_0_06 extends Validator[SchemaDocument] {

  def apply(x: SchemaDocument): ValidationResult[SchemaDocument] = {
    val unprefixedElms = x.doc.documentElement filterElemsOrSelf { e => e.qname.prefixOption.isEmpty }

    if (unprefixedElms.isEmpty) ValidationResult.validResult(x)
    else {
      val messages = unprefixedElms map { e => "Found unprefixed element '%s'".format(e.qname) }
      new ValidationResult(x, false, messages)
    }
  }
}
