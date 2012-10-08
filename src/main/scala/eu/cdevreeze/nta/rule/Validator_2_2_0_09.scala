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
import eu.cdevreeze.yaidom.EName

/**
 * Validator of rule 2.2.0.09. The rule says that the the schema document must have a @attributeFormDefault attribute
 * with value unqualified and a @elementFormDefault attribute with value qualified.
 *
 * @author Chris de Vreeze
 */
final class Validator_2_2_0_09 extends Validator[SchemaDocument] {

  def apply(x: SchemaDocument): ValidationResult[SchemaDocument] = {
    val attributeFormDefaultOk = x.doc.documentElement.attributeOption(EName("@attributeFormDefault")) == Some("unqualified")
    val elementFormDefaultOk = x.doc.documentElement.attributeOption(EName("@elementFormDefault")) == Some("qualified")

    if (attributeFormDefaultOk && elementFormDefaultOk) ValidationResult.validResult(x)
    else {
      new ValidationResult(x, false, Vector("Missing or incorrect @attributeFormDefault and/or @elementFormDefault"))
    }
  }
}
