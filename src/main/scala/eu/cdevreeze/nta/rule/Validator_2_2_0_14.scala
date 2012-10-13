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
 * Validator of rule 2.2.0.14. The rule says that in the schema document xs:import elements must have "parent path" //xs:annotation/xs:appinfo.
 *
 * @author Chris de Vreeze
 */
final class Validator_2_2_0_14 extends Validator[SchemaDocument] {

  def apply(x: SchemaDocument): ValidationResult[SchemaDocument] = {
    val ns = SchemaDocument.NS

    val matchingElmPaths = x.doc.documentElement filterElemOrSelfPaths { e => e.resolvedName == EName(ns, "import") }

    val rejectedElmPaths = matchingElmPaths filter { path =>
      assert(path.lastEntry.elementName == EName(ns, "import"))
      assert(path.lastEntry.elementName == x.doc.documentElement.getWithElemPath(path).resolvedName)

      path.entries.size < 3 ||
        path.parentPath.lastEntry.elementName != EName(SchemaDocument.NS, "appinfo") ||
        path.parentPath.parentPath.lastEntry.elementName != EName(SchemaDocument.NS, "annotation")
    }

    if (rejectedElmPaths.isEmpty) ValidationResult.validResult(x)
    else {
      new ValidationResult(x, false, Vector("Not all xs:import elements have 'parent path' //xs:annotation/xs:appinfo"))
    }
  }
}
