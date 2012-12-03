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

import scala.collection.immutable
import common.document.{ SchemaDocument, Taxonomy }
import common.validate.{ Validator, ValidationResult }
import eu.cdevreeze.yaidom._

/**
 * Validator of rule 2.2.0.11. The rule says that the schema document must not declare any namespaces that are not used in the document.
 *
 * My interpretation  is that all namespaces must be used in element or attribute names, ignoring text nodes.
 *
 * TODO Fix, keeping imports in mind (which increases the "context" of the validator)!
 *
 * @author Chris de Vreeze
 */
final class Validator_2_2_0_11 extends Validator[SchemaDocument, Taxonomy] {

  def validate(doc: SchemaDocument)(context: Taxonomy): ValidationResult[SchemaDocument] = {
    val adaptedRootElm = doc.doc.documentElement.notUndeclaringPrefixes(Scope.Empty)

    val unusedNamespaceUrisIgnoringTns: Set[String] = unusedNamespacesIgnoringTns(Scope.Empty, adaptedRootElm)
    val unusedNamespaceUris: Set[String] = unusedNamespaceUrisIgnoringTns diff doc.targetNamespaceOption.toSet

    if (unusedNamespaceUris.isEmpty) ValidationResult.validResult(doc)
    else {
      val messages = unusedNamespaceUris map { nsUri => "Namespace URI '%s' unused from the point it was declared".format(nsUri) }
      new ValidationResult(doc, false, messages.toIndexedSeq)
    }
  }

  private def unusedNamespacesIgnoringTns(parentScope: Scope, elm: Elem): Set[String] = {
    val introducedNamespaceUris: Set[String] = parentScope.relativize(elm.scope).withoutUndeclarations.map.values.toSet

    val usedNamespaceUris: Set[String] = introducedNamespaceUris filter { ns =>
      val matchingElmOption = elm findElemOrSelf { e =>
        e.resolvedName.namespaceUriOption == Some(ns) ||
          e.resolvedAttributes.map(_._1).flatMap(_.namespaceUriOption).contains(ns)
      }
      matchingElmOption.isDefined
    }

    val unusedNamespaceUris = introducedNamespaceUris diff usedNamespaceUris

    // Recursive but not tail-recursive calls

    elm.allChildElems.foldLeft(unusedNamespaceUris) { (acc, childElm) => acc union unusedNamespacesIgnoringTns(elm.scope, childElm) }
  }
}
