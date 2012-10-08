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
package common
package document

import eu.cdevreeze.yaidom.{ Document, Elem, EName, QName }
import SchemaDocument._

/**
 * XML Schema document.
 *
 * @author Chris de Vreeze
 */
final class SchemaDocument(val doc: Document) extends Immutable {

  require(doc ne null)
  require(doc.documentElement.resolvedName == EName(NS, "schema"))
}

object SchemaDocument {

  val NS = "http://www.w3.org/2001/XMLSchema"
}
