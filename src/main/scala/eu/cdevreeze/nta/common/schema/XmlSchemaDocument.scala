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
package schema

import java.net.URI
import scala.collection.immutable
import eu.cdevreeze.yaidom._
import XmlSchemaObject._

/**
 * Immutable XML Schema Document.
 *
 * @author Chris de Vreeze
 */
sealed class XmlSchemaDocument(val wrappedDocument: indexed.Document) extends Immutable {

  final val schemaElement: XmlSchemaObject = XmlSchemaObject(wrappedDocument.documentElement)

  final override def toString: String = wrappedDocument.document.toString
}
