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

import java.net.URI
import eu.cdevreeze.yaidom.{ Document, Elem, EName, QName }
import LinkbaseDocument._

/**
 * XBRL linkbase document.
 *
 * @author Chris de Vreeze
 */
final class LinkbaseDocument(
  override val originalUri: URI,
  override val localUri: URI,
  override val doc: Document) extends TaxonomyDocument {

  require(originalUri ne null)
  require(localUri ne null)
  require(doc ne null)

  require(originalUri.isAbsolute, "The original URI '%s' is not absolute".format(originalUri))
  require(localUri.isAbsolute, "The local URI '%s' is not absolute".format(localUri))
  require(originalUri.getFragment eq null, "The original URI '%s' has a fragment".format(originalUri))
  require(localUri.getFragment eq null, "The local URI '%s' has a fragment".format(localUri))

  require(doc.documentElement.resolvedName == EName(NS, "linkbase"))
}

object LinkbaseDocument {

  val NS = "http://www.xbrl.org/2003/linkbase"
}
