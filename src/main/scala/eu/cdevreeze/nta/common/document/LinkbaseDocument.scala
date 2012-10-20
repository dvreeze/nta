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
import scala.collection.immutable
import eu.cdevreeze.yaidom.{ Document, Elem, EName, QName, xlink }
import LinkbaseDocument._

/**
 * XBRL linkbase document. May be overridden.
 *
 * @author Chris de Vreeze
 */
class LinkbaseDocument(
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

  final def extendedLinks: immutable.IndexedSeq[xlink.ExtendedLink] =
    doc.documentElement collectFromElemsOrSelf { case e if xlink.XLink.mustBeExtendedLink(e) => xlink.ExtendedLink(e) }

  final def labelToXLinkMap(extendedLink: xlink.ExtendedLink): Map[String, xlink.XLink] = {
    val locators = extendedLink.locatorXLinks filter { _.labelOption.isDefined }
    val resources = extendedLink.resourceXLinks filter { _.labelOption.isDefined }
    val locatorMap = locators.map(xlink => (xlink.labelOption.get -> xlink)).toMap
    val resourceMap = resources.map(xlink => (xlink.labelOption.get -> xlink)).toMap
    locatorMap ++ resourceMap
  }

  final def locatorLabelToHrefMap(extendedLink: xlink.ExtendedLink): Map[String, URI] = {
    val locators = extendedLink.locatorXLinks filter { _.labelOption.isDefined }
    val locatorHrefs = locators map { xlink => (xlink.labelOption.get -> xlink.href) }
    locatorHrefs.toMap
  }

  final def locatorLabelToLocalUriMap(extendedLink: xlink.ExtendedLink): Map[String, URI] = {
    locatorLabelToHrefMap(extendedLink) mapValues { href => localUri.resolve(href) }
  }
}

object LinkbaseDocument {

  val NS = "http://www.xbrl.org/2003/linkbase"
}
