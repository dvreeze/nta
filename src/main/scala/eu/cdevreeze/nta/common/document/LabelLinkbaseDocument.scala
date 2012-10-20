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

/**
 * XBRL label linkbase document.
 *
 * @author Chris de Vreeze
 */
final class LabelLinkbaseDocument(
  override val originalUri: URI,
  override val localUri: URI,
  override val doc: Document) extends LinkbaseDocument(originalUri, localUri, doc) {

  // TODO Check element names for XLinks

  /**
   * Returns label resource XLinks per concept URI (pointing to an ID within a schema document), given an extended link (which
   * contains the necessary arcs, locators to concepts, and the searched for label resource links).
   *
   * The resulting Map can be matched with the resulting Map from a call to `SchemaDocument.topLevelElementDeclarationsByUris`,
   * thus relating concepts to labels.
   *
   * This is an expensive method, so it should certainly not be called many times in succession.
   */
  def conceptLabelsByConceptUris(extendedLink: xlink.ExtendedLink): Map[URI, xlink.Resource] = {
    val labelsToXLinks: Map[String, xlink.XLink] = labelToXLinkMap(extendedLink)
    val locatorLabelsToLocalUris: Map[String, URI] = locatorLabelToLocalUriMap(extendedLink)

    val arcs = extendedLink.arcXLinks filter { arc => arc.arcroleOption == Some("http://www.xbrl.org/2003/arcrole/concept-label") }

    val arcFromToTuples: Seq[(xlink.Arc, xlink.Locator, xlink.Resource)] = arcs map { arc: xlink.Arc =>
      require(arc.fromOption.isDefined, "Expected 'from' in arc")
      require(arc.toOption.isDefined, "Expected 'to' in arc")
      val from = labelsToXLinks.getOrElse(
        arc.fromOption.get,
        sys.error("Could not resolve label '%s'".format(arc.fromOption)))
      val to = labelsToXLinks.getOrElse(
        arc.toOption.get,
        sys.error("Could not resolve label '%s'".format(arc.toOption)))

      require(from.isInstanceOf[xlink.Locator], "Expected 'from' to be locator")
      require(to.isInstanceOf[xlink.Resource], "Expected 'to' to be resource")

      (arc, from.asInstanceOf[xlink.Locator], to.asInstanceOf[xlink.Resource])
    }

    val result = arcFromToTuples map {
      case (arc, fromLoc, toRes) =>
        val uri = locatorLabelsToLocalUris.getOrElse(
          fromLoc.labelOption.get,
          sys.error("Could not find locator with label '%s'".format(fromLoc.labelOption)))
        (uri -> toRes)
    }
    result.toMap
  }
}
