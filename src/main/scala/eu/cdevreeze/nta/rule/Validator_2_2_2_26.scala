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

import java.net.URI
import scala.collection.immutable
import eu.cdevreeze.yaidom._
import common.document.{ SchemaDocument, Taxonomy, LabelLinkbaseDocument, SubstitutionGroup }
import common.validate.{ Validator, ValidationResult }

/**
 * Validator of rule 2.2.2.26. All concepts must have a standard label in the local language (NL).
 *
 * @author Chris de Vreeze
 */
final class Validator_2_2_2_26 extends Validator[SchemaDocument, Taxonomy] {

  def validate(doc: SchemaDocument)(context: Taxonomy): ValidationResult[SchemaDocument] = {
    val conceptUris: Set[URI] = doc.conceptDeclarationsByUris(context.substitutionGroups).keySet

    var warnings = List[String]()

    val labelLinkbases: immutable.Seq[LabelLinkbaseDocument] = {
      val linkbaseRefs = doc.doc.documentElement.filterElemsOrSelf(EName("{http://www.xbrl.org/2003/linkbase}linkbaseRef"))

      val linkbases = linkbaseRefs flatMap { e =>
        val simpleXLink = xlink.SimpleLink(e)
        val href = simpleXLink.hrefOption.getOrElse(sys.error("Missing href in simple link: %s".format(e)))
        val absoluteUri = doc.localUri.resolve(href)

        val linkbaseOption = context.linkbases.get(absoluteUri)
        if (!linkbaseOption.isDefined)
          warnings = "Href to linkbase not resolved: '%s' (local URI: %s)".format(href, doc.localUri.toString) :: warnings
        linkbaseOption
      }

      linkbases collect { case linkbase: LabelLinkbaseDocument => linkbase }
    }

    def findLabelsByUris(doc: LabelLinkbaseDocument): Map[URI, xlink.Resource] = {
      val extendedLinks = doc.extendedLinks

      val result = extendedLinks.foldLeft(Map[URI, xlink.Resource]()) { (acc, xlink) =>
        val conceptLabels = doc.standardConceptLabelsByConceptUris(xlink)
        val filteredConceptLabels = conceptLabels filter {
          case (uri, res) =>
            res.wrappedElem.attributeOption(EName("http://www.w3.org/XML/1998/namespace", "lang")) == Some("nl")
        }
        acc ++ filteredConceptLabels
      }
      result
    }

    val labelsByUris: Map[URI, xlink.Resource] = labelLinkbases.foldLeft(Map[URI, xlink.Resource]()) { (acc, linkbase) =>
      acc ++ findLabelsByUris(linkbase)
    }

    val valid = conceptUris.subsetOf(labelsByUris.keySet)

    if (valid) new ValidationResult(doc, true, warnings.toIndexedSeq)
    else {
      new ValidationResult(doc, false, Vector("Some concepts have no standard label in the local language: %s".format(conceptUris.diff(labelsByUris.keySet))))
    }
  }
}
