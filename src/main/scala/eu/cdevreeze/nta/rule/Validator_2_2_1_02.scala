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
 * Validator of rule 2.2.1.02. The rule says that each schema defining concepts must link to a label linkbase.
 *
 * @author Chris de Vreeze
 */
final class Validator_2_2_1_02 extends Validator[SchemaDocument, Taxonomy] {

  def validate(doc: SchemaDocument)(context: Taxonomy): ValidationResult[SchemaDocument] = {
    // Assuming a "useful" taxonomy, with all needed substitution groups
    val hasConcepts = definesConcepts(doc, context.substitutionGroups)

    val labelsFoundIfApplicable =
      if (!hasConcepts) true
      else {
        val linkbaseRefs = doc.doc.documentElement.filterElemsOrSelf(EName("{http://www.xbrl.org/2003/linkbase}linkbaseRef"))

        val labelLinkbaseRefs = linkbaseRefs filter { e =>
          val simpleXLink = xlink.SimpleLink(e)
          val href = simpleXLink.hrefOption.getOrElse(sys.error("Missing href in simple link: %s".format(e)))
          val absoluteUri = doc.localUri.resolve(href)

          val linkbaseOption = context.linkbases.get(absoluteUri)
          linkbaseOption.isDefined && linkbaseOption.get.isInstanceOf[LabelLinkbaseDocument]
        }
        !labelLinkbaseRefs.isEmpty
      }

    if (labelsFoundIfApplicable) ValidationResult.validResult(doc)
    else {
      new ValidationResult(doc, false, Vector("There are no label linkbase refs in the schema document"))
    }
  }

  private def definesConcepts(doc: SchemaDocument, conceptSubstGroups: Set[SubstitutionGroup]): Boolean = {
    !doc.conceptDeclarations(conceptSubstGroups).isEmpty
  }
}
