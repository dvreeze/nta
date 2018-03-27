/*
 * Copyright 2011-2017 Chris de Vreeze
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

package eu.cdevreeze.nta.rule

import scala.reflect.classTag

import org.scalactic.Accumulation.convertGenTraversableOnceToCombinable
import org.scalactic.Bad
import org.scalactic.Every
import org.scalactic.Good
import org.scalactic.Or

import eu.cdevreeze.nta.taxo.SubTaxonomy
import eu.cdevreeze.nta.validator.SubTaxonomyValidator
import eu.cdevreeze.nta.validator.ValidationError
import eu.cdevreeze.nta.validator.ValidationErrorOrWarning
import eu.cdevreeze.tqa.base.dom.ConceptDeclaration
import eu.cdevreeze.tqa.base.dom.XsdSchema
import eu.cdevreeze.tqa.base.relationship.ConceptLabelRelationship
import eu.cdevreeze.tqa.base.taxonomy.BasicTaxonomy
import eu.cdevreeze.yaidom.core.EName

/**
 * Validator of rule 2.2.2.26. All concepts must have a standard label in the local language (NL).
 *
 * @author Chris de Vreeze
 */
final class Validator_2_2_2_26(val languageCode: String) extends SubTaxonomyValidator {

  private val defaultResourceRole = "http://www.xbrl.org/2003/role/label"

  def validate(subTaxonomy: SubTaxonomy): Unit Or Every[ValidationErrorOrWarning] = {
    val xsdSchemas = subTaxonomy.asBasicTaxonomy.findAllXsdSchemas

    xsdSchemas.map(xsd => validate(xsd, subTaxonomy.backingTaxonomy)).combined.map(good => ())
  }

  private def validate(xsdRootElem: XsdSchema, backingTaxonomy: BasicTaxonomy): Unit Or Every[ValidationErrorOrWarning] = {
    val conceptDeclBuilder = new ConceptDeclaration.Builder(backingTaxonomy.substitutionGroupMap)
    val conceptDecls = xsdRootElem.findAllGlobalElementDeclarations.flatMap(e => conceptDeclBuilder.optConceptDeclaration(e))

    val offendingConcepts: Set[EName] = conceptDecls.map(_.targetEName).toSet filter { conceptEName =>
      val conceptLabels = backingTaxonomy.filterOutgoingStandardRelationshipsOfType(conceptEName, classTag[ConceptLabelRelationship]) { rel =>
        (rel.language == languageCode) && (rel.resourceRole == defaultResourceRole)
      }

      conceptLabels.isEmpty
    }

    val errors =
      offendingConcepts.toSeq.map(concept => ValidationError("2.2.2.26", s"Found concept ${concept} without standard label in the local language in document ${xsdRootElem.docUri}"))

    Every.from(errors).map(errs => Bad(errs)).getOrElse(Good(()))
  }
}
