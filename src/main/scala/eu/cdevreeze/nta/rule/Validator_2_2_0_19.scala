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

import org.scalactic.Accumulation.convertGenTraversableOnceToCombinable
import org.scalactic.Bad
import org.scalactic.Every
import org.scalactic.Good
import org.scalactic.Or

import eu.cdevreeze.nta.taxo.SubTaxonomy
import eu.cdevreeze.nta.validator.SubTaxonomyValidator
import eu.cdevreeze.nta.validator.ValidationError
import eu.cdevreeze.nta.validator.ValidationErrorOrWarning
import eu.cdevreeze.tqa.base.dom.XsdSchema
import eu.cdevreeze.tqa.base.taxonomy.BasicTaxonomy

/**
 * Validator of rule 2.2.0.19. The rule says that in the schema document all namespace declarations must occur only in
 * the root element.
 *
 * Implementation note: we can only check that "significant" namespace declarations occur only in the root
 * element, because "superfluous" namespace declarations are lost after parsing.
 *
 * @author Chris de Vreeze
 */
final class Validator_2_2_0_19 extends SubTaxonomyValidator {

  def validate(subTaxonomy: SubTaxonomy): Unit Or Every[ValidationErrorOrWarning] = {
    val xsdSchemas = subTaxonomy.asBasicTaxonomy.findAllXsdSchemas

    xsdSchemas.map(xsd => validate(xsd, subTaxonomy.backingTaxonomy)).combined.map(good => ())
  }

  private def validate(xsdRootElem: XsdSchema, backingTaxonomy: BasicTaxonomy): Unit Or Every[ValidationErrorOrWarning] = {
    val invalidElems = xsdRootElem.filterElems(e => e.scope != e.backingElem.parent.scope)

    val errors =
      invalidElems.map(e => ValidationError("2.2.0.19", s"Found namespace (un)declarations in an ${e.resolvedName} element in document ${xsdRootElem.docUri}"))

    Every.from(errors).map(errs => Bad(errs)).getOrElse(Good(()))
  }
}
