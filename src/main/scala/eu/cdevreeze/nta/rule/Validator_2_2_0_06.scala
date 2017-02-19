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
import eu.cdevreeze.tqa.dom.XsdSchema
import eu.cdevreeze.tqa.taxonomy.BasicTaxonomy

/**
 * Validator of rule 2.2.0.06. The rule says that there must be only prefixed element nodes in the schema document.
 *
 * @author Chris de Vreeze
 */
final class Validator_2_2_0_06 extends SubTaxonomyValidator {

  def validate(subTaxonomy: SubTaxonomy): Unit Or Every[ValidationErrorOrWarning] = {
    val xsdSchemas = subTaxonomy.asBasicTaxonomy.findAllXsdSchemas

    xsdSchemas.map(xsd => validate(xsd, subTaxonomy.backingTaxonomy)).combined.map(good => ())
  }

  private def validate(xsdRootElem: XsdSchema, backingTaxonomy: BasicTaxonomy): Unit Or Every[ValidationErrorOrWarning] = {
    // This is a query on XML level, which is easy to implement using yaidom
    val unprefixedElms = xsdRootElem.filterElemsOrSelf(_.qname.prefixOption.isEmpty)

    if (unprefixedElms.isEmpty) Good(())
    else {
      val errors = unprefixedElms.map(e => ValidationError("2.2.0.06", s"Found unprefixed element ${e.qname} in document ${xsdRootElem.docUri}"))
      Bad(Every(errors.head, errors.tail: _*))
    }
  }
}
