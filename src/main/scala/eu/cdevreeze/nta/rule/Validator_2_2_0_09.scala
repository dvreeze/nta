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
import org.scalactic.One
import org.scalactic.Or

import eu.cdevreeze.nta.taxo.SubTaxonomy
import eu.cdevreeze.nta.validator.SubTaxonomyValidator
import eu.cdevreeze.nta.validator.ValidationError
import eu.cdevreeze.nta.validator.ValidationErrorOrWarning
import eu.cdevreeze.tqa.ENames.AttributeFormDefaultEName
import eu.cdevreeze.tqa.ENames.ElementFormDefaultEName
import eu.cdevreeze.tqa.base.dom.XsdSchema
import eu.cdevreeze.tqa.base.taxonomy.BasicTaxonomy

/**
 * Validator of rule 2.2.0.09. The rule says that the the schema document must have a @attributeFormDefault attribute
 * with value unqualified and a @elementFormDefault attribute with value qualified.
 *
 * @author Chris de Vreeze
 */
final class Validator_2_2_0_09 extends SubTaxonomyValidator {

  def validate(subTaxonomy: SubTaxonomy): Unit Or Every[ValidationErrorOrWarning] = {
    val xsdSchemas = subTaxonomy.asBasicTaxonomy.findAllXsdSchemas

    xsdSchemas.map(xsd => validate(xsd, subTaxonomy.backingTaxonomy)).combined.map(good => ())
  }

  private def validate(xsdRootElem: XsdSchema, backingTaxonomy: BasicTaxonomy): Unit Or Every[ValidationErrorOrWarning] = {
    val attributeFormDefaultResult =
      if (xsdRootElem.attributeOption(AttributeFormDefaultEName) == Some("unqualified")) {
        Good(())
      } else {
        Bad(One(ValidationError("2.2.2.09", s"Missing or incorrect attributeFormDefault attribute in document ${xsdRootElem.docUri}")))
      }

    val elementFormDefaultResult =
      if (xsdRootElem.attributeOption(ElementFormDefaultEName) == Some("qualified")) {
        Good(())
      } else {
        Bad(One(ValidationError("2.2.2.09", s"Missing or incorrect elementFormDefault attribute in document ${xsdRootElem.docUri}")))
      }

    List(attributeFormDefaultResult, elementFormDefaultResult).combined.map(good => ())
  }
}
