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
import eu.cdevreeze.tqa.base.dom.XsdSchema
import eu.cdevreeze.tqa.base.taxonomy.BasicTaxonomy
import eu.cdevreeze.yaidom.core.EName

/**
 * Validator of rule 2.2.0.10. The rule says that the the schema document must not have @blockDefault, @finalDefault and
 * @version attributes.
 *
 * @author Chris de Vreeze
 */
final class Validator_2_2_0_10 extends SubTaxonomyValidator {

  private val BlockDefaultEName = EName("blockDefault")
  private val FinalDefaultEName = EName("finalDefault")
  private val VersionEName = EName("version")

  def validate(subTaxonomy: SubTaxonomy): Unit Or Every[ValidationErrorOrWarning] = {
    val xsdSchemas = subTaxonomy.asBasicTaxonomy.findAllXsdSchemas

    xsdSchemas.map(xsd => validate(xsd, subTaxonomy.backingTaxonomy)).combined.map(good => ())
  }

  private def validate(xsdRootElem: XsdSchema, backingTaxonomy: BasicTaxonomy): Unit Or Every[ValidationErrorOrWarning] = {
    val blockDefaultResult =
      if (xsdRootElem.attributeOption(BlockDefaultEName).isEmpty) {
        Good(())
      } else {
        Bad(One(ValidationError("2.2.2.10", s"Attribute blockDefault not allowed in document ${xsdRootElem.docUri}")))
      }

    val finalDefaultResult =
      if (xsdRootElem.attributeOption(FinalDefaultEName).isEmpty) {
        Good(())
      } else {
        Bad(One(ValidationError("2.2.2.10", s"Attribute finalDefault not allowed in document ${xsdRootElem.docUri}")))
      }

    val versionResult =
      if (xsdRootElem.attributeOption(VersionEName).isEmpty) {
        Good(())
      } else {
        Bad(One(ValidationError("2.2.2.10", s"Attribute version not allowed in document ${xsdRootElem.docUri}")))
      }

    List(blockDefaultResult, finalDefaultResult, versionResult).combined.map(good => ())
  }
}
