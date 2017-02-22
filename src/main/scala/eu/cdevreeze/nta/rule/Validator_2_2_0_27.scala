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
import eu.cdevreeze.tqa.dom.LinkbaseRef
import eu.cdevreeze.tqa.dom.XsdSchema
import eu.cdevreeze.tqa.taxonomy.BasicTaxonomy

/**
 * Validator of rule 2.2.0.27. The rule says that schema must have unique linkbaseRefs.
 *
 * @author Chris de Vreeze
 */
final class Validator_2_2_0_27 extends SubTaxonomyValidator {

  def validate(subTaxonomy: SubTaxonomy): Unit Or Every[ValidationErrorOrWarning] = {
    val xsdSchemas = subTaxonomy.asBasicTaxonomy.findAllXsdSchemas

    xsdSchemas.map(xsd => validate(xsd, subTaxonomy.backingTaxonomy)).combined.map(good => ())
  }

  private def validate(xsdRootElem: XsdSchema, backingTaxonomy: BasicTaxonomy): Unit Or Every[ValidationErrorOrWarning] = {
    val linkbaseRefs = xsdRootElem.findAllElemsOfType(classTag[LinkbaseRef])

    val linkbaseRefGroups = linkbaseRefs.groupBy(e => e.baseUri.resolve(e.rawHref))

    if (linkbaseRefGroups.exists(_._2.size >= 2)) {
      Bad(Every(ValidationError("2.2.0.27", s"Duplicate linkbaseRefs in document ${xsdRootElem.docUri}")))
    } else {
      Good(())
    }
  }
}
