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
import org.scalactic.One
import org.scalactic.Or

import eu.cdevreeze.nta.taxo.SubTaxonomy
import eu.cdevreeze.nta.validator.SubTaxonomyValidator
import eu.cdevreeze.nta.validator.ValidationError
import eu.cdevreeze.nta.validator.ValidationErrorOrWarning
import eu.cdevreeze.tqa.base.dom.ConceptDeclaration
import eu.cdevreeze.tqa.base.dom.LabelLink
import eu.cdevreeze.tqa.base.dom.LinkbaseRef
import eu.cdevreeze.tqa.base.dom.XsdSchema
import eu.cdevreeze.tqa.base.taxonomy.BasicTaxonomy

/**
 * Validator of rule 2.2.1.02. The rule says that each schema defining concepts must link to a label linkbase.
 *
 * @author Chris de Vreeze
 */
final class Validator_2_2_1_02 extends SubTaxonomyValidator {

  def validate(subTaxonomy: SubTaxonomy): Unit Or Every[ValidationErrorOrWarning] = {
    val xsdSchemas = subTaxonomy.asBasicTaxonomy.findAllXsdSchemas

    xsdSchemas.map(xsd => validate(xsd, subTaxonomy.backingTaxonomy)).combined.map(good => ())
  }

  private def validate(xsdRootElem: XsdSchema, backingTaxonomy: BasicTaxonomy): Unit Or Every[ValidationErrorOrWarning] = {
    val conceptDeclBuilder = new ConceptDeclaration.Builder(backingTaxonomy.substitutionGroupMap)
    val conceptDecls = xsdRootElem.findAllGlobalElementDeclarations.flatMap(e => conceptDeclBuilder.optConceptDeclaration(e))

    val hasConceptDecl: Boolean = conceptDecls.size >= 1

    val hasLinkToLabelLinkbase: Boolean = {
      val linkbaseRefs = xsdRootElem.findAllElemsOfType(classTag[LinkbaseRef])

      linkbaseRefs exists { linkbaseRef =>
        val linkbaseUri = linkbaseRef.baseUri.resolve(linkbaseRef.rawHref)

        backingTaxonomy.taxonomyBase.rootElemUriMap.get(linkbaseUri) exists { linkbase =>
          linkbase.findElemOfType(classTag[LabelLink])(_ => true).isDefined
        }
      }
    }

    if (hasLinkToLabelLinkbase || !hasConceptDecl) {
      Good(())
    } else {
      assert(hasConceptDecl && !hasLinkToLabelLinkbase)

      Bad(One(ValidationError("2.2.1.02", s"There are no label linkbase refs in the schema document ${xsdRootElem.docUri}, in spite of it having at least one concept declaration")))
    }
  }
}
