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

import scala.collection.immutable

import org.scalactic.Accumulation.convertGenTraversableOnceToCombinable
import org.scalactic.Bad
import org.scalactic.Every
import org.scalactic.Good
import org.scalactic.Or

import eu.cdevreeze.nta.taxo.SubTaxonomy
import eu.cdevreeze.nta.validator.SubTaxonomyValidator
import eu.cdevreeze.nta.validator.ValidationError
import eu.cdevreeze.nta.validator.ValidationErrorOrWarning
import eu.cdevreeze.tqa.dom.TaxonomyElem
import eu.cdevreeze.tqa.dom.XsdSchema
import eu.cdevreeze.tqa.taxonomy.BasicTaxonomy
import eu.cdevreeze.yaidom.queryapi.Nodes
import eu.cdevreeze.yaidom.resolved.ResolvedNodes

/**
 * Validator of rule 2.2.0.05. The rule says that there must be at most one comment in the schema document.
 *
 * TODO This should really be a more low level validation on XML documents (with their document children such as comments),
 * instead of a sub-taxonomy validator. For now this validator only checks that the document element tree
 * has no comments anywhere.
 *
 * @author Chris de Vreeze
 */
final class Validator_2_2_0_05(val getCommentChildren: TaxonomyElem => immutable.IndexedSeq[Nodes.Comment]) extends SubTaxonomyValidator {

  def validate(subTaxonomy: SubTaxonomy): Unit Or Every[ValidationErrorOrWarning] = {
    val xsdSchemas = subTaxonomy.asBasicTaxonomy.findAllXsdSchemas

    xsdSchemas.map(xsd => validate(xsd, subTaxonomy.backingTaxonomy)).combined.map(good => ())
  }

  private def validate(xsdRootElem: XsdSchema, backingTaxonomy: BasicTaxonomy): Unit Or Every[ValidationErrorOrWarning] = {
    val offendingComments = xsdRootElem.findAllElemsOrSelf.flatMap(e => getCommentChildren(e))

    val errors =
      offendingComments.map(c => ValidationError("2.2.0.05", s"Non-allowed comment found in document element tree in document ${xsdRootElem.docUri}"))

    Every.from(errors).map(errs => Bad(errs)).getOrElse(Good(()))
  }
}
