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
import eu.cdevreeze.tqa.base.dom.Annotation
import eu.cdevreeze.tqa.base.dom.Appinfo
import eu.cdevreeze.tqa.base.dom.Import
import eu.cdevreeze.tqa.base.dom.XsdSchema
import eu.cdevreeze.tqa.base.taxonomy.BasicTaxonomy

/**
 * Validator of rule 2.2.0.14. The rule says that in the schema document xs:import elements must come directly after //xs:annotation/xs:appinfo.
 *
 * @author Chris de Vreeze
 */
final class Validator_2_2_0_14 extends SubTaxonomyValidator {

  def validate(subTaxonomy: SubTaxonomy): Unit Or Every[ValidationErrorOrWarning] = {
    val xsdSchemas = subTaxonomy.asBasicTaxonomy.findAllXsdSchemas

    xsdSchemas.map(xsd => validate(xsd, subTaxonomy.backingTaxonomy)).combined.map(good => ())
  }

  private def validate(xsdRootElem: XsdSchema, backingTaxonomy: BasicTaxonomy): Unit Or Every[ValidationErrorOrWarning] = {
    val childElemsWithIndex = xsdRootElem.findAllChildElems.zipWithIndex

    val idxOfAnnotationOption =
      childElemsWithIndex collectFirst { case (e: Annotation, idx) if e.findChildElemOfType(classTag[Appinfo])(_ => true).isDefined => idx }
    val idxesOfImports =
      childElemsWithIndex collect { case (e: Import, idx) => idx }

    val minIdxOfImportOption = if (idxesOfImports.isEmpty) None else Some(idxesOfImports.min)
    val maxIdxOfImportOption = if (idxesOfImports.isEmpty) None else Some(idxesOfImports.max)

    val importsAfterEachOtherResult =
      if (childElemsWithIndex.collect({ case (e, idx) if idx >= minIdxOfImportOption.getOrElse(-1) && idx <= maxIdxOfImportOption.getOrElse(-1) => e }).forall(_.isInstanceOf[Import])) {
        Good(())
      } else {
        Bad(One(ValidationError("2.2.0.14", s"Not all xs:import elements are directly after each other in document ${xsdRootElem.docUri}")))
      }

    val importsAfterAnnotationResult =
      if (idxOfAnnotationOption.forall(idx => minIdxOfImportOption.forall(_ == idx + 1))) {
        Good(())
      } else {
        Bad(One(ValidationError("2.2.0.14", s"The xs:import elements are not directly after //xs:annotation/xs:appinfo in ${xsdRootElem.docUri}")))
      }

    List(importsAfterEachOtherResult, importsAfterAnnotationResult).combined.map(good => ())
  }
}
