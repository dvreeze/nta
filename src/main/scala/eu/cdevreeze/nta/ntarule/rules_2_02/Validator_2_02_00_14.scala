/*
 * Copyright 2011-2018 Chris de Vreeze
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

package eu.cdevreeze.nta.ntarule.rules_2_02

import java.net.URI

import scala.collection.immutable
import scala.reflect.classTag

import eu.cdevreeze.nta.common.taxonomy.Taxonomy
import eu.cdevreeze.nta.common.validator.Result
import eu.cdevreeze.nta.common.validator.TaxonomyDocumentValidator
import eu.cdevreeze.nta.common.validator.TaxonomyValidatorFactory
import eu.cdevreeze.nta.common.validator.ValidationScope
import eu.cdevreeze.nta.ntarule.NtaRuleConfigWrapper
import eu.cdevreeze.nta.ntarule.NtaRules
import eu.cdevreeze.tqa.base.dom.Annotation
import eu.cdevreeze.tqa.base.dom.Appinfo
import eu.cdevreeze.tqa.base.dom.Import
import eu.cdevreeze.tqa.base.dom.TaxonomyDocument
import eu.cdevreeze.tqa.base.dom.XsdSchema

/**
 * Validator of rule 2.02.00.14. The rule says that in the schema document xs:import elements must come directly
 * after //xs:annotation/xs:appinfo.
 *
 * @author Chris de Vreeze
 */
final class Validator_2_02_00_14(val excludedDocumentUris: Set[URI]) extends TaxonomyDocumentValidator {

  def ruleName: String = NtaRules.extractRuleName(getClass)

  def validateDocument(
    doc: TaxonomyDocument,
    taxonomy: Taxonomy,
    validationScope: ValidationScope): immutable.IndexedSeq[Result] = {

    require(isTypeOfDocumentToValidate(doc, taxonomy), s"Document ${doc.uri} should not be validated")

    val childElemsWithIndex = doc.documentElement.findAllChildElems.zipWithIndex

    val idxOfAnnotationOption: Option[Int] =
      childElemsWithIndex.collectFirst {
        case (e: Annotation, idx) if e.findChildElemOfType(classTag[Appinfo])(_ => true).isDefined => idx
      }

    val idxesOfImports: immutable.IndexedSeq[Int] =
      childElemsWithIndex.collect { case (e: Import, idx) => idx }

    val minIdxOfImportOption: Option[Int] = if (idxesOfImports.isEmpty) None else Some(idxesOfImports.min)
    val maxIdxOfImportOption: Option[Int] = if (idxesOfImports.isEmpty) None else Some(idxesOfImports.max)

    val allImportsAreDirectlyAfterEachOther: Boolean =
      childElemsWithIndex.collect {
        case (e, idx) if idx >= minIdxOfImportOption.getOrElse(-1) && idx <= maxIdxOfImportOption.getOrElse(-1) => e
      }.forall(_.isInstanceOf[Import])

    val importsAreDirectlyAfterAnnotation: Boolean =
      idxOfAnnotationOption.forall(idx => minIdxOfImportOption.forall(_ == idx + 1))

    val importSeqResultOption: Option[Result] =
      if (allImportsAreDirectlyAfterEachOther) {
        None
      } else {
        Some(Result.makeErrorResult(
          ruleName,
          "imports-not-directly-after-each-other",
          s"Imports must be directly after each other but are not in '${doc.uri}'"))
      }

    val importAfterAnnotationResultOption: Option[Result] =
      if (importsAreDirectlyAfterAnnotation) {
        None
      } else {
        Some(Result.makeErrorResult(
          ruleName,
          "imports-not-directly-after-annotation",
          s"Imports must be directly after an xs:annotation element but are not in '${doc.uri}'"))
      }

    importSeqResultOption.toIndexedSeq ++ importAfterAnnotationResultOption.toIndexedSeq
  }

  def isTypeOfDocumentToValidate(doc: TaxonomyDocument, taxonomy: Taxonomy): Boolean = {
    doc.documentElement.isInstanceOf[XsdSchema]
  }
}

object Validator_2_02_00_14 extends TaxonomyValidatorFactory {

  type Validator = Validator_2_02_00_14

  type CfgWrapper = NtaRuleConfigWrapper

  def ruleName: String = {
    NtaRules.extractRuleName(classOf[Validator_2_02_00_14])
  }

  def create(configWrapper: NtaRuleConfigWrapper): Validator_2_02_00_14 = {
    new Validator_2_02_00_14(
      configWrapper.excludedDocumentUrisForRule(ruleName))
  }
}
