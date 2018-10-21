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
import eu.cdevreeze.tqa.ENames
import eu.cdevreeze.tqa.base.dom.ArcroleRef
import eu.cdevreeze.tqa.base.dom.LinkbaseRef
import eu.cdevreeze.tqa.base.dom.RoleRef
import eu.cdevreeze.tqa.base.dom.SimpleLink
import eu.cdevreeze.tqa.base.dom.TaxonomyDocument
import eu.cdevreeze.tqa.base.dom.XsdSchema
import eu.cdevreeze.yaidom.queryapi.BackingNodes

/**
 * Validator of rule 2.02.00.12. The rule says that in the schema document all link roles, arc roles and linkbase refs
 * must have "parent path" /xs:schema/xs:annotation/xs:appinfo, and this annotation must be the first child element
 * of the root element.
 *
 * @author Chris de Vreeze
 */
final class Validator_2_02_00_12(val excludedDocumentUris: Set[URI]) extends TaxonomyDocumentValidator {

  def ruleName: String = NtaRules.extractRuleName(getClass)

  def validateDocument(
    doc: TaxonomyDocument,
    taxonomy: Taxonomy,
    validationScope: ValidationScope): immutable.IndexedSeq[Result] = {

    require(isTypeOfDocumentToValidate(doc, taxonomy), s"Document ${doc.uri} should not be validated")

    val simpleLinks = doc.documentElement.findAllElemsOfType(classTag[SimpleLink])

    val roleRefs = simpleLinks collect { case roleRef: RoleRef => roleRef }
    val arcroleRefs = simpleLinks collect { case arcroleRef: ArcroleRef => arcroleRef }
    val linkbaseRefs = simpleLinks collect { case linkbaseRef: LinkbaseRef => linkbaseRef }

    val simpleLinksUnderConsideration: immutable.IndexedSeq[SimpleLink] = roleRefs ++ arcroleRefs ++ linkbaseRefs

    val expectedAncestorENames = List(ENames.XsAppinfoEName, ENames.XsAnnotationEName, ENames.XsSchemaEName)

    val simpleLinksNotInAppinfo =
      simpleLinksUnderConsideration.filter(_.backingElem.ancestors.map(_.resolvedName) != expectedAncestorENames)

    val annotationAncestors: immutable.IndexedSeq[BackingNodes.Elem] =
      simpleLinksUnderConsideration
        .flatMap(_.backingElem.asInstanceOf[BackingNodes.Elem].findAncestor(_.resolvedName == ENames.XsAnnotationEName))
        .distinct

    val firstChildElemOption = doc.documentElement.findAllChildElems.headOption

    val violatingAnnotationAncestors =
      annotationAncestors.filter(e => e.path != firstChildElemOption.get.backingElem.path)

    val wrongAncestryResultOption: Option[Result] =
      if (simpleLinksNotInAppinfo.isEmpty) {
        None
      } else {
        Some(Result.makeErrorResult(
          ruleName,
          "simple-link-having-unexpected-ancestry",
          s"At least one of the roleRefs, arcroleRefs and linkbaseRefs do not have 'parent path' /xs:schema/xs:annotation/xs:appinfo in '${doc.uri}'"))
      }

    val notWithinFirstChildElementResultOption: Option[Result] =
      if (violatingAnnotationAncestors.isEmpty) {
        None
      } else {
        Some(Result.makeErrorResult(
          ruleName,
          "simple-link-not-in-annotation-that-is-first-child",
          s"At least one of the roleRefs, arcroleRefs and linkbaseRefs are not in the annotation that is the first root child element in '${doc.uri}'"))
      }

    wrongAncestryResultOption.toIndexedSeq ++ notWithinFirstChildElementResultOption.toIndexedSeq
  }

  def isTypeOfDocumentToValidate(doc: TaxonomyDocument, taxonomy: Taxonomy): Boolean = {
    doc.documentElement.isInstanceOf[XsdSchema]
  }
}

object Validator_2_02_00_12 extends TaxonomyValidatorFactory {

  type Validator = Validator_2_02_00_12

  type CfgWrapper = NtaRuleConfigWrapper

  def ruleName: String = {
    NtaRules.extractRuleName(classOf[Validator_2_02_00_12])
  }

  def create(configWrapper: NtaRuleConfigWrapper): Validator_2_02_00_12 = {
    new Validator_2_02_00_12(
      configWrapper.excludedDocumentUrisForRule(ruleName))
  }
}
