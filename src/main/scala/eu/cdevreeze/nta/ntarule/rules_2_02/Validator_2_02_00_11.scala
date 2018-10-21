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

import eu.cdevreeze.nta.common.taxonomy.Taxonomy
import eu.cdevreeze.nta.common.validator.Result
import eu.cdevreeze.nta.common.validator.TaxonomyDocumentValidator
import eu.cdevreeze.nta.common.validator.TaxonomyValidatorFactory
import eu.cdevreeze.nta.common.validator.ValidationScope
import eu.cdevreeze.nta.ntarule.NtaRuleConfigWrapper
import eu.cdevreeze.nta.ntarule.NtaRules
import eu.cdevreeze.tqa.base.dom.TaxonomyDocument
import eu.cdevreeze.tqa.base.dom.XsdSchema
import eu.cdevreeze.yaidom.utils.DocumentENameExtractor
import eu.cdevreeze.yaidom.utils.NamespaceUtils

/**
 * Validator of rule 2.02.00.11. The rule says that the schema document must not declare any namespaces that are not used in the document.
 *
 * A namespace declaration for the target namespace is always allowed, even if that namespace is not used anywhere in the schema document.
 * Other than that, used namespaces are those in element names and attribute names, of course, but also those used in attributes of type xs:QName.
 *
 * Inside xs:appinfo elements we can also have additional namespaces used in element text or attribute values of type xs:QName. In particular,
 * the text of a link:usedOn attribute is of type xs:QName, so it uses a namespace.
 *
 * This validator can be tweaked to find even more used namespaces, by configuring more element text parsers and attribute value parsers,
 * which are then picked up by the DocumentENameExtractor passed to this validator. This may be needed because
 * the content of an xs:appinfo section can be anything.
 *
 * @author Chris de Vreeze
 */
final class Validator_2_02_00_11(
  val excludedDocumentUris: Set[URI],
  val documentENameExtractor: DocumentENameExtractor) extends TaxonomyDocumentValidator {

  def ruleName: String = NtaRules.extractRuleName(getClass)

  def validateDocument(
    doc: TaxonomyDocument,
    taxonomy: Taxonomy,
    validationScope: ValidationScope): immutable.IndexedSeq[Result] = {

    require(isTypeOfDocumentToValidate(doc, taxonomy), s"Document ${doc.uri} should not be validated")

    val declaredNamespaces: Set[String] =
      doc.documentElement.backingElem.findAllElemsOrSelf.flatMap(_.scope.namespaces).toSet

    val tnsOption = doc.documentElement.asInstanceOf[XsdSchema].targetNamespaceOption

    val usedNamespaces: Set[String] =
      NamespaceUtils.findAllNamespaces(doc.documentElement.backingElem, documentENameExtractor)
        .union(tnsOption.toSet)

    val unusedNamespaces: Set[String] = declaredNamespaces.diff(usedNamespaces)

    unusedNamespaces.toIndexedSeq.sorted.map { ns =>
      Result.makeErrorResult(
        ruleName,
        "unused-declared-namespace",
        s"Namespace '$ns' has been declared but is unused in schema document '${doc.uri}'")
    }
  }

  def isTypeOfDocumentToValidate(doc: TaxonomyDocument, taxonomy: Taxonomy): Boolean = {
    doc.documentElement.isInstanceOf[XsdSchema]
  }
}

object Validator_2_02_00_11 extends TaxonomyValidatorFactory {

  type Validator = Validator_2_02_00_11

  type CfgWrapper = NtaRuleConfigWrapper

  def ruleName: String = {
    NtaRules.extractRuleName(classOf[Validator_2_02_00_11])
  }

  def create(configWrapper: NtaRuleConfigWrapper): Validator_2_02_00_11 = {
    new Validator_2_02_00_11(
      configWrapper.excludedDocumentUrisForRule(ruleName),
      configWrapper.documentENameExtractor)
  }
}
