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
import eu.cdevreeze.tqa.base.dom.ConceptDeclaration
import eu.cdevreeze.tqa.base.dom.LabelLink
import eu.cdevreeze.tqa.base.dom.LinkbaseRef
import eu.cdevreeze.tqa.base.dom.TaxonomyDocument
import eu.cdevreeze.tqa.base.dom.XsdSchema

/**
 * Validator of rule 2.02.01.02. The rule says that each schema defining concepts must link to a label linkbase.
 *
 * @author Chris de Vreeze
 */
final class Validator_2_02_01_02(val excludedDocumentUris: Set[URI]) extends TaxonomyDocumentValidator {

  def ruleName: String = NtaRules.extractRuleName(getClass)

  def validateDocument(
    doc: TaxonomyDocument,
    taxonomy: Taxonomy,
    validationScope: ValidationScope): immutable.IndexedSeq[Result] = {

    require(acceptForValidation(doc, taxonomy), s"Document ${doc.uri} should not be validated")

    val xsdSchema = doc.documentElement.asInstanceOf[XsdSchema]

    val conceptDeclBuilder = new ConceptDeclaration.Builder(taxonomy.universeTaxonomy.substitutionGroupMap)
    val conceptDecls =
      xsdSchema.findAllGlobalElementDeclarations.flatMap(e => conceptDeclBuilder.optConceptDeclaration(e))

    val hasConceptDecl: Boolean = conceptDecls.size >= 1

    val hasLinkToLabelLinkbase: Boolean = {
      val linkbaseRefs = xsdSchema.findAllElemsOfType(classTag[LinkbaseRef])

      linkbaseRefs exists { linkbaseRef =>
        val linkbaseUri = linkbaseRef.resolvedHref

        taxonomy.universeTaxonomy.taxonomyBase.rootElemUriMap.get(linkbaseUri) exists { linkbase =>
          linkbase.findElemOfType(classTag[LabelLink])(_ => true).isDefined
        }
      }
    }

    if (!hasConceptDecl || hasLinkToLabelLinkbase) {
      immutable.IndexedSeq()
    } else {
      immutable.IndexedSeq(Result.makeErrorResult(
        ruleName,
        "missing-label-linkbaseref",
        s"No label link reference found in concept-declaring schema '${doc.uri}'"))
    }
  }

  def acceptForValidation(doc: TaxonomyDocument, taxonomy: Taxonomy): Boolean = {
    doc.documentElement.isInstanceOf[XsdSchema]
  }
}

object Validator_2_02_01_02 extends TaxonomyValidatorFactory {

  type Validator = Validator_2_02_01_02

  type CfgWrapper = NtaRuleConfigWrapper

  def ruleName: String = {
    NtaRules.extractRuleName(classOf[Validator_2_02_01_02])
  }

  def create(configWrapper: NtaRuleConfigWrapper): Validator_2_02_01_02 = {
    new Validator_2_02_01_02(
      configWrapper.excludedDocumentUrisForRule(ruleName))
  }
}
