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
import eu.cdevreeze.tqa.ENames.ReferEName
import eu.cdevreeze.tqa.ENames.TypeEName
import eu.cdevreeze.tqa.ENames.XsKeyrefEName
import eu.cdevreeze.tqa.Namespaces.LinkNamespace
import eu.cdevreeze.tqa.base.dom.AttributeDeclaration
import eu.cdevreeze.tqa.base.dom.ElementDeclaration
import eu.cdevreeze.tqa.base.dom.Reference
import eu.cdevreeze.tqa.base.dom.RestrictionOrExtension
import eu.cdevreeze.tqa.base.dom.XsdSchema
import eu.cdevreeze.tqa.base.taxonomy.BasicTaxonomy
import eu.cdevreeze.yaidom.core.EName
import eu.cdevreeze.yaidom.queryapi.ScopedElemApi

/**
 * Validator of rule 2.2.0.11. The rule says that the schema document must not declare any namespaces that are not used in the document.
 *
 * A namespace declaration for the target namespace is always allowed, even if that namespace is not used anywhere in the schema document.
 * Other than that, used namespaces are those in element names and attribute names, of course, but also those used in attributes of type xs:QName.
 *
 * Inside xs:appinfo elements we can also have additional namespaces used in element text or attribute values of type xs:QName. In particular,
 * the text of a link:usedOn attribute is of type xs:QName, so it uses a namespace.
 *
 * This validator can be tweaked to find even more used namespaces, by passing as constructor argument a function that given an element may find more
 * namespaces in its attribute values or element text than those that are already found. This may be needed because the content of an xs:appinfo section
 * can be anything.
 *
 * @author Chris de Vreeze
 */
final class Validator_2_2_0_11(val findExtraNamespaces: ScopedElemApi => Set[String]) extends SubTaxonomyValidator {

  def this() = {
    this(_ => Set())
  }

  private val LinkUsedOnEName = EName(LinkNamespace, "usedOn")

  def validate(subTaxonomy: SubTaxonomy): Unit Or Every[ValidationErrorOrWarning] = {
    val xsdSchemas = subTaxonomy.asBasicTaxonomy.findAllXsdSchemas

    xsdSchemas.map(xsd => validate(xsd, subTaxonomy.backingTaxonomy)).combined.map(good => ())
  }

  private def validate(xsdRootElem: XsdSchema, backingTaxonomy: BasicTaxonomy): Unit Or Every[ValidationErrorOrWarning] = {
    val allElemsOrSelf = xsdRootElem.findAllElemsOrSelf

    val namespacesInElemNames = allElemsOrSelf.flatMap(_.resolvedName.namespaceUriOption).toSet
    val namespacesInAttrNames = allElemsOrSelf.flatMap(_.resolvedAttributes).flatMap(_._1.namespaceUriOption).toSet

    val extraNamespaces = allElemsOrSelf.flatMap(e => findExtraNamespaces(e)).toSet

    val allowedNamespaces =
      namespacesInElemNames.
        union(namespacesInAttrNames).
        union(tnsOption(xsdRootElem).toSet).
        union(refAttributeNamespaces(xsdRootElem)).
        union(typeAttributeNamespaces(xsdRootElem)).
        union(substitutionGroupAttributeNamespaces(xsdRootElem)).
        union(baseAttributeNamespaces(xsdRootElem)).
        union(referAttributeNamespaces(xsdRootElem)).
        union(usedOnTextNamespaces(xsdRootElem)).
        union(extraNamespaces)

    val declaredNamespaces = allElemsOrSelf.flatMap(_.scope.prefixNamespaceMap.values).toSet

    val offendingNamespaces = declaredNamespaces.diff(allowedNamespaces)

    val errors =
      offendingNamespaces.toSeq.sorted.map(ns => ValidationError("2.2.0.11", s"Found declared but unused namespace ${ns} in document ${xsdRootElem.docUri}"))

    Every.from(errors).map(errs => Bad(errs)).getOrElse(Good(()))
  }

  private def tnsOption(xsdRootElem: XsdSchema): Option[String] = {
    xsdRootElem.targetNamespaceOption
  }

  private def refAttributeNamespaces(xsdRootElem: XsdSchema): Set[String] = {
    xsdRootElem.findAllElemsOfType(classTag[Reference]).map(_.ref).flatMap(_.namespaceUriOption).toSet
  }

  private def typeAttributeNamespaces(xsdRootElem: XsdSchema): Set[String] = {
    val decls =
      xsdRootElem.findAllElemsOfType(classTag[ElementDeclaration]) ++ xsdRootElem.findAllElemsOfType(classTag[AttributeDeclaration])

    decls.flatMap(_.attributeAsResolvedQNameOption(TypeEName)).flatMap(_.namespaceUriOption).toSet
  }

  private def substitutionGroupAttributeNamespaces(xsdRootElem: XsdSchema): Set[String] = {
    xsdRootElem.findAllGlobalElementDeclarations.flatMap(_.substitutionGroupOption).flatMap(_.namespaceUriOption).toSet
  }

  private def baseAttributeNamespaces(xsdRootElem: XsdSchema): Set[String] = {
    xsdRootElem.findAllElemsOfType(classTag[RestrictionOrExtension]).flatMap(_.baseTypeOption).flatMap(_.namespaceUriOption).toSet
  }

  private def referAttributeNamespaces(xsdRootElem: XsdSchema): Set[String] = {
    xsdRootElem.filterElems(_.resolvedName == XsKeyrefEName).flatMap(_.attributeAsResolvedQNameOption(ReferEName)).flatMap(_.namespaceUriOption).toSet
  }

  private def usedOnTextNamespaces(xsdRootElem: XsdSchema): Set[String] = {
    xsdRootElem.filterElems(_.resolvedName == LinkUsedOnEName).map(_.textAsResolvedQName).flatMap(_.namespaceUriOption).toSet
  }
}
