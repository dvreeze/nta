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
import eu.cdevreeze.tqa.ENames.XsAnnotationEName
import eu.cdevreeze.tqa.ENames.XsAppinfoEName
import eu.cdevreeze.tqa.base.dom.ArcroleRef
import eu.cdevreeze.tqa.base.dom.LinkbaseRef
import eu.cdevreeze.tqa.base.dom.RoleRef
import eu.cdevreeze.tqa.base.dom.SimpleLink
import eu.cdevreeze.tqa.base.dom.XsdSchema
import eu.cdevreeze.tqa.base.taxonomy.BasicTaxonomy

/**
 * Validator of rule 2.2.0.12. The rule says that in the schema document all link roles, arc roles and linkbase refs
 * must have "parent path" /xs:schema/xs:annotation/xs:appinfo.
 *
 * TODO Directly after root element ...
 *
 * @author Chris de Vreeze
 */
final class Validator_2_2_0_12 extends SubTaxonomyValidator {

  def validate(subTaxonomy: SubTaxonomy): Unit Or Every[ValidationErrorOrWarning] = {
    val xsdSchemas = subTaxonomy.asBasicTaxonomy.findAllXsdSchemas

    xsdSchemas.map(xsd => validate(xsd, subTaxonomy.backingTaxonomy)).combined.map(good => ())
  }

  private def validate(xsdRootElem: XsdSchema, backingTaxonomy: BasicTaxonomy): Unit Or Every[ValidationErrorOrWarning] = {
    val simpleLinks = xsdRootElem.findAllElemsOfType(classTag[SimpleLink])

    val roleRefs = simpleLinks collect { case roleRef: RoleRef => roleRef }
    val arcroleRefs = simpleLinks collect { case arcroleRef: ArcroleRef => arcroleRef }
    val linkbaseRefs = simpleLinks collect { case linkbaseRef: LinkbaseRef => linkbaseRef }

    val matchingElemPaths = (roleRefs ++ arcroleRefs ++ linkbaseRefs).map(_.backingElem.path)

    val rejectedElemPaths = matchingElemPaths filter { path =>
      assert(path.lastEntry.elementName == xsdRootElem.getElemOrSelfByPath(path).resolvedName)

      path.entries.size < 3 ||
        path.parentPath.lastEntry.elementName != XsAppinfoEName ||
        path.parentPath.parentPath.lastEntry.elementName != XsAnnotationEName
    }

    val errors =
      rejectedElemPaths map { e =>
        ValidationError(
          "2.2.0.12",
          s"Not all linkrole, arcrole and linkbase refs have 'parent path' /xs:schema/xs:annotation/xs:appinfo in document ${xsdRootElem.docUri}")
      }

    Every.from(errors).map(errs => Bad(errs)).getOrElse(Good(()))
  }
}
