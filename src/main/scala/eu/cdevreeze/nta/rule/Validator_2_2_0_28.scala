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

import org.scalactic.Bad
import org.scalactic.Every
import org.scalactic.Good
import org.scalactic.Or

import eu.cdevreeze.nta.validator.DtsValidator
import eu.cdevreeze.nta.validator.ValidationError
import eu.cdevreeze.nta.validator.ValidationErrorOrWarning
import eu.cdevreeze.tqa.base.taxonomy.BasicTaxonomy

/**
 * Validator of rule 2.2.0.28. The rule says that each DTS must have no arcs with the same ELR, source
 * and target.
 *
 * @author Chris de Vreeze
 */
final class Validator_2_2_0_28 extends DtsValidator {

  def validate(dts: BasicTaxonomy): Unit Or Every[ValidationErrorOrWarning] = {
    val relationships = dts.relationships

    val relationshipGroups = relationships.groupBy(rel => (rel.elr, rel.sourceElem.key, rel.targetElem.key))

    val offendingRelationshipGroups = relationshipGroups.filter(_._2.size >= 2)

    val errors =
      offendingRelationshipGroups.toSeq map {
        case ((elr, sourceKey, targetKey), relationshipGroup) =>
          val docUris = relationshipGroup.map(_.docUri).distinct
          val msg = s"There are multiple arcs with ELR $elr, source $sourceKey and target $targetKey. Documents: ${docUris.mkString(", ")}"

          ValidationError("2.2.0.28", msg)
      }

    Every.from(errors).map(errs => Bad(errs)).getOrElse(Good(()))
  }
}
