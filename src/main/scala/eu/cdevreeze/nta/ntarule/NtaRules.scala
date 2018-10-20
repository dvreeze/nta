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

package eu.cdevreeze.nta.ntarule

import eu.cdevreeze.nta.common.validator.TaxonomyValidator

/**
 * Utility object with commonly used functions by NTA validators.
 *
 * @author Chris de Vreeze
 */
object NtaRules {

  /**
   * Extracts the rule name from the validator class. If the simple class name does not start with "Validator_",
   * an exception is thrown. Otherwise this prefix is stripped, and on the remainder the underscores are replaced
   * by dots. For example "Validator_2_02_00_05" becomes "2.02.00.05".
   */
  def extractRuleName(validatorClass: Class[_ <: TaxonomyValidator]): String = {
    val simpleName = validatorClass.getSimpleName
    require(simpleName.startsWith(validatorNamePrefix), s"Cannot extract the rule name from '$simpleName'")

    simpleName.stripPrefix(validatorNamePrefix).replace("_", ".")
  }

  private val validatorNamePrefix = "Validator_"
}
