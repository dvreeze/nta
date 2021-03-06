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

package eu.cdevreeze.nta.common.validator

import eu.cdevreeze.nta.common.config.ConfigWrapper

/**
 * Factory of a taxonomy validator.
 *
 * @author Chris de Vreeze
 */
trait TaxonomyValidatorFactory {

  type Validator <: TaxonomyValidator

  type CfgWrapper <: ConfigWrapper

  /**
   * Returns the unique rule name, such as "2.02.00.05".
   */
  def ruleName: String

  def create(configWrapper: CfgWrapper): Validator
}

object TaxonomyValidatorFactory {

  type Aux[V, C] = TaxonomyValidatorFactory {
    type Validator = V
    type CfgWrapper = C
  }
}
