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

package eu.cdevreeze.nta.common.config

import com.typesafe.config.Config

/**
 * Type-safe config wrapper contract. It is used as type-safe wrapper around a Config, and it is used to validate
 * the Config.
 *
 * The configuration is used for instantiating validators. This config wrapper should fail fast if required
 * configuration is missing.
 *
 * @author Chris de Vreeze
 */
trait ConfigWrapper {

  def underlyingConfig: Config
}
