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

import com.typesafe.config.Config

import eu.cdevreeze.nta.common.config.ConfigWrapper

/**
 * The config wrapper specific for NTA rules. Everything that is checked can be relied on by the validators.
 *
 * @author Chris de Vreeze
 */
final class NtaRuleConfigWrapper(val underlyingConfig: Config) extends ConfigWrapper {
  checkConfig(underlyingConfig)

  def localLanguageCode: String = {
    ???
  }

  private def checkConfig(config: Config): Unit = {
    // TODO
  }
}
