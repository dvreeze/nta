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

import java.net.URI

import scala.collection.JavaConverters._

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory

import eu.cdevreeze.nta.common.config.ConfigWrapper

/**
 * The config wrapper specific for NTA rules. Everything that is checked can be relied on by the validators.
 *
 * @author Chris de Vreeze
 */
final class NtaRuleConfigWrapper(val underlyingConfig: Config) extends ConfigWrapper {
  underlyingConfig.checkValid(ConfigFactory.defaultReference)

  def localLanguageCode: String = {
    underlyingConfig.getString("local-language-code")
  }

  def defaultExcludedDocumentUris: Set[URI] = {
    underlyingConfig.getStringList("default-excluded-document-uris").asScala
      .map(u => URI.create(u)).toSet
  }

  def defaultExcludedEntrypointDocumentUris: Set[URI] = {
    underlyingConfig.getStringList("default-excluded-entrypoint-document-uris").asScala
      .map(u => URI.create(u)).toSet
  }

  def excludedDocumentUrisForRule(ruleName: String): Set[URI] = {
    val path = s"$ruleName.excluded-document-uris"

    if (underlyingConfig.hasPath(path)) {
      underlyingConfig.getStringList(path).asScala.map(u => URI.create(u)).toSet
    } else {
      Set.empty
    }
  }

  def excludedEntrypointDocumentUrisForRule(ruleName: String): Set[URI] = {
    val path = s"$ruleName.excluded-entrypoint-document-uris"

    if (underlyingConfig.hasPath(path)) {
      underlyingConfig.getStringList(path).asScala.map(u => URI.create(u)).toSet
    } else {
      Set.empty
    }
  }
}
