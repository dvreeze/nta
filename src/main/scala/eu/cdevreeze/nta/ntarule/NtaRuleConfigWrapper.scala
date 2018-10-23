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
import com.typesafe.config.ConfigUtil
import com.typesafe.config.ConfigValue

import eu.cdevreeze.nta.common.config.ConfigWrapper
import eu.cdevreeze.yaidom.core.EName
import eu.cdevreeze.yaidom.utils.DocumentENameExtractor
import eu.cdevreeze.yaidom.utils.TextENameExtractor
import eu.cdevreeze.yaidom.queryapi.BackingElemApi
import eu.cdevreeze.nta.common.util.QNameTextENameExtractor
import eu.cdevreeze.yaidom.utils.SimpleTextENameExtractor

/**
 * The config wrapper specific for NTA rules. Everything that is checked can be relied on by the validators.
 *
 * @author Chris de Vreeze
 */
final class NtaRuleConfigWrapper(val underlyingConfig: Config) extends ConfigWrapper {
  underlyingConfig.checkValid(
    ConfigFactory.defaultReference,
    "local-language-code",
    "default-excluded-document-uris",
    "default-excluded-entrypoint-document-uris",
    "element-text-parsers",
    "attribute-value-parsers")

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
    excludedDocumentUrisForRule(ruleName, true)
  }

  def excludedDocumentUrisForRule(ruleName: String, fallbackToDefault: Boolean): Set[URI] = {
    val path = ConfigUtil.joinPath(ruleName, "excluded-document-uris")

    if (underlyingConfig.hasPath(path)) {
      underlyingConfig.getStringList(path).asScala.map(u => URI.create(u)).toSet
    } else {
      if (fallbackToDefault) defaultExcludedDocumentUris else Set.empty
    }
  }

  def excludedEntrypointDocumentUrisForRule(ruleName: String): Set[URI] = {
    excludedEntrypointDocumentUrisForRule(ruleName, true)
  }

  def excludedEntrypointDocumentUrisForRule(ruleName: String, fallbackToDefault: Boolean): Set[URI] = {
    val path = ConfigUtil.joinPath(ruleName, "excluded-entrypoint-document-uris")

    if (underlyingConfig.hasPath(path)) {
      underlyingConfig.getStringList(path).asScala.map(u => URI.create(u)).toSet
    } else {
      if (fallbackToDefault) defaultExcludedEntrypointDocumentUris else Set.empty
    }
  }

  /**
   * Mapping from element names to parsers of the element text.
   * This is only useful if the element name itself is enough to determine the element text parser.
   */
  def elementTextParsers: Map[EName, TextENameExtractor] = {
    val elementNameMap: Map[String, ConfigValue] =
      underlyingConfig.getValue("element-text-parsers").unwrapped
        .asInstanceOf[java.util.Map[String, ConfigValue]].asScala.toMap

    elementNameMap.toSeq.map(k => EName.parse(k._1) -> getTextENameExtractorByName(k._2.toString)).toMap
  }

  /**
   * Mapping from pairs of element names and attribute names to parsers of the attribute value.
   * This is only useful if the element plus attribute name itself are enough to determine the attribute value parser.
   */
  def attributeValueParsers: Map[(EName, EName), TextENameExtractor] = {
    val elementNameMap: Map[String, ConfigValue] =
      underlyingConfig.getValue("attribute-value-parsers").unwrapped
        .asInstanceOf[java.util.Map[String, ConfigValue]].asScala.toMap

    val rawResultSeq: Seq[(EName, EName, TextENameExtractor)] =
      elementNameMap.toSeq.flatMap { k =>
        val elementEName = EName.parse(k._1)

        val attrNameMap: Map[String, String] = k._2
          .asInstanceOf[java.util.Map[String, String]].asScala.toMap

        val attrNameParserMap: Map[EName, TextENameExtractor] =
          attrNameMap.toSeq.map(k => EName.parse(k._1) -> getTextENameExtractorByName(k._2)).toMap

        attrNameParserMap.toSeq.map(k => (elementEName, k._1, k._2))
      }

    rawResultSeq.groupBy(triple => (triple._1, triple._2))
      .mapValues(_.head._3)
  }

  /**
   * The DocumentENameExtractor formed by the elementTextParsers and attributeValueParsers.
   */
  def documentENameExtractor: DocumentENameExtractor = new DocumentENameExtractor {

    private val elemTextParsers = elementTextParsers

    private val attrValueParsers = attributeValueParsers

    def findElemTextENameExtractor(elem: BackingElemApi): Option[TextENameExtractor] = {
      elemTextParsers.get(elem.resolvedName)
    }

    def findAttributeValueENameExtractor(elem: BackingElemApi, attributeEName: EName): Option[TextENameExtractor] = {
      val key = (elem.resolvedName, attributeEName)
      attrValueParsers.get(key)
    }
  }

  private def getTextENameExtractorByName(name: String): TextENameExtractor = {
    name match {
      case "SimpleTextENameExtractor" => SimpleTextENameExtractor
      case "QNameTextENameExtractor" => QNameTextENameExtractor
      case _ => sys.error(s"Unknown TextENameExtractor '$name'")
    }
  }
}
