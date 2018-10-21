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

package eu.cdevreeze.nta.console

import java.io.File
import java.net.URI
import java.util.logging.Logger

import scala.collection.immutable

import com.typesafe.config.ConfigFactory

import eu.cdevreeze.nta.common.taxonomy.Taxonomy
import eu.cdevreeze.nta.common.validator.Level
import eu.cdevreeze.nta.common.validator.Result
import eu.cdevreeze.nta.common.validator.TaxonomyValidator
import eu.cdevreeze.nta.common.validator.TaxonomyValidatorFactory
import eu.cdevreeze.nta.common.validator.ValidationScope
import eu.cdevreeze.nta.ntarule.NtaRuleConfigWrapper
import eu.cdevreeze.nta.ntarule.NtaRuleConfigWrapperFactory
import eu.cdevreeze.nta.ntarule.rules_2_02._
import eu.cdevreeze.tqa.base.relationship.DefaultRelationshipFactory
import eu.cdevreeze.tqa.base.taxonomybuilder.DefaultDtsCollector
import eu.cdevreeze.tqa.base.taxonomybuilder.DocumentCollector
import eu.cdevreeze.tqa.base.taxonomybuilder.TaxonomyBuilder
import eu.cdevreeze.tqa.docbuilder.DocumentBuilder
import eu.cdevreeze.tqa.docbuilder.indexed.IndexedDocumentBuilder
import eu.cdevreeze.tqa.docbuilder.jvm.UriResolvers
import eu.cdevreeze.tqa.docbuilder.saxon.SaxonDocumentBuilder
import eu.cdevreeze.yaidom.parse.DocumentParserUsingStax
import net.sf.saxon.s9api.Processor

/**
 * NTA rule runner.
 *
 * @author Chris de Vreeze
 */
object ValidatorRunner {

  private val logger = Logger.getGlobal

  private val validatorFactoryMap: Map[String, TaxonomyValidatorFactory.Aux[_, NtaRuleConfigWrapper]] =
    List[TaxonomyValidatorFactory.Aux[_, NtaRuleConfigWrapper]](
      Validator_2_02_00_05,
      Validator_2_02_00_06,
      Validator_2_02_00_08,
      Validator_2_02_00_09,
      Validator_2_02_00_10,
      Validator_2_02_00_18,
      Validator_2_02_00_19,
      Validator_2_02_00_23,
      Validator_2_02_00_27,
      Validator_2_02_00_28,
      Validator_2_02_01_02,
      Validator_2_02_02_26).groupBy(_.ruleName).mapValues(_.head)

  def main(args: Array[String]): Unit = {
    require(args.size >= 3, s"Usage: ValidatorRunner <taxo root dir> <document URI start (validation scope)> <entrypoint URI> ...")
    val rootDir = new File(args(0))
    require(rootDir.isDirectory, s"Not a directory: $rootDir")

    val documentUriStart = URI.create(args(1))
    val entrypointUris = args.drop(2).map(u => URI.create(u)).toSet

    val useSaxon = System.getProperty("useSaxon", "false").toBoolean

    val documentBuilder = getDocumentBuilder(useSaxon, rootDir)
    val documentCollector: DocumentCollector = DefaultDtsCollector()

    val lenient = System.getProperty("lenient", "false").toBoolean

    val relationshipFactory =
      if (lenient) DefaultRelationshipFactory.LenientInstance else DefaultRelationshipFactory.StrictInstance

    val taxoBuilder =
      TaxonomyBuilder.
        withDocumentBuilder(documentBuilder).
        withDocumentCollector(documentCollector).
        withRelationshipFactory(relationshipFactory)

    logger.info(s"Starting building the DTS with entrypoint(s) ${entrypointUris.mkString(", ")}")

    val basicTaxo = taxoBuilder.build(entrypointUris)

    val taxonomy = Taxonomy.build(basicTaxo, documentCollector, entrypointUris.map(u => Set(u)))

    val validationScope = new ValidationScope(Set(documentUriStart.toString))

    logger.info(s"Creating the validators ...")

    val config = ConfigFactory.defaultApplication()
    val configWrapper: NtaRuleConfigWrapper = NtaRuleConfigWrapperFactory.create(config)

    val validators: immutable.IndexedSeq[TaxonomyValidator] =
      validatorFactoryMap.values.toIndexedSeq
        .map(factory => factory.create(configWrapper).asInstanceOf[TaxonomyValidator])
        .sortBy(_.ruleName)

    logger.info("Starting rule validation ...")

    val validationResults: immutable.IndexedSeq[Result] = validators.flatMap { validator =>
      logger.info(s"Running validator for rule ${validator.ruleName}")

      validator.validate(validationScope, taxonomy)
    }

    logger.info(s"Validation result is OK: ${validationResults.filterNot(_.level == Level.Ok).isEmpty}")

    val errors = validationResults.filter(_.level == Level.Error)
    val warnings = validationResults.filter(_.level == Level.Warning)

    errors foreach { err =>
      logger.severe(err.toString)
    }

    warnings foreach { warn =>
      logger.warning(warn.toString)
    }
  }

  private def getDocumentBuilder(useSaxon: Boolean, rootDir: File): DocumentBuilder = {
    if (useSaxon) {
      val processor = new Processor(false)

      new SaxonDocumentBuilder(processor.newDocumentBuilder(), UriResolvers.fromLocalMirrorRootDirectory(rootDir))
    } else {
      new IndexedDocumentBuilder(DocumentParserUsingStax.newInstance(), UriResolvers.fromLocalMirrorRootDirectory(rootDir))
    }
  }
}
