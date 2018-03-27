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

import org.scalactic.Accumulation.convertGenTraversableOnceToCombinable
import org.scalactic.Every
import org.scalactic.Or
import eu.cdevreeze.nta.rule._
import eu.cdevreeze.nta.taxo.SubTaxonomy
import eu.cdevreeze.nta.validator.DtsValidator
import eu.cdevreeze.nta.validator.SubTaxonomyValidator
import eu.cdevreeze.nta.validator.ValidationError
import eu.cdevreeze.nta.validator.ValidationErrorOrWarning
import eu.cdevreeze.nta.validator.ValidationWarning
import eu.cdevreeze.tqa.base.relationship.DefaultRelationshipFactory
import eu.cdevreeze.tqa.base.taxonomy.BasicTaxonomy
import eu.cdevreeze.tqa.base.taxonomybuilder.DefaultDtsCollector
import eu.cdevreeze.tqa.base.taxonomybuilder.DocumentCollector
import eu.cdevreeze.tqa.base.taxonomybuilder.TaxonomyBuilder
import eu.cdevreeze.tqa.docbuilder.DocumentBuilder
import eu.cdevreeze.tqa.docbuilder.indexed.IndexedDocumentBuilder
import eu.cdevreeze.tqa.docbuilder.jvm.UriResolvers
import eu.cdevreeze.tqa.docbuilder.saxon.SaxonDocumentBuilder
import eu.cdevreeze.yaidom.parse.DocumentParserUsingStax
import eu.cdevreeze.yaidom.queryapi.BackingDocumentApi
import net.sf.saxon.s9api.Processor

/**
 * NTA rule runner.
 *
 * @author Chris de Vreeze
 */
object ValidatorRunner {

  private val logger = Logger.getGlobal

  private val languageCode = System.getProperty("localLanguageCode", "nl")

  private def ruleValidatorMap(entrypoints: Set[URI]): Map[String, SubTaxonomyValidator] = {
    // TODO Type class for enriching BackingElemApi instead of function getCommentChildren below.
    Map(
      "2.2.0.05" -> new Validator_2_2_0_05,
      "2.2.0.06" -> new Validator_2_2_0_06,
      "2.2.0.08" -> new Validator_2_2_0_08,
      "2.2.0.09" -> new Validator_2_2_0_09,
      "2.2.0.10" -> new Validator_2_2_0_10,
      "2.2.0.11" -> new Validator_2_2_0_11,
      "2.2.0.12" -> new Validator_2_2_0_12,
      "2.2.0.14" -> new Validator_2_2_0_14,
      "2.2.0.18" -> new Validator_2_2_0_18,
      "2.2.0.19" -> new Validator_2_2_0_19,
      "2.2.0.22" -> new Validator_2_2_0_22,
      "2.2.0.23" -> new Validator_2_2_0_23(entrypoints),
      "2.2.0.27" -> new Validator_2_2_0_27,
      "2.2.1.02" -> new Validator_2_2_1_02,
      "2.2.2.26" -> new Validator_2_2_2_26(languageCode))
  }

  private val dtsRuleValidatorMap: Map[String, DtsValidator] = {
    Map(
      "2.2.0.28" -> new Validator_2_2_0_28)
  }

  def main(args: Array[String]): Unit = {
    require(args.size >= 3, s"Usage: ValidatorRunner <taxo root dir> <sub-taxo document URI start> <entrypoint URI> ...")
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

    val subTaxo = new SubTaxonomy(basicTaxo, (uri => uri.toString.startsWith(documentUriStart.toString)))

    logger.info("Starting rule validation ...")

    val normalValidationResult: Unit Or Every[ValidationErrorOrWarning] =
      (ruleValidatorMap(entrypointUris).values.toIndexedSeq.sortBy(_.getClass.getSimpleName) map { validator =>
        logger.info(s"Running validator ${validator.getClass.getSimpleName}")
        validator.validate(subTaxo)
      }).combined.map(good => ())

    logger.info(s"Validation result for regular rules is OK: ${normalValidationResult.isGood}")

    val dtsValidationResult: Unit Or Every[ValidationErrorOrWarning] = {
      (dtsRuleValidatorMap.values.toIndexedSeq.sortBy(_.getClass.getSimpleName) flatMap { validator =>
        entrypointUris.toSeq.sortBy(_.toString) map { entrypointUri =>
          val dts = getDts(entrypointUri, basicTaxo, lenient)

          logger.info(s"Running DTS validator ${validator.getClass.getSimpleName} for entrypoint ${entrypointUri}")
          validator.validate(dts)
        }
      }).combined.map(good => ())
    }

    logger.info(s"Validation result for DTS rules is OK: ${dtsValidationResult.isGood}")

    val validationResult: Unit Or Every[ValidationErrorOrWarning] =
      List(normalValidationResult, dtsValidationResult).combined.map(good => ())

    val errorsAndWarnings: immutable.IndexedSeq[ValidationErrorOrWarning] =
      validationResult.fold(good => Vector[ValidationErrorOrWarning](), bad => bad.toIndexedSeq)

    val errors = errorsAndWarnings collect { case err: ValidationError => err }
    val warnings = errorsAndWarnings collect { case warn: ValidationWarning => warn }

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

  private def getDts(entrypointUri: URI, context: BasicTaxonomy, lenient: Boolean): BasicTaxonomy = {
    // Inefficient to recompute the relationships

    val documentBuilder = new DocumentBuilder {

      type BackingDoc = BackingDocumentApi

      def build(uri: URI): BackingDoc = {
        context.taxonomyBase.taxonomyDocUriMap.getOrElse(uri, sys.error(s"Missing document $uri")).backingDocument
      }
    }

    val documentCollector = DefaultDtsCollector()

    val relationshipFactory =
      if (lenient) DefaultRelationshipFactory.LenientInstance else DefaultRelationshipFactory.StrictInstance

    val taxoBuilder =
      TaxonomyBuilder.
        withDocumentBuilder(documentBuilder).
        withDocumentCollector(documentCollector).
        withRelationshipFactory(relationshipFactory)

    taxoBuilder.build(Set(entrypointUri))
  }
}
