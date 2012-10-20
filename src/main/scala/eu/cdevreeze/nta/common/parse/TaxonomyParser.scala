/*
 * Copyright 2011 Chris de Vreeze
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

package eu.cdevreeze.nta
package common
package parse

import java.{ util => jutil, io => jio }
import java.net.URI
import scala.collection.immutable
import eu.cdevreeze.yaidom._
import common.document._

/**
 * Taxonomy parser.
 *
 * @author Chris de Vreeze
 */
trait TaxonomyParser {

  protected val logger: jutil.logging.Logger = jutil.logging.Logger.getLogger("eu.cdevreeze.nta.common.parse")

  private val docParser = eu.cdevreeze.yaidom.parse.DocumentParserUsingDom.newInstance

  /**
   * Parses a "taxonomy" (using this term loosely) from a root directory, with a given function mapping local URIs to
   * the original URIs.
   *
   * If multiple roots should be parsed, simply call this method repeatedly on those different roots, and combine the
   * resulting `Taxonomy` instances using method `++`.
   */
  final def parse(rootDir: jio.File)(localUriToOriginalUri: URI => URI): Taxonomy = {
    require(rootDir.isDirectory && rootDir.exists, "Expected root directory %s".format(rootDir.getPath))

    val files = findAllFiles(rootDir)
    logger.info("Found %d taxonomy files".format(files.size))

    val docs: immutable.Seq[TaxonomyDocument] = {
      val result = files.par flatMap { f: jio.File =>
        try {
          val doc = docParser.parse(new jio.FileInputStream(f))

          val localUri = f.toURI
          val originalUri = localUriToOriginalUri(localUri)

          // TODO Recognizing linkbases is now hard-coded and very brittle. Refactor.

          if (doc.documentElement.resolvedName.namespaceUriOption == Some(SchemaDocument.NS))
            Some(new SchemaDocument(originalUri, localUri, doc))
          else if (doc.documentElement.resolvedName.namespaceUriOption == Some(LinkbaseDocument.NS) &&
            doc.documentElement.findChildElem(EName(LinkbaseDocument.NS, "labelLink")).isDefined)

            Some(new LabelLinkbaseDocument(originalUri, localUri, doc))
          else if (doc.documentElement.resolvedName.namespaceUriOption == Some(LinkbaseDocument.NS))
            Some(new LinkbaseDocument(originalUri, localUri, doc))
          else None
        } catch {
          case e: Exception =>
            logger.warning(e.toString)
            None
        }
      }
      result.seq
    }
    logger.info("Found %d taxonomy documents".format(docs.size))

    require(!docs.isEmpty, "Expected at least one taxonomy file")
    val taxonomy = Taxonomy(docs)
    taxonomy
  }

  private def findAllFiles(dir: jio.File): immutable.Seq[jio.File] = {
    require(dir.isDirectory, "Not a directory: %s".format(dir.getPath))

    dir.listFiles.toIndexedSeq flatMap { f: jio.File =>
      if (f.isFile) Vector(f)
      else if (f.isDirectory) findAllFiles(f)
      else Vector()
    }
  }
}
