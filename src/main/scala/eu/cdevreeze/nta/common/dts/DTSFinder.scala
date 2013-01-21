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
package dts

import java.net.URI
import scala.annotation.tailrec
import scala.collection.immutable
import eu.cdevreeze.yaidom.{ Document, Elem, EName, QName, xlink }
import DTSFinder._

/**
 * DTS finder, starting with a (taxonomy) document, and trying to find the DTS within the given "universe".
 *
 * @author Chris de Vreeze
 */
final class DTSFinder(val universe: Map[URI, Document]) extends Immutable {

  /**
   * Finds the DTS starting with the given document URI, by following DTS discovery rules.
   */
  def findDTS(docUri: URI): DiscoveryResult = findDTS(Set(docUri))

  /**
   * Finds the DTS starting with the given document URIs, by following DTS discovery rules.
   */
  @tailrec
  def findDTS(docUris: Set[URI]): DiscoveryResult = {
    require(docUris forall (_.isAbsolute), "Expected only absolute URIs but got '%s' instead".format(docUris))
    require(docUris forall (_.getFragment eq null), "Expected only non-fragment URIs but got '%s' instead".format(docUris))

    // Perform one step in the DTS discovery, by looking for appropriate hrefs in the Document
    val newHrefs: Set[URI] = {
      val result =
        for {
          baseUri <- (docUris intersect universe.keySet)
          doc = universe(baseUri)
          f <- hrefProducers
          newUri <- f(doc)
        } yield withoutFragment(resolveUri(baseUri, newUri))
      result.toSet
    }

    val newlyAddedHrefs = newHrefs diff docUris

    if (newlyAddedHrefs.isEmpty) new DiscoveryResult(docUris)
    else {
      // Recursive call
      findDTS(docUris union newHrefs)
    }
  }

  private val hrefProducers: Seq[Document => Set[URI]] =
    Seq(getImports _, getIncludes _, getLocs _, getRoleRefs _, getArcroleRefs _, getLinkbaseRefs _)

  private def getImports(doc: Document): Set[URI] = doc match {
    case schemaDoc if mustBeSchema(doc) =>
      val importElms = (doc.documentElement \\ EName(nsSchema, "import"))
      val hrefs = importElms flatMap { e => (e \@ "schemaLocation") } map { v => new URI(v) }
      hrefs.toSet
    case _ => Set()
  }

  private def getIncludes(doc: Document): Set[URI] = doc match {
    case schemaDoc if mustBeSchema(doc) =>
      val includeElms = (doc.documentElement \\ EName(nsSchema, "include"))
      val hrefs = includeElms flatMap { e => (e \@ "schemaLocation") } map { v => new URI(v) }
      hrefs.toSet
    case _ => Set()
  }

  private def getLocs(doc: Document): Set[URI] = doc match {
    case linkbaseDoc if mustBeLinkbase(doc) =>
      val locElms = (doc.documentElement \\ EName(nsLinkbase, "loc"))
      val hrefs = locElms map { e => xlink.Locator(e).href }
      hrefs.toSet
    case _ => Set()
  }

  private def getRoleRefs(doc: Document): Set[URI] = doc match {
    case linkbaseDoc if mustBeLinkbase(doc) =>
      val roleRefElms = (doc.documentElement \\ EName(nsLinkbase, "roleRef"))
      val hrefs = roleRefElms flatMap { e => xlink.SimpleLink(e).hrefOption }
      hrefs.toSet
    case _ => Set()
  }

  private def getArcroleRefs(doc: Document): Set[URI] = doc match {
    case linkbaseDoc if mustBeLinkbase(doc) =>
      val arcroleRefElms = (doc.documentElement \\ EName(nsLinkbase, "arcroleRef"))
      val hrefs = arcroleRefElms flatMap { e => xlink.SimpleLink(e).hrefOption }
      hrefs.toSet
    case _ => Set()
  }

  private def getLinkbaseRefs(doc: Document): Set[URI] = doc match {
    case schemaDoc if mustBeSchema(doc) =>
      val linkbaseRefElms = (doc.documentElement \\ EName(nsLinkbase, "linkbaseRef"))
      val hrefs = linkbaseRefElms flatMap { e => xlink.SimpleLink(e).hrefOption }
      hrefs.toSet
    case _ => Set()
  }

  private def mustBeSchema(doc: Document): Boolean = doc.documentElement.resolvedName == EName(nsSchema, "schema")

  private def mustBeLinkbase(doc: Document): Boolean = doc.documentElement.resolvedName == EName(nsLinkbase, "linkbase")

  private def resolveUri(baseUri: URI, uri: URI): URI = {
    require(baseUri.isAbsolute, "Expected absolute base URI but got '%s' instead".format(baseUri))
    baseUri.resolve(uri)
  }

  private def withoutFragment(uri: URI): URI = new URI(uri.getScheme, uri.getSchemeSpecificPart, null)

  final class DiscoveryResult(val uris: Set[URI]) extends Immutable {

    val dts: Map[URI, Document] = universe filterKeys uris

    val undiscovered: Set[URI] = uris diff universe.keySet

    def ++(other: DiscoveryResult): DiscoveryResult =
      new DiscoveryResult(this.uris ++ other.uris)
  }

  object DiscoveryResult {

    val Empty = new DiscoveryResult(Set())
  }
}

object DTSFinder {

  val nsSchema = "http://www.w3.org/2001/XMLSchema"
  val nsLinkbase = "http://www.xbrl.org/2003/linkbase"
  val nsXLink = "http://www.w3.org/1999/xlink"
}
