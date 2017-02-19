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

package eu.cdevreeze.nta.taxo

import java.net.URI

import eu.cdevreeze.tqa.taxonomy.BasicTaxonomy

/**
 * Sub-taxonomy, consisting of a TQA "backing" taxonomy and an explicit subset of the document URIs
 * of that taxonomy. This document URI subset makes up the "sub-taxonomy".
 *
 * The sub-taxonomy determines the scope of validation. It could be a single document in the backing
 * taxonomy, or one NT version for one NT partner, or even an extension taxonomy.
 *
 * @author Chris de Vreeze
 */
final class SubTaxonomy(val backingTaxonomy: BasicTaxonomy, val filterSubTaxonomy: URI => Boolean) {

  def asBasicTaxonomy: BasicTaxonomy = {
    val docUris = backingTaxonomy.taxonomyBase.rootElemUriMap.keySet.filter(filterSubTaxonomy)
    backingTaxonomy.filterDocumentUris(docUris)
  }
}
