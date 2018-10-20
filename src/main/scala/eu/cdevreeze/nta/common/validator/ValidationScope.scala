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

import java.net.URI

/**
 * Validation scope. It says which part of a taxonomy or DTS must be validated. It holds URI start strings,
 * much like the ones in an XML catalog (as restricted by the Taxonomy Packages standard).
 *
 * Note that a Taxonomy holds entire DTSes, including www.xbrl.org and www.w3.org, but it is highly unlikely
 * that a validator should validate those parts of the taxonomy as well. Hence this notion of a validation
 * scope. Validation scopes can even be a restricted as some ad-hoc extension taxonomy.
 *
 * A validator may make use of multiple layered validation scopes, if so desired.
 *
 * File exclusion filters are not part of this notion of a validation scope.
 *
 * @author Chris de Vreeze
 */
final class ValidationScope(val uriStartStrings: Set[String]) {

  def matches(uri: URI): Boolean = {
    val uriAsString = uri.toString
    uriStartStrings.exists(s => uriAsString.startsWith(s))
  }
}
