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
package document

import java.net.URI
import scala.collection.immutable
import eu.cdevreeze.yaidom.EName

/**
 * Substitution group.
 *
 * @author Chris de Vreeze
 */
final class SubstitutionGroup(val name: EName, val parentGroupOption: Option[SubstitutionGroup]) extends Immutable {

  require(name ne null)
  require(parentGroupOption ne null)

  def isItem: Boolean = (this == SubstitutionGroup.Item) || (parentGroupOption.exists(_.isItem))

  def isTuple: Boolean = (this == SubstitutionGroup.Tuple) || (parentGroupOption.exists(_.isTuple))

  def ::(newName: EName): SubstitutionGroup = new SubstitutionGroup(newName, Some(this))

  override def equals(obj: Any): Boolean = obj match {
    case other: SubstitutionGroup => this.name == other.name
    case _ => false
  }

  override def hashCode: Int = name.hashCode

  override def toString: String = name.toString
}

object SubstitutionGroup {

  val Item = new SubstitutionGroup(EName("http://www.xbrl.org/2003/instance", "item"), None)

  val Tuple = new SubstitutionGroup(EName("http://www.xbrl.org/2003/instance", "tuple"), None)

  val HypercubeItem = EName("http://xbrl.org/2005/xbrldt", "hypercubeItem") :: Item

  val DimensionItem = EName("http://xbrl.org/2005/xbrldt", "dimensionItem") :: Item

  val wellKnownSubstitutionGroups: Set[SubstitutionGroup] = Set(Item, Tuple, HypercubeItem, DimensionItem)
}
