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
package schema

import java.net.URI
import scala.collection.immutable
import eu.cdevreeze.yaidom._
import XmlSchemaObject._

/**
 * Immutable XML Schema object. Inspired by the Apache Common XML Schema API 2.0.
 *
 * TODO Use yaidom trait HasParent.
 *
 * @author Chris de Vreeze
 */
sealed class XmlSchemaObject(val wrappedElem: indexed.Elem) extends ElemLike[XmlSchemaObject] with XmlSchemaObject.HasParent[XmlSchemaObject] with HasText with Immutable {

  final override def allChildElems: immutable.IndexedSeq[XmlSchemaObject] = {
    wrappedElem.allChildElems map { e => XmlSchemaObject(e) }
  }

  final override def resolvedName: EName = wrappedElem.resolvedName

  final override def resolvedAttributes: immutable.IndexedSeq[(EName, String)] = wrappedElem.resolvedAttributes

  final override def equals(obj: Any): Boolean = obj match {
    case other: XmlSchemaObject => (other.wrappedElem == this.wrappedElem)
    case _ => false
  }

  final override def hashCode: Int = wrappedElem.hashCode

  final override def text: String = {
    val textStrings = wrappedElem.elem.textChildren map { t => t.text }
    textStrings.mkString
  }

  final override def parentOption: Option[XmlSchemaObject] =
    wrappedElem.parentOption map { e => XmlSchemaObject(e) }

  final override def toString: String = wrappedElem.elem.toString
}

final class XmlSchema(override val wrappedElem: indexed.Elem) extends XmlSchemaObject(wrappedElem) {
  require(wrappedElem.resolvedName == EName(ns, "schema"))

  final def targetNamespaceOption: Option[String] = wrappedElem \@ EName("targetNamespace")

  final def elementDeclarations: immutable.IndexedSeq[XmlSchemaElement] =
    this filterElemsOrSelf { e => e.resolvedName == EName(ns, "element") } collect { case e: XmlSchemaElement => e }

  final def globalElementDeclarations: immutable.IndexedSeq[XmlSchemaElement] =
    elementDeclarations filter { e => e.wrappedElem.elemPath.entries.size == 1 }
}

abstract class XmlSchemaParticle(override val wrappedElem: indexed.Elem) extends XmlSchemaObject(wrappedElem) {

  final def minOccurs: String = (wrappedElem \@ EName("minOccurs")).getOrElse(1.toString)

  final def maxOccursOption: String = (wrappedElem \@ EName("maxOccurs")).getOrElse(1.toString)
}

final class XmlSchemaElement(override val wrappedElem: indexed.Elem) extends XmlSchemaParticle(wrappedElem) {
  require(wrappedElem.resolvedName == EName(ns, "element"))

  final def enameOption: Option[EName] = {
    val tnsOption = wrappedElem.rootElem \@ EName("targetNamespace")
    val localNameOption = wrappedElem \@ EName("name")
    localNameOption map { nm => EName(tnsOption, nm) }
  }

  final def idOption: Option[String] = wrappedElem \@ EName("id")

  final def typeAttributeOption: Option[EName] = {
    val typeAttrStringOption = wrappedElem \@ EName("type")
    typeAttrStringOption map { tpe =>
      wrappedElem.elem.scope.resolveQNameOption(QName(tpe)).getOrElse(
        sys.error("Could not resolve type '%s' as expanded name".format(tpe)))
    }
  }

  final def substitutionGroupOption: Option[EName] = {
    val substGroupStringOption = wrappedElem \@ EName("substitutionGroup")
    substGroupStringOption map { substGroup =>
      wrappedElem.elem.scope.resolveQNameOption(QName(substGroup)).getOrElse(
        sys.error("Could not resolve substitution group '%s' as expanded name".format(substGroup)))
    }
  }

  final def abstractOption: Option[Boolean] = (wrappedElem \@ EName("abstract")) map (_.toBoolean)

  final def nillableOption: Option[Boolean] = (wrappedElem \@ EName("nillable")) map (_.toBoolean)

  final def refOption: Option[EName] = {
    val refOption = wrappedElem \@ EName("ref")
    refOption map { ref =>
      wrappedElem.elem.scope.resolveQNameOption(QName(ref)).getOrElse(
        sys.error("Could not resolve ref '%s' as expanded name".format(ref)))
    }
  }

  final def xbrliPeriodTypeOption: Option[String] = wrappedElem \@ EName(nsXbrli, "periodType")
}

abstract class XmlSchemaType(override val wrappedElem: indexed.Elem) extends XmlSchemaObject(wrappedElem) {
}

final class XmlSchemaSimpleType(override val wrappedElem: indexed.Elem) extends XmlSchemaType(wrappedElem) {
  require(wrappedElem.resolvedName == EName(ns, "simpleType"))
}

final class XmlSchemaComplexType(override val wrappedElem: indexed.Elem) extends XmlSchemaType(wrappedElem) {
  require(wrappedElem.resolvedName == EName(ns, "complexType"))
}

object XmlSchemaObject {

  val ns = "http://www.w3.org/2001/XMLSchema"
  val nsXbrli = "http://www.xbrl.org/2003/instance"

  trait HasParent[E <: HasParent[E]] { self: E =>

    /**
     * Returns the parent element, if any, wrapped in an Option
     */
    def parentOption: Option[E]

    /**
     * Returns the equivalent `parentOption.get`, throwing an exception if this is the root element
     */
    final def parent: E = parentOption.getOrElse(sys.error("There is no parent element"))

    /**
     * Returns all ancestor elements or self
     */
    final def ancestorsOrSelf: immutable.IndexedSeq[E] =
      self +: (parentOption.toIndexedSeq flatMap ((e: E) => e.ancestorsOrSelf))

    /**
     * Returns `ancestorsOrSelf.drop(1)`
     */
    final def ancestors: immutable.IndexedSeq[E] = ancestorsOrSelf.drop(1)

    /**
     * Returns the first found ancestor-or-self element obeying the given predicate, if any, wrapped in an Option
     */
    final def findAncestorOrSelf(p: E => Boolean): Option[E] = ancestorsOrSelf find p

    /**
     * Returns the first found ancestor element obeying the given predicate, if any, wrapped in an Option
     */
    final def findAncestor(p: E => Boolean): Option[E] = ancestors find p
  }

  def apply(wrappedElem: indexed.Elem): XmlSchemaObject = wrappedElem match {
    // TODO
    case e if e.resolvedName == EName(ns, "schema") => XmlSchema(wrappedElem)
    case e if e.resolvedName == EName(ns, "element") => XmlSchemaElement(wrappedElem)
    case e if e.resolvedName == EName(ns, "simpleType") => XmlSchemaSimpleType(wrappedElem)
    case e if e.resolvedName == EName(ns, "complexType") => XmlSchemaComplexType(wrappedElem)
    case _ => new XmlSchemaObject(wrappedElem)
  }
}

object XmlSchema {

  def apply(wrappedElem: indexed.Elem): XmlSchema = {
    require(wrappedElem.resolvedName == EName(ns, "schema"))
    new XmlSchema(wrappedElem)
  }
}

object XmlSchemaElement {

  def apply(wrappedElem: indexed.Elem): XmlSchemaElement = {
    require(wrappedElem.resolvedName == EName(ns, "element"))
    new XmlSchemaElement(wrappedElem)
  }
}

object XmlSchemaSimpleType {

  def apply(wrappedElem: indexed.Elem): XmlSchemaSimpleType = {
    require(wrappedElem.resolvedName == EName(ns, "simpleType"))
    new XmlSchemaSimpleType(wrappedElem)
  }
}

object XmlSchemaComplexType {

  def apply(wrappedElem: indexed.Elem): XmlSchemaComplexType = {
    require(wrappedElem.resolvedName == EName(ns, "complexType"))
    new XmlSchemaComplexType(wrappedElem)
  }
}
