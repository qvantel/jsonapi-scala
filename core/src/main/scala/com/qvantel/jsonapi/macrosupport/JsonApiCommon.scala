/*
Copyright (c) 2017, Qvantel
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
 * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
 * Neither the name of the Qvantel nor the
      names of its contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL Qvantel BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package com.qvantel.jsonapi.macrosupport

import scala.annotation.compileTimeOnly

import com.qvantel.jsonapi._

@compileTimeOnly("Macros can only be used at compile-time")
trait JsonApiCommon extends Tools {
  import c.universe._

  val toOneType                 = typeOf[ToOne[_]]
  val optionalToOneType         = typeOf[Option[ToOne[_]]]
  val jsonOptionalToOneType     = typeOf[JsonOption[ToOne[_]]]
  val toManyType                = typeOf[ToMany[_]]
  val polyToOneType             = typeOf[PolyToOne[_]]
  val optionalPolyToOneType     = typeOf[Option[PolyToOne[_]]]
  val jsonOptionalPolyToOneType = typeOf[JsonOption[PolyToOne[_]]]
  val polyToManyType            = typeOf[PolyToMany[_]]

  val optionType     = typeOf[Option[_]]
  val jsonOptionType = typeOf[JsonOption[_]]

  def camelToDashes(name: String) =
    "[A-Z\\d]".r.replaceAllIn(name, m => "-" + m.group(0).toLowerCase())

  def resourceType(t: c.Type): c.Tree =
    q"_root_.scala.Predef.implicitly[_root_.com.qvantel.jsonapi.ResourceType[$t]].resourceType"

  def caseClassFields(t: c.Type): List[c.universe.TermSymbol] = {
    val primaryConstructor = t.typeSymbol.asClass.primaryConstructor.asMethod
    primaryConstructor.paramLists.headOption getOrElse {
      c.abort(c.enclosingPosition, s"Case class constructor does not have any fields")
    } map (_.asTerm) sortBy (_.name.toString)
  }

  def partitionedCaseClassFields(t: c.Type): (List[c.universe.TermSymbol], List[c.universe.TermSymbol]) =
    partitionRelations(t, caseClassFields(t))

  def relationFields(t: c.Type): List[c.universe.TermSymbol] =
    partitionedCaseClassFields(t)._1

  def resolveContainedType(t: c.Type): c.Type =
    if (t <:< toOneType || t <:< toManyType || t <:< polyToOneType || t <:< polyToManyType) {
      t.typeArgs.headOption.getOrElse(c.abort(c.enclosingPosition, s"$t typeArgs is empty"))
    } else if (t <:< optionalToOneType || t <:< jsonOptionalToOneType || t <:< optionalPolyToOneType || t <:< jsonOptionalPolyToOneType) {
      t.typeArgs.headOption
        .getOrElse(c.abort(c.enclosingPosition, s"$t typeArgs is empty"))
        .typeArgs
        .headOption
        .getOrElse(c.abort(c.enclosingPosition, s"$t typeArgs is empty"))
    } else {
      c.abort(c.enclosingPosition, s"Cannot determine contained type for $t")
    }

  def emptyForIterableType(t: c.Type): c.Tree =
    if (t <:< typeOf[Iterable[_]]) {
      q"${t.typeSymbol.companion}.empty[..${t.typeArgs}]"
    } else {
      c.abort(c.enclosingPosition, s"Type $t is not an Iterable")
    }

  def resolveContainedAttributeType(t: c.Type): c.Type =
    if (t <:< optionType || t <:< jsonOptionType) {
      t.typeArgs.headOption.getOrElse(c.abort(c.enclosingPosition, s"$t typeArgs is empty"))
    } else {
      c.abort(c.enclosingPosition, s"Cannot determine contained type for $t")
    }

  def relationshipTypes(t: c.Type): List[(c.universe.Type, String)] =
    relationFields(t) map { field =>
      (field.infoIn(t), field.name.toString)
    }

  def partitionRelations(
      t: c.Type,
      fields: List[c.universe.TermSymbol]): (List[c.universe.TermSymbol], List[c.universe.TermSymbol]) =
    fields partition { field =>
      val fieldType = field.infoIn(t)
      fieldType <:< toOneType ||
      fieldType <:< optionalToOneType ||
      fieldType <:< jsonOptionalToOneType ||
      fieldType <:< toManyType ||
      fieldType <:< polyToOneType ||
      fieldType <:< optionalPolyToOneType ||
      fieldType <:< jsonOptionalPolyToOneType ||
      fieldType <:< polyToManyType
    }
}
