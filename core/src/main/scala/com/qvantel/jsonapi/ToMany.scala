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
package com.qvantel.jsonapi

import _root_.spray.json.DefaultJsonProtocol._
import _root_.spray.json._

/**
  * Represents a relationship to zero or more objects of type A
  * [[com.qvantel.jsonapi.ToMany.Reference]] case class is used to represent a ToMany relationship where
  * the objects have not been loaded
  * [[com.qvantel.jsonapi.ToMany.Loaded]] case class is used to represent a ToMany relationship where
  * the objects have been loaded
  * @tparam A Type of the object the relationships point to
  */
sealed trait ToMany[A] {
  def ids: Set[String]

  /** Loaded biased get method as a helper when you don't want to pattern match like crazy */
  def get: Seq[A]
}

object ToMany {
  final case class Reference[A](ids: Set[String]) extends ToMany[A] {
    override def get: Seq[A] = Seq.empty
  }

  final case class Loaded[A: Identifiable](entities: Seq[A]) extends ToMany[A] {
    val ids = entities.map(implicitly[Identifiable[A]].identify).toSet

    override def get: Seq[A] = entities
  }

  def reference[A]: ToMany[A]                              = Reference[A](Set.empty)
  def reference[A](ids: Set[String]): ToMany[A]            = Reference[A](ids)
  def loaded[A: Identifiable](entities: Seq[A]): ToMany[A] = Loaded[A](entities)

  def renderRelation[P, A](parent: P, name: String, relation: ToMany[A])(implicit pt: PathTo[P],
                                                                         rt: ResourceType[A],
                                                                         ident: Identifiable[A]): JsObject = {
    def json(entities: Seq[A]): JsObject =
      JsObject(
        "links" -> JsObject("self" -> (pt.entity(parent) / "relationships" / name).toJson,
                            "related" -> (pt.entity(parent) / name).toJson),
        "data" -> JsArray(entities map { entity =>
          JsObject("type" -> rt.resourceType.toJson, "id" -> ident.identify(entity).toJson)
        }: _*)
      )
    relation match {
      case Reference(_)     => json(Seq.empty)
      case Loaded(entities) => json(entities)
    }
  }
}
