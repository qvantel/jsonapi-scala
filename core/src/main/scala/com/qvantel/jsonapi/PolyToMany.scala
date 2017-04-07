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
 * Neither the name of the <organization> nor the
      names of its contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package com.qvantel.jsonapi

import shapeless.Coproduct
import shapeless.ops.coproduct.Inject
import _root_.spray.json.DefaultJsonProtocol._
import _root_.spray.json._

import com.qvantel.jsonapi.PolyIdentifiable.CoproductResourceType
import com.qvantel.jsonapi.PolyToMany.Rel

sealed trait PolyToMany[A <: Coproduct] {
  def relationships: Set[Rel]

  /** Loaded biased get method as a helper when you don't want to pattern match like crazy */
  def get: Seq[A]
}

object PolyToMany {
  final case class Rel(id: String, resourceType: String)

  object Rel {
    def apply[A: Identifiable: ResourceType](a: A): Rel =
      Rel(implicitly[Identifiable[A]].identify(a), implicitly[ResourceType[A]].resourceType)
    def apply[A: ResourceType](id: String): Rel =
      Rel(id, implicitly[ResourceType[A]].resourceType)
  }

  final case class Reference[A <: Coproduct](relationships: Set[Rel]) extends PolyToMany[A] {
    override def get: Seq[A] = Seq.empty
  }

  final case class Loaded[A <: Coproduct: PolyIdentifiable](entities: Seq[A]) extends PolyToMany[A] {
    override lazy val relationships = entities
      .map(x => Rel(implicitly[PolyIdentifiable[A]].identify(x), implicitly[PolyIdentifiable[A]].resourceType(x)))
      .toSet

    override def get: Seq[A] = entities
  }

  def reference[A <: Coproduct]: PolyToMany[A] = Reference[A](Set.empty)
  def reference[A <: Coproduct](rels: Map[String, String])(implicit crt: CoproductResourceType[A]): PolyToMany[A] = {
    val relationships = {
      val types = crt.apply

      if (rels.forall(x => types.contains(x._2))) {
        rels.map { x =>
          Rel(x._1, x._2)
        }
      } else {
        throw new PolyWrongTypeException(s"types ${rels.values.toSet} given when only $types are allowed")
      }

    }

    Reference[A](relationships.toSet)
  }

  def loaded[A <: Coproduct: PolyIdentifiable, E](entities: Seq[E])(implicit inj: Inject[A, E]): PolyToMany[A] =
    Loaded(entities map inj.apply)

  def loaded[A <: Coproduct: PolyIdentifiable](entities: Seq[A]): PolyToMany[A] =
    Loaded(entities)

  def renderRelation[P, A <: Coproduct](parent: P, name: String, relation: PolyToMany[A])(
      implicit pt: PathTo[P],
      pi: PolyIdentifiable[A]): JsObject = {
    def json(entities: Seq[A]): JsObject =
      JsObject(
        "links" -> JsObject("self" -> (pt.entity(parent) / "relationships" / name).toJson,
                            "related" -> (pt.entity(parent) / name).toJson),
        "data" -> JsArray(entities map { entity =>
          JsObject("type" -> pi.resourceType(entity).toJson, "id" -> pi.identify(entity).toJson)
        }: _*)
      )
    relation match {
      case Reference(_)     => json(Seq.empty)
      case Loaded(entities) => json(entities)
    }
  }

  final class PolyWrongTypeException(message: String = null, cause: Throwable = null) extends Exception(message, cause)
}
