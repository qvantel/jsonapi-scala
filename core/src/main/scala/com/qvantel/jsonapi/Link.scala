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

import shapeless.Coproduct
import com.netaporter.uri.Uri
import com.netaporter.uri.dsl._
import _root_.spray.json.DefaultJsonProtocol._
import _root_.spray.json._

object Link {
  private[this] def links[P](parent: P, name: String)(implicit pathToParent: PathTo[P],
                                                      pid: Identifiable[P]): JsObject =
    JsObject("related" -> implicitly[JsonWriter[Uri]].write(pathToParent.entity(parent) / name))

  def to[A, P](parent: P, relation: ToOne[A], name: String)(implicit identifiable: Identifiable[A],
                                                            pid: Identifiable[P],
                                                            rt: ResourceType[A],
                                                            pathTo: PathTo[A],
                                                            pathToParent: PathTo[P]): JsValue = {
    def resourceLinkage(id: String): JsObject =
      JsObject("type" -> implicitly[ResourceType[A]].resourceType.toJson, "id" -> id.toJson)

    relation match {
      case ToOne.Reference(id) =>
        JsObject("data" -> resourceLinkage(id), "links" -> links(parent, name))

      case ToOne.Loaded(entity) =>
        JsObject("data" -> resourceLinkage(identifiable.identify(entity)), "links" -> links(parent, name))
    }
  }

  def to[A, P](parent: P, maybeRelation: Option[ToOne[A]], name: String)(implicit identifiable: Identifiable[A],
                                                                         pid: Identifiable[P],
                                                                         rt: ResourceType[A],
                                                                         pathTo: PathTo[A],
                                                                         pathToParent: PathTo[P]): JsValue = {
    def resourceLinkage(id: String): JsObject =
      JsObject("type" -> implicitly[ResourceType[A]].resourceType.toJson, "id" -> id.toJson)

    maybeRelation match {
      case Some(ToOne.Reference(id)) =>
        JsObject("data" -> resourceLinkage(id), "links" -> links(parent, name))

      case Some(ToOne.Loaded(entity)) =>
        JsObject("data" -> resourceLinkage(identifiable.identify(entity)), "links" -> links(parent, name))

      case _ =>
        JsObject("data" -> JsNull, "links" -> links(parent, name))
    }
  }

  def to[A, P](parent: P, maybeRelation: JsonOption[ToOne[A]], name: String)(implicit identifiable: Identifiable[A],
                                                                             pid: Identifiable[P],
                                                                             rt: ResourceType[A],
                                                                             pathTo: PathTo[A],
                                                                             pathToParent: PathTo[P]): JsValue = {
    def resourceLinkage(id: String): JsObject =
      JsObject("type" -> implicitly[ResourceType[A]].resourceType.toJson, "id" -> id.toJson)

    maybeRelation match {
      case JsonSome(ToOne.Reference(id)) =>
        JsObject("data" -> resourceLinkage(id), "links" -> links(parent, name))

      case JsonSome(ToOne.Loaded(entity)) =>
        JsObject("data" -> resourceLinkage(identifiable.identify(entity)), "links" -> links(parent, name))

      case JsonNull =>
        JsObject("data" -> JsNull, "links" -> links(parent, name))

      case _ =>
        JsObject("links" -> links(parent, name))
    }
  }

  def to[A, P](parent: P, relation: ToMany[A], name: String)(implicit identifiable: Identifiable[A],
                                                             pid: Identifiable[P],
                                                             rt: ResourceType[A],
                                                             pathTo: PathTo[A],
                                                             pathToParent: PathTo[P]): JsValue =
    relation match {
      case ToMany.IdsReference(ids) =>
        val linksTuple: (String, JsValue) = "links" -> links(parent, name)
        val resourceLinkage = ids.map { id =>
          JsObject(
            "type" -> implicitly[ResourceType[A]].resourceType.toJson,
            "id"   -> id.toJson
          )
        }
        val data: (String, JsValue) = "data" -> JsArray(resourceLinkage.toVector)
        JsObject(Map(linksTuple, data))
      case ToMany.PathReference(Some(path)) =>
        JsObject("links" -> JsObject("related" -> path.toJson))
      case ToMany.PathReference(None) =>
        JsObject("links" -> links(parent, name))
      case ToMany.Loaded(entities) =>
        val resourceLinkage = entities map { entity =>
          JsObject("type" -> implicitly[ResourceType[A]].resourceType.toJson,
                   "id"   -> identifiable.identify(entity).toJson)
        }
        JsObject("data" -> JsArray(resourceLinkage.toVector), "links" -> links(parent, name))
    }

  def to[A <: Coproduct, P](parent: P, relation: PolyToOne[A], name: String)(implicit identifiable: PolyIdentifiable[A],
                                                                             pid: Identifiable[P],
                                                                             pathToParent: PathTo[P]): JsValue = {
    def resourceLinkage(tpe: String, id: String): JsObject =
      JsObject("type" -> tpe.toJson, "id" -> id.toJson)

    relation match {
      case PolyToOne.Reference(id, rt) =>
        JsObject("data" -> resourceLinkage(rt, id), "links" -> links(parent, name))

      case PolyToOne.Loaded(entity, id, rt) =>
        JsObject("data" -> resourceLinkage(rt, id), "links" -> links(parent, name))
    }
  }

  def to[A <: Coproduct, P](parent: P, maybeRelation: Option[PolyToOne[A]], name: String)(
      implicit identifiable: PolyIdentifiable[A],
      pid: Identifiable[P],
      pathToParent: PathTo[P]): JsValue = {
    def resourceLinkage(tpe: String, id: String): JsObject =
      JsObject("type" -> tpe.toJson, "id" -> id.toJson)

    maybeRelation match {
      case Some(PolyToOne.Reference(id, rt)) =>
        JsObject("data" -> resourceLinkage(rt, id), "links" -> links(parent, name))

      case Some(PolyToOne.Loaded(entity, id, rt)) =>
        JsObject("data" -> resourceLinkage(rt, id), "links" -> links(parent, name))

      case _ =>
        JsObject("data" -> JsNull, "links" -> links(parent, name))
    }
  }

  def to[A <: Coproduct, P](parent: P, maybeRelation: JsonOption[PolyToOne[A]], name: String)(
      implicit identifiable: PolyIdentifiable[A],
      pid: Identifiable[P],
      pathToParent: PathTo[P]): JsValue = {
    def resourceLinkage(tpe: String, id: String): JsObject =
      JsObject("type" -> tpe.toJson, "id" -> id.toJson)

    maybeRelation match {
      case JsonSome(PolyToOne.Reference(id, rt)) =>
        JsObject("data" -> resourceLinkage(rt, id), "links" -> links(parent, name))

      case JsonSome(PolyToOne.Loaded(entity, id, rt)) =>
        JsObject("data" -> resourceLinkage(rt, id), "links" -> links(parent, name))

      case JsonNull =>
        JsObject("data" -> JsNull, "links" -> links(parent, name))

      case _ =>
        JsObject("links" -> links(parent, name))
    }
  }

  def to[A <: Coproduct, P](parent: P, relation: PolyToMany[A], name: String)(
      implicit identifiable: PolyIdentifiable[A],
      pid: Identifiable[P],
      pathToParent: PathTo[P]): JsValue =
    relation match {
      case PolyToMany.IdsReference(rels) =>
        val linksTuple: (String, JsValue) = "links" -> links(parent, name)
        val resourceLinkage = rels.map { rel =>
          JsObject(
            "type" -> rel.resourceType.toJson,
            "id"   -> rel.id.toJson
          )
        }
        val data: (String, JsValue) = "data" -> JsArray(resourceLinkage.toVector)
        JsObject(Map(linksTuple, data))

      case PolyToMany.PathReference(Some(path)) =>
        JsObject("links" -> JsObject("related" -> path.toJson))

      case PolyToMany.PathReference(None) =>
        JsObject("links" -> links(parent, name))

      case PolyToMany.Loaded(entities) =>
        val resourceLinkage = entities map { entity =>
          JsObject("type" -> identifiable.resourceType(entity).toJson, "id" -> identifiable.identify(entity).toJson)
        }

        JsObject("data" -> JsArray(resourceLinkage.toVector), "links" -> links(parent, name))
    }

  def toNoParentPath[A, P](parent: P, relation: ToOne[A], name: String)(implicit identifiable: Identifiable[A],
                                                                        rt: ResourceType[A],
                                                                        pathTo: PathTo[A]): JsValue = {
    def resourceLinkage(id: String): JsObject =
      JsObject("type" -> implicitly[ResourceType[A]].resourceType.toJson, "id" -> id.toJson)

    relation match {
      case ToOne.Reference(id) =>
        JsObject("data" -> resourceLinkage(id))

      case ToOne.Loaded(entity) =>
        JsObject("data" -> resourceLinkage(identifiable.identify(entity)))
    }
  }

  def toNoParentPath[A, P](parent: P, maybeRelation: Option[ToOne[A]], name: String)(
      implicit identifiable: Identifiable[A],
      rt: ResourceType[A],
      pathTo: PathTo[A]): JsValue = {
    def resourceLinkage(id: String): JsObject =
      JsObject("type" -> implicitly[ResourceType[A]].resourceType.toJson, "id" -> id.toJson)

    maybeRelation match {
      case Some(ToOne.Reference(id))  => JsObject("data" -> resourceLinkage(id))
      case Some(ToOne.Loaded(entity)) => JsObject("data" -> resourceLinkage(identifiable.identify(entity)))
      case _                          => JsObject("data" -> JsNull)
    }
  }

  def toNoParentPath[A, P](parent: P, maybeRelation: JsonOption[ToOne[A]], name: String)(
      implicit identifiable: Identifiable[A],
      rt: ResourceType[A],
      pathTo: PathTo[A]): JsValue = {
    def resourceLinkage(id: String): JsObject =
      JsObject("type" -> implicitly[ResourceType[A]].resourceType.toJson, "id" -> id.toJson)

    maybeRelation match {
      case JsonSome(ToOne.Reference(id))  => JsObject("data" -> resourceLinkage(id))
      case JsonSome(ToOne.Loaded(entity)) => JsObject("data" -> resourceLinkage(identifiable.identify(entity)))
      case JsonNull                       => JsObject("data" -> JsNull)
      case _                              => JsObject.empty
    }
  }

  def toNoParentPath[A, P](parent: P, relation: ToMany[A], name: String)(implicit identifiable: Identifiable[A],
                                                                         rt: ResourceType[A],
                                                                         pathTo: PathTo[A]): JsValue =
    relation match {
      case ToMany.IdsReference(ids) =>
        val resourceLinkage = ids.map { id =>
          JsObject(
            "type" -> implicitly[ResourceType[A]].resourceType.toJson,
            "id"   -> id.toJson
          )
        }
        JsObject("data" -> JsArray(resourceLinkage.toVector))

      case ToMany.PathReference(Some(path)) =>
        JsObject("links" -> JsObject("related" -> path.toJson))

      case ToMany.PathReference(None) =>
        // not 100% sure what to do in this case
        JsObject("data" -> JsArray.empty)

      case ToMany.Loaded(entities) =>
        val resourceLinkage = entities map { entity =>
          JsObject("type" -> implicitly[ResourceType[A]].resourceType.toJson,
                   "id"   -> identifiable.identify(entity).toJson)
        }
        JsObject("data" -> JsArray(resourceLinkage.toVector))
    }

  def toNoParentPath[A <: Coproduct, P](parent: P, relation: PolyToOne[A], name: String)(
      implicit identifiable: PolyIdentifiable[A]): JsValue = {
    def resourceLinkage(tpe: String, id: String): JsObject =
      JsObject("type" -> tpe.toJson, "id" -> id.toJson)

    relation match {
      case PolyToOne.Reference(id, rt) =>
        JsObject("data" -> resourceLinkage(rt, id))

      case PolyToOne.Loaded(entity, id, rt) =>
        JsObject("data" -> resourceLinkage(rt, id))
    }
  }

  def toNoParentPath[A <: Coproduct, P](parent: P, maybeRelation: Option[PolyToOne[A]], name: String)(
      implicit identifiable: PolyIdentifiable[A]): JsValue = {
    def resourceLinkage(tpe: String, id: String): JsObject =
      JsObject("type" -> tpe.toJson, "id" -> id.toJson)

    maybeRelation match {
      case Some(PolyToOne.Reference(id, rt)) =>
        JsObject("data" -> resourceLinkage(rt, id))

      case Some(PolyToOne.Loaded(entity, id, rt)) =>
        JsObject("data" -> resourceLinkage(rt, id))

      case _ =>
        JsObject("data" -> JsNull)
    }
  }

  def toNoParentPath[A <: Coproduct, P](parent: P, maybeRelation: JsonOption[PolyToOne[A]], name: String)(
      implicit identifiable: PolyIdentifiable[A]): JsValue = {
    def resourceLinkage(tpe: String, id: String): JsObject =
      JsObject("type" -> tpe.toJson, "id" -> id.toJson)

    maybeRelation match {
      case JsonSome(PolyToOne.Reference(id, rt)) =>
        JsObject("data" -> resourceLinkage(rt, id))

      case JsonSome(PolyToOne.Loaded(entity, id, rt)) =>
        JsObject("data" -> resourceLinkage(rt, id))

      case JsonNull =>
        JsObject("data" -> JsNull)

      case _ =>
        JsObject.empty
    }
  }

  def toNoParentPath[A <: Coproduct, P](parent: P, relation: PolyToMany[A], name: String)(
      implicit identifiable: PolyIdentifiable[A]): JsValue =
    relation match {
      case PolyToMany.IdsReference(rels) =>
        val resourceLinkage = rels.map { rel =>
          JsObject(
            "type" -> rel.resourceType.toJson,
            "id"   -> rel.id.toJson
          )
        }
        JsObject("data" -> JsArray(resourceLinkage.toVector))

      case PolyToMany.PathReference(Some(path)) =>
        JsObject("links" -> JsObject("related" -> path.toJson))

      case PolyToMany.PathReference(None) =>
        // not 100% sure what to do in this case
        JsObject("data" -> JsArray.empty)

      case PolyToMany.Loaded(entities) =>
        val resourceLinkage = entities map { entity =>
          JsObject("type" -> identifiable.resourceType(entity).toJson, "id" -> identifiable.identify(entity).toJson)
        }

        JsObject("data" -> JsArray(resourceLinkage.toVector))
    }
}
