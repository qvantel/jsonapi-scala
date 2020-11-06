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
package com.qvantel.jsonapi.model

import _root_.spray.json.DefaultJsonProtocol._
import _root_.spray.json._

import com.qvantel.jsonapi.JsonApiFormat
import com.qvantel.jsonapi.model.TopLevel.IdType

sealed abstract class TopLevel {
  def meta: MetaObject
  def jsonapi: Option[JsonApiInfo]
  def links: Links
}

sealed trait Compound {
  def included: Map[IdType, ResourceObject]
  final def isCompound: Boolean = included.nonEmpty
}

object TopLevel {
  type IdType = (String, String)

  @inline def mkResourceObjectTuple(obj: ResourceObject): (IdType, ResourceObject) =
    ((obj.id.getOrElse(obj.hashCode().toString), obj.`type`), obj)

  @inline def mkResourceObjectMap(jsValue: JsValue): Map[IdType, ResourceObject] =
    jsValue match {
      case arr: JsArray => arr.elements.map(json => mkResourceObjectTuple(json.convertTo[ResourceObject])).toMap
      case JsNull       => Map.empty
      case _            => deserializationError("included/data must be array or null")
    }

  @inline def mkResourceObjectMap(objs: Iterable[ResourceObject]): Map[IdType, ResourceObject] =
    objs.map(mkResourceObjectTuple).toMap

  final case class Single(data: Option[(IdType, ResourceObject)],
                          meta: MetaObject,
                          jsonapi: Option[JsonApiInfo],
                          links: Links,
                          included: Map[IdType, ResourceObject])
      extends TopLevel
      with Compound

  final case class Collection(data: Map[IdType, ResourceObject],
                              meta: MetaObject,
                              jsonapi: Option[JsonApiInfo],
                              links: Links,
                              included: Map[IdType, ResourceObject])
      extends TopLevel
      with Compound

  final case class Errors(meta: MetaObject, jsonapi: Option[JsonApiInfo], links: Links, errors: Set[ErrorObject])
      extends TopLevel

  implicit object SingleJsonFormat extends RootJsonFormat[Single] {
    @inline def mkResourceObjectMap(jsValue: JsValue): Map[IdType, ResourceObject] =
      jsValue match {
        case arr: JsArray => arr.elements.map(json => mkResourceObjectTuple(json.convertTo[ResourceObject])).toMap
        case JsNull       => Map.empty
        case _            => deserializationError("included must be array or null")
      }

    override def write(obj: Single): JsValue = {
      val builder = Map.newBuilder[String, JsValue]
      builder += "data" -> obj.data.map(_._2).toJson
      if (obj.meta.nonEmpty) builder += "meta" -> obj.meta.toJson
      obj.jsonapi.foreach(x => builder += "jsonapi" -> x.toJson)
      if (obj.links.nonEmpty) builder += "links"       -> obj.links.toJson
      if (obj.included.nonEmpty) builder += "included" -> obj.included.values.toJson
      JsObject(builder.result())
    }

    override def read(json: JsValue): Single = {
      val fields = json.asJsObject.fields
      Single(
        data = fields
          .get("data")
          .flatMap(_ match {
            case d: JsObject => Some(mkResourceObjectTuple(d.convertTo[ResourceObject]))
            case JsNull      => None
            case _           => deserializationError("data must be null or object")
          }),
        meta = fields.get("meta").map(_.convertTo[MetaObject]).getOrElse(Map.empty),
        jsonapi = fields.get("jsonapi").map(_.convertTo[JsonApiInfo]),
        links = fields.get("links").map(Link.convertToLinks).getOrElse(Map.empty),
        included = fields.get("included").map(mkResourceObjectMap).getOrElse(Map.empty)
      )
    }
  }

  implicit object CollectionJsonFormat extends RootJsonFormat[Collection] {
    @inline private[this] def mkResourceObjectMap(jsValue: JsValue): Map[IdType, ResourceObject] =
      jsValue match {
        case arr: JsArray => arr.elements.map(json => mkResourceObjectTuple(json.convertTo[ResourceObject])).toMap
        case JsNull       => Map.empty
        case _            => deserializationError("included/data must be array or null")
      }

    override def write(obj: Collection): JsValue = {
      val builder = Map.newBuilder[String, JsValue]
      builder += "data" -> obj.data.values.toJson
      if (obj.meta.nonEmpty) builder += "meta" -> obj.meta.toJson
      obj.jsonapi.foreach(x => builder += "jsonapi" -> x.toJson)
      if (obj.links.nonEmpty) builder += "links"       -> obj.links.toJson
      if (obj.included.nonEmpty) builder += "included" -> obj.included.values.toJson
      JsObject(builder.result())
    }

    override def read(json: JsValue): Collection = {
      val fields = json.asJsObject.fields
      Collection(
        data = fields
          .get("data")
          .map(mkResourceObjectMap)
          .getOrElse(deserializationError(s"Expected ‘data’ in resource object")),
        meta = fields.get("meta").map(_.convertTo[MetaObject]).getOrElse(Map.empty),
        jsonapi = fields.get("jsonapi").map(_.convertTo[JsonApiInfo]),
        links = fields.get("links").map(Link.convertToLinks).getOrElse(Map.empty),
        included = fields.get("included").map(mkResourceObjectMap).getOrElse(Map.empty)
      )
    }
  }

  implicit object ErrorsJsonFormat extends RootJsonFormat[Errors] {
    override def write(obj: Errors): JsValue = {
      val builder = Map.newBuilder[String, JsValue]
      builder += "errors" -> obj.errors.toJson
      if (obj.meta.nonEmpty) builder += "meta" -> obj.meta.toJson
      obj.jsonapi.foreach(x => builder += "jsonapi" -> x.toJson)
      if (obj.links.nonEmpty) builder += "links" -> obj.links.toJson
      JsObject(builder.result())
    }

    override def read(json: JsValue): Errors = {
      val fields = json.asJsObject.fields
      Errors(
        errors = fields.get("errors").map(_.convertTo[Set[ErrorObject]]).getOrElse(Set.empty),
        meta = fields.get("meta").map(_.convertTo[MetaObject]).getOrElse(Map.empty),
        jsonapi = fields.get("jsonapi").map(_.convertTo[JsonApiInfo]),
        links = fields.get("links").map(Link.convertToLinks).getOrElse(Map.empty)
      )
    }
  }

  implicit object TopLevelJsonFormat extends JsonApiFormat[TopLevel] {

    override def write(obj: TopLevel, sparseFields: Map[String, List[String]]): JsValue = obj match {
      case s: Single     => SingleJsonFormat.write(s)
      case c: Collection => CollectionJsonFormat.write(c)
      case e: Errors     => ErrorsJsonFormat.write(e)
    }

    override def write(obj: TopLevel): JsValue = obj match {
      case s: Single     => SingleJsonFormat.write(s)
      case c: Collection => CollectionJsonFormat.write(c)
      case e: Errors     => ErrorsJsonFormat.write(e)
    }

    override def read(json: JsValue): TopLevel = {
      val fields = json.asJsObject.fields
      fields.get("errors") map { _ =>
        ErrorsJsonFormat.read(json)
      } getOrElse (fields.get("data") match {
        case Some(JsArray(_))  => CollectionJsonFormat.read(json)
        case Some(JsObject(_)) => SingleJsonFormat.read(json)
        case Some(JsNull)      => SingleJsonFormat.read(json)
        case None              => deserializationError(s"Missing ‘data’ in resource object")
        case _                 => deserializationError(s"Invalid ‘data’ in resource object")
      })
    }

    override def included(obj: TopLevel, sparseFields: Map[String, List[String]] = Map.empty): Set[JsObject] =
      obj match {
        case s: Single     => s.included.values.map(_.toJson.asJsObject).toSet
        case c: Collection => c.included.values.map(_.toJson.asJsObject).toSet
        case _: Errors     => Set.empty
      }

    override def read(primary: JsValue,
                      included: Map[(String, String), JsObject],
                      includePaths: Set[String],
                      includePath: String): TopLevel = ???
  }
}
