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
package com.qvantel.jsonapi.model

import _root_.spray.json.DefaultJsonProtocol._
import _root_.spray.json._

final case class ResourceObject(id: Option[String],
                                `type`: String,
                                attributes: Attributes,
                                relationships: Relationships,
                                links: Links,
                                meta: MetaObject)

object ResourceObject {
  implicit object ResourceObjectJsonFormat extends RootJsonFormat[ResourceObject] {
    override def write(obj: ResourceObject): JsValue = {
      val builder = Map.newBuilder[String, JsValue]
      obj.id.foreach(x => builder += "id" -> x.toJson)
      builder += "type" -> obj.`type`.toJson
      if (obj.attributes.nonEmpty) builder += "attributes"       -> obj.attributes.toJson
      if (obj.relationships.nonEmpty) builder += "relationships" -> obj.relationships.toJson
      if (obj.links.nonEmpty) builder += "links"                 -> obj.links.toJson
      if (obj.meta.nonEmpty) builder += "meta"                   -> obj.meta.toJson
      JsObject(builder.result())
    }

    override def read(json: JsValue): ResourceObject = {
      val fields = json.asJsObject.fields

      ResourceObject(
        id = fields.get("id").flatMap(_.convertTo[Option[String]]),
        `type` = fields
          .get("type")
          .map(_.convertTo[String])
          .getOrElse(deserializationError(s"No ‘type’ field in resource object")),
        attributes = fields.get("attributes").map(_.convertTo[Map[String, JsValue]]).getOrElse(Map.empty),
        relationships = fields.get("relationships").map(_.convertTo[Relationships]).getOrElse(Map.empty),
        links = fields.get("links").map(Link.convertToLinks).getOrElse(Map.empty),
        meta = fields.get("meta").map(_.convertTo[MetaObject]).getOrElse(Map.empty)
      )
    }
  }
}
