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

final case class ErrorObjects(errors: List[ErrorObject])

object ErrorObjects {
  import ErrorObject.ErrorObjectJsonFormat._
  implicit object ErrorObjectsJsonFormat extends RootJsonFormat[ErrorObjects] {
    override def write(obj: ErrorObjects): JsValue = {
      val builder = Map.newBuilder[String, JsValue]
      builder += "errors" -> obj.errors.toJson
      JsObject(builder.result())
    }

    override def read(json: JsValue): ErrorObjects = {
      val fields = json.asJsObject.fields
      ErrorObjects(errors = fields.get("errors").fold(List.empty[ErrorObject])(_.convertTo[List[ErrorObject]]))
    }
  }
}

final case class ErrorObject(id: Option[String] = None,
                             links: Links = Map.empty,
                             status: Option[String] = None,
                             code: Option[String] = None,
                             title: Option[String] = None,
                             detail: Option[String] = None,
                             source: Option[ErrorSource] = None,
                             meta: MetaObject = Map.empty)

object ErrorObject {
  implicit object ErrorObjectJsonFormat extends RootJsonFormat[ErrorObject] {
    override def write(obj: ErrorObject): JsValue = {
      val builder = Map.newBuilder[String, JsValue]
      obj.id.foreach(x => builder += ("id" -> x.toJson))
      if (obj.links.nonEmpty) builder += "links" -> obj.links.toJson
      obj.status.foreach(x => builder += ("status" -> x.toJson))
      obj.code.foreach(x => builder += ("code"     -> x.toJson))
      obj.title.foreach(x => builder += ("title"   -> x.toJson))
      obj.detail.foreach(x => builder += ("detail" -> x.toJson))
      obj.source.foreach(x => builder += ("source" -> x.toJson))
      if (obj.meta.nonEmpty) builder += "meta" -> obj.meta.toJson
      JsObject(builder.result())
    }

    override def read(json: JsValue): ErrorObject = {
      val fields = json.asJsObject.fields
      ErrorObject(
        id = fields.get("id").flatMap(_.convertTo[Option[String]]),
        links = fields.get("links").map(_.convertTo[Links]).getOrElse(Map.empty),
        status = fields.get("status").flatMap(_.convertTo[Option[String]]),
        code = fields.get("code").flatMap(_.convertTo[Option[String]]),
        title = fields.get("title").flatMap(_.convertTo[Option[String]]),
        detail = fields.get("detail").flatMap(_.convertTo[Option[String]]),
        source = fields.get("source").flatMap(_.convertTo[Option[ErrorSource]]),
        meta = fields.get("meta").map(_.convertTo[MetaObject]).getOrElse(Map.empty)
      )
    }
  }
}
