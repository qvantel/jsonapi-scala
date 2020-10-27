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
import io.lemonlabs.uri.{Url => ScalaUrl}
import io.lemonlabs.uri.config.UriConfig
import io.lemonlabs.uri.typesafe.dsl._

sealed abstract class Link {
  def href: ScalaUrl
}

object Link {
  final case class Url(href: ScalaUrl)                          extends Link
  final case class LinkObject(href: ScalaUrl, meta: MetaObject) extends Link

  implicit val uriConfig: UriConfig = com.qvantel.jsonapi.uriConfig

  implicit object LinkJsonFormat extends JsonFormat[Link] {
    override def write(obj: Link): JsValue = obj match {
      case Url(href) => JsString(href.toString)
      case LinkObject(href, meta) =>
        if (meta.nonEmpty) {
          JsObject("href" -> href.toString.toJson, "meta" -> meta.toJson)
        } else {
          JsObject("href" -> href.toString.toJson)
        }
    }

    override def read(json: JsValue): Link = json match {
      case JsString(href) => Url(href)
      case JsObject(fields) =>
        LinkObject(
          href = fields.get("href").map(_.convertTo[String]).getOrElse(deserializationError("Expected ‘href’ in Link")),
          meta = fields.get("meta").map(_.convertTo[MetaObject]).getOrElse(Map.empty)
        )
      case invalid => deserializationError(s"Expected Link as JsString or JsObject, got ‘$invalid’")
    }
  }

  def convertToLinks: (JsValue) => Links =
    _.convertTo[Map[String, JsValue]].filterNot(_._2 == JsNull).map(entry => (entry._1, entry._2.convertTo[Link]))
}
