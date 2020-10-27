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

sealed abstract class ResourceLinkage

object ResourceLinkage {
  final case class ToOne(resource: Option[ResourceIdentifierObject]) extends ResourceLinkage
  final case class ToMany(resources: Set[ResourceIdentifierObject])  extends ResourceLinkage

  implicit object ResourceLinkageJsonFormat extends JsonFormat[ResourceLinkage] {
    override def write(obj: ResourceLinkage): JsValue = obj match {
      case ToOne(None)           => JsNull
      case ToOne(Some(resource)) => resource.toJson
      case ToMany(resources)     => JsArray(resources.map(_.toJson).toVector)
    }

    override def read(json: JsValue): ResourceLinkage = json match {
      case JsNull      => ToOne(None)
      case _: JsObject => ToOne(Some(json.convertTo[ResourceIdentifierObject]))
      case _: JsArray  => ToMany(json.convertTo[Set[ResourceIdentifierObject]])
      case invalid     => deserializationError(s"Invalid resource linkage: ‘$invalid’")
    }
  }
}
