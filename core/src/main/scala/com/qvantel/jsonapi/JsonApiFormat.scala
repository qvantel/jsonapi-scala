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

import scala.annotation.implicitNotFound
import _root_.spray.json._

@implicitNotFound(msg = "Cannot find JsonApiReader or JsonApiFormat type class for ${T}")
trait JsonApiReader[T] extends RootJsonReader[T] {
  override def read(primary: JsValue): T = read(primary, Map.empty[(String, String), JsObject], Set.empty[String], "")
  def read(primary: JsValue, included: Set[JsObject]): T =
    read(primary, handleIncludes(included), Set.empty[String], "")
  def read(primary: JsValue, included: Set[JsObject], includePaths: Set[String], includePath: String): T =
    read(primary, handleIncludes(included), includePaths, includePath)
  def read(primary: JsValue,
           included: Map[(String, String), JsObject],
           includePaths: Set[String],
           includePath: String): T

  /** Makes the includes set a map of (id, type) -> jsObject of the includes
    */
  private[this] def handleIncludes(includes: Set[JsObject]): Map[(String, String), JsObject] = {
    import _root_.spray.json.DefaultJsonProtocol._
    import _root_.spray.json.lenses.JsonLenses._

    includes.map { json =>
      val id  = json.extract[String]('id)
      val tpe = json.extract[String]('type)

      ((id, tpe), json)
    }.toMap
  }
}

@implicitNotFound(msg = "Cannot find JsonApiWriter or JsonApiFormat type class for ${T}")
trait JsonApiWriter[T] extends RootJsonWriter[T] {
  override def write(obj: T): JsValue = write(obj, Map.empty)
  def included(obj: T, sparseFields: Map[String, List[String]] = Map.empty): Set[JsObject]
  def write(obj: T, sparseFields: Map[String, List[String]]): JsValue
}

@implicitNotFound(msg = "Cannot find JsonApiFormat type class for ${T}")
trait JsonApiFormat[T] extends JsonApiReader[T] with JsonApiWriter[T] with RootJsonFormat[T]
