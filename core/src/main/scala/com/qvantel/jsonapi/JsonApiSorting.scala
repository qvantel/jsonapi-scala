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

import _root_.spray.json._
import _root_.spray.json.DefaultJsonProtocol._

/** Very crude sorting used by JsonApiSupport marshalling bits
  * works on raw json objects so performance won't be good
  */
sealed trait JsonApiSorting {
  def sort(items: Iterable[JsObject]): Seq[JsObject]
}

object JsonApiSorting {
  private[this] val byId: Ordering[JsObject] =
    Ordering.by[JsObject, Option[String]](_.fields.get("id").map(_.convertTo[String]))

  case object Unsorted extends JsonApiSorting {
    override def sort(items: Iterable[JsObject]): Seq[JsObject] = items.toSeq
  }

  case object AscendingId extends JsonApiSorting {
    private[this] val ord = ByOrdering(byId)

    override def sort(items: Iterable[JsObject]): Seq[JsObject] = ord.sort(items)
  }

  case object DescendingId extends JsonApiSorting {
    private[this] val ord = ByOrdering(byId.reverse)

    override def sort(items: Iterable[JsObject]): Seq[JsObject] = ord.sort(items)
  }

  case class ByOrdering(ord: Ordering[JsObject]) extends JsonApiSorting {
    override def sort(items: Iterable[JsObject]): Seq[JsObject] =
      items.toSeq.sorted(ord)
  }
}
