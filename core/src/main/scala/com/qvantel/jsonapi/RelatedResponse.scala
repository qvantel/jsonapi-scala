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

import _root_.spray.json.{JsArray, JsNull, JsObject, JsValue, JsonPrinter, PrettyPrinter}

/** Used render proper related link response as specified by jsonapi spec
  * found at http://jsonapi.org/format/1.0/#fetching-resources
  *
  * @tparam A Type of the object that the RelatedResponse points to
  */
sealed trait RelatedResponse[A] {
  def toResponse(implicit writer: JsonApiWriter[A],
                 printer: JsonPrinter = PrettyPrinter,
                 sorting: JsonApiSorting = JsonApiSorting.Unsorted,
                 sparseFields: Map[String, List[String]] = Map.empty,
                 pagination: JsonApiPagination.PaginationFunc = JsonApiPagination.EmptyFunc): JsValue

  def map[B](f: A => B): RelatedResponse[B]
}

object RelatedResponse {
  private[this] sealed trait One[A] extends RelatedResponse[A]

  private[this] object One {
    final class Empty[A] extends One[A] {
      def toResponse(implicit writer: JsonApiWriter[A],
                     printer: JsonPrinter = PrettyPrinter,
                     sorting: JsonApiSorting = JsonApiSorting.Unsorted,
                     sparseFields: Map[String, List[String]] = Map.empty,
                     pagination: JsonApiPagination.PaginationFunc = JsonApiPagination.EmptyFunc): JsValue =
        JsObject("data" -> JsNull)

      def map[B](f: A => B): RelatedResponse[B] = new Empty[B]
    }

    final case class Result[A](data: A) extends One[A] {
      def toResponse(implicit writer: JsonApiWriter[A],
                     printer: JsonPrinter = PrettyPrinter,
                     sorting: JsonApiSorting = JsonApiSorting.Unsorted,
                     sparseFields: Map[String, List[String]] = Map.empty,
                     pagination: JsonApiPagination.PaginationFunc = JsonApiPagination.EmptyFunc): JsValue = rawOne(data)

      def map[B](f: A => B): RelatedResponse[B] = Result(f(data))
    }

    def apply[A](a: Option[A]): One[A] = a match {
      case None       => new Empty
      case Some(data) => Result(data)
    }

    def apply[A](a: A): One[A] = Result(a)
  }

  private[this] sealed trait Many[+A] extends RelatedResponse[A]

  private[this] object ToMany {
    final class Empty[A] extends Many[A] {
      def toResponse(implicit writer: JsonApiWriter[A],
                     printer: JsonPrinter = PrettyPrinter,
                     sorting: JsonApiSorting = JsonApiSorting.Unsorted,
                     sparseFields: Map[String, List[String]] = Map.empty,
                     pagination: JsonApiPagination.PaginationFunc = JsonApiPagination.EmptyFunc): JsValue =
        JsObject("data" -> JsArray.empty)

      def map[B](f: A => B): RelatedResponse[B] = new Empty[B]
    }

    final case class Result[A](data: List[A]) extends Many[A] {
      def toResponse(implicit writer: JsonApiWriter[A],
                     printer: JsonPrinter = PrettyPrinter,
                     sorting: JsonApiSorting = JsonApiSorting.Unsorted,
                     sparseFields: Map[String, List[String]] = Map.empty,
                     pagination: JsonApiPagination.PaginationFunc = JsonApiPagination.EmptyFunc): JsValue =
        rawCollection(data)

      def map[B](f: A => B): RelatedResponse[B] = Result(data.map(f))
    }

    def apply[A](a: List[A]): Many[A] = a match {
      case Nil => new Empty
      case all => Result(all)
    }

    def apply[A](a: Iterable[A]): Many[A] = apply(a.toList)
    def apply[A](a: Seq[A]): Many[A]      = apply(a.toList)
    def apply[A](a: Set[A]): Many[A]      = apply(a.toList)
  }

  def apply[A](a: Option[A]): RelatedResponse[A] = One(a)
  def apply[A](a: A): RelatedResponse[A]         = One(a)

  def apply[A](a: Iterable[A]): RelatedResponse[A] = ToMany(a.toList)
  def apply[A](a: Seq[A]): RelatedResponse[A]      = ToMany(a.toList)
  def apply[A](a: Set[A]): RelatedResponse[A]      = ToMany(a.toList)
}
