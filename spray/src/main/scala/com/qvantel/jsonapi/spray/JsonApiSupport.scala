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
package com.qvantel.jsonapi.spray

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success, Try}
import akka.actor.{ActorRef, ActorRefFactory}
import akka.io.IO
import akka.util.Timeout
import _root_.spray.can.Http
import _root_.spray.client.pipelining.SendReceive
import _root_.spray.http._
import _root_.spray.httpx._
import _root_.spray.httpx.marshalling.Marshaller
import _root_.spray.httpx.unmarshalling._
import _root_.spray.json._
import com.qvantel.jsonapi._

trait JsonApiSupport extends JsonApiSupport0 {
  implicit def jsonApiCollectionMarshaller[T](
      implicit writer: JsonApiWriter[T],
      printer: JsonPrinter = PrettyPrinter,
      metaProfiles: Set[MetaProfile] = Set.empty,
      sorting: JsonApiSorting = JsonApiSorting.Unsorted,
      pagination: JsonApiPagination.PaginationFunc = JsonApiPagination.EmptyFunc): Marshaller[Iterable[T]] =
    Marshaller.of[Iterable[T]](ct) { (value, _, ctx) =>
      ctx.marshalTo(HttpEntity(ct, HttpData(printer.apply(rawCollection(value)), HttpCharsets.`UTF-8`)))
    }

  implicit def jsonApiCollectionRequestUnmarshaller[T](
      implicit reader: JsonApiReader[T]): FromRequestUnmarshaller[Iterable[T]] =
    new FromRequestUnmarshaller[Iterable[T]] {
      override def apply(req: HttpRequest): Deserialized[Iterable[T]] =
        req.entity match {
          case HttpEntity.NonEmpty(requestCt @ ContentType(mediaType, _), data)
              if requestCt == ct || mediaType == MediaTypes.`application/vnd.api+json` =>
            Try { JsonParser(data.asString(HttpCharsets.`UTF-8`)).asJsObject } match {
              case Failure(e) => Left(MalformedContent(e.getMessage, e))
              case Success(json) =>
                val include = req.uri.query.get("include").map(_.split(',').toSet).getOrElse(Set.empty[String])
                Right(readCollection[T](json, include))
            }
          case HttpEntity.NonEmpty(ContentType(mediaType, _), _) =>
            Left(UnsupportedContentType(s"expected Content-Type:application/vnd.api+json but got $mediaType"))
          case HttpEntity.Empty =>
            Left(ContentExpected)
        }
    }

  implicit def jsonApiCollectionResponseUnmarshaller[T](
      implicit reader: JsonApiReader[T]): FromResponseUnmarshaller[Iterable[T]] =
    new FromResponseUnmarshaller[Iterable[T]] {
      override def apply(resp: HttpResponse): Deserialized[Iterable[T]] =
        resp.entity match {
          case HttpEntity.NonEmpty(requestCt @ ContentType(mediaType, _), data)
              if requestCt == ct || mediaType == MediaTypes.`application/vnd.api+json` =>
            Try { JsonParser(data.asString(HttpCharsets.`UTF-8`)).asJsObject } match {
              case Failure(e) => Left(MalformedContent(e.getMessage, e))
              case Success(json) =>
                val include = resp.headers
                  .find(_.name == JsonApiSupport.JsonApiIncludeHeader)
                  .map(_.value.split(',').toSet)
                  .getOrElse(Set.empty[String])
                Right(readCollection[T](json, include))
            }
          case HttpEntity.NonEmpty(ContentType(mediaType, _), _) =>
            Left(UnsupportedContentType(s"expected Content-Type:application/vnd.api+json but got $mediaType"))
          case HttpEntity.Empty =>
            Left(ContentExpected)
        }
    }
}

trait JsonApiSupport0 {
  val ct = ContentType(MediaTypes.`application/vnd.api+json`, None).withoutDefinedCharset

  implicit def relatedResponseMarshaller[A](
      implicit writer: JsonApiWriter[A],
      printer: JsonPrinter = PrettyPrinter,
      sorting: JsonApiSorting = JsonApiSorting.Unsorted): Marshaller[com.qvantel.jsonapi.RelatedResponse[A]] =
    Marshaller.of[RelatedResponse[A]](ct) { (value, _, ctx) =>
      ctx.marshalTo(HttpEntity(ct, HttpData(printer.apply(value.toResponse), HttpCharsets.`UTF-8`)))
    }

  implicit def jsonApiOneMarshaller[T](implicit writer: JsonApiWriter[T],
                                       printer: JsonPrinter = PrettyPrinter,
                                       metaProfiles: Set[MetaProfile] = Set.empty,
                                       sorting: JsonApiSorting = JsonApiSorting.Unsorted): Marshaller[T] =
    Marshaller.of[T](ct) { (value, _, ctx) =>
      ctx.marshalTo(HttpEntity(ct, HttpData(printer.apply(rawOne(value)), HttpCharsets.`UTF-8`)))
    }

  implicit def jsonApiOneRequestUnmarshaller[T](implicit reader: JsonApiReader[T]): FromRequestUnmarshaller[T] =
    new FromRequestUnmarshaller[T] {
      override def apply(req: HttpRequest): Deserialized[T] =
        req.entity match {
          case HttpEntity.NonEmpty(ContentType(mediaType, _), data)
              if mediaType == MediaTypes.`application/vnd.api+json` =>
            Try { JsonParser(data.asString(HttpCharsets.`UTF-8`)).asJsObject } match {
              case Failure(e) => Left(MalformedContent(e.getMessage, e))
              case Success(json) =>
                val include = req.uri.query.get("include").map(_.split(',').toSet).getOrElse(Set.empty[String])
                Right(readOne[T](json, include))
            }
          case HttpEntity.NonEmpty(ContentType(mediaType, _), _) =>
            Left(UnsupportedContentType(s"expected Content-Type:application/vnd.api+json but got $mediaType"))
          case HttpEntity.Empty =>
            Left(ContentExpected)
        }
    }

  implicit def jsonApiOneResponseUnmarshaller[T](implicit reader: JsonApiReader[T]): FromResponseUnmarshaller[T] =
    new FromResponseUnmarshaller[T] {
      override def apply(resp: HttpResponse): Deserialized[T] =
        resp.entity match {
          case HttpEntity.NonEmpty(requestCt @ ContentType(mediaType, _), data)
              if requestCt == ct || mediaType == MediaTypes.`application/vnd.api+json` =>
            Try { JsonParser(data.asString(HttpCharsets.`UTF-8`)).asJsObject } match {
              case Failure(e) => Left(MalformedContent(e.getMessage, e))
              case Success(json) =>
                val include = resp.headers
                  .find(_.name == JsonApiSupport.JsonApiIncludeHeader)
                  .map(_.value.split(',').toSet)
                  .getOrElse(Set.empty[String])
                Right(readOne[T](json, include))
            }
          case HttpEntity.NonEmpty(ContentType(mediaType, _), _) =>
            Left(UnsupportedContentType(s"expected Content-Type:application/vnd.api+json but got $mediaType"))
          case HttpEntity.Empty =>
            Left(ContentExpected)
        }
    }
}

/** Custom SendReceive that adds the include params into X-Internal-Include
  * header that can be read by FromResponseUnmarshaller
  */
object JsonApiClient extends RequestBuilding with ResponseTransformation {
  import scala.concurrent.duration._
  import akka.pattern.ask
  import _root_.spray.util.actorSystem

  def jsonApiSendReceive(implicit refFactory: ActorRefFactory,
                         executionContext: ExecutionContext,
                         futureTimeout: Timeout = 60.seconds): SendReceive =
    jsonApiSendReceive(IO(Http)(actorSystem))

  def jsonApiSendReceive(transport: ActorRef)(implicit ec: ExecutionContext, futureTimeout: Timeout): SendReceive =
    request =>
      transport ? request map {
        case x: HttpResponse =>
          request.uri.query.get("include") match {
            case Some(include) => x.withHeaders(HttpHeaders.RawHeader(JsonApiSupport.JsonApiIncludeHeader, include))
            case None          => x
          }
        case x: HttpResponsePart      => sys.error("sendReceive doesn't support chunked responses, try sendTo instead")
        case x: Http.ConnectionClosed => sys.error(s"Connection closed before reception of response: $x")
        case x                        => sys.error(s"Unexpected response from HTTP transport: $x")
    }
}

object JsonApiSupport extends JsonApiSupport {
  val JsonApiIncludeHeader: String = "X-Internal-Include"
}
