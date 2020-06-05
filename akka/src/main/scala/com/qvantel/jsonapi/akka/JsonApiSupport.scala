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
package com.qvantel.jsonapi.akka

import _root_.akka.http.scaladsl.Http
import _root_.akka.http.scaladsl.client.RequestBuilding
import _root_.akka.http.scaladsl.marshalling._
import _root_.akka.http.scaladsl.model._
import _root_.akka.http.scaladsl.model.headers._
import _root_.akka.http.scaladsl.unmarshalling._
import _root_.akka.stream.Materializer
import _root_.akka.stream.scaladsl._
import _root_.akka.util.{ByteString, Timeout}
import _root_.spray.json._

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._

import com.qvantel.jsonapi._
import com.qvantel.jsonapi.model.TopLevel

trait JsonApiSupport extends JsonApiSupport0 {

  implicit def jsonApiCollectionMarshaller[T](
      implicit writer: JsonApiWriter[T],
      printer: JsonPrinter = PrettyPrinter,
      metaProfiles: Set[MetaProfile] = Set.empty,
      sorting: JsonApiSorting = JsonApiSorting.Unsorted,
      sparseFields: Map[String, List[String]] = Map.empty,
      pagination: JsonApiPagination.PaginationFunc = JsonApiPagination.EmptyFunc): ToEntityMarshaller[Iterable[T]] =
    Marshaller.withFixedContentType(ct) { as =>
      HttpEntity(ct, rawCollection(as))
    }

  implicit def jsonApiCollectionRequestUnmarshaller[T](
      implicit reader: JsonApiReader[T]): FromRequestUnmarshaller[Iterable[T]] =
    new FromRequestUnmarshaller[Iterable[T]] {
      override def apply(value: HttpRequest)(implicit ec: ExecutionContext,
                                             materializer: Materializer): Future[Iterable[T]] = {
        val include = value.uri.query().get("include").map(_.split(',').toSet).getOrElse(Set.empty[String])
        value.entity.toStrict(10.seconds).flatMap(strictEntity => extractEntities(strictEntity.data, include))
      }
    }

  implicit def jsonApiCollectionResponseUnmarshaller[T](
      implicit reader: JsonApiReader[T]): FromResponseUnmarshaller[Iterable[T]] =
    new FromResponseUnmarshaller[Iterable[T]] {
      override def apply(value: HttpResponse)(implicit ec: ExecutionContext,
                                              materializer: Materializer): Future[Iterable[T]] = {
        val include = value.headers
          .find(_.name == JsonApiSupport.JsonApiIncludeHeader)
          .map(_.value.split(',').toSet)
          .getOrElse(Set.empty[String])
        value.entity.toStrict(10.seconds).flatMap(strictEntity => extractEntities(strictEntity.data, include))
      }
    }

}

trait JsonApiSupport0 {
  val ct = MediaTypes.`application/vnd.api+json`

  implicit val jsonApiTopLevelSingle: Unmarshaller[HttpEntity, TopLevel.Single] = {
    Unmarshaller.byteStringUnmarshaller.map { data =>
      JsonParser(data.utf8String).asJsObject.convertTo[TopLevel.Single]
    }
  }

  implicit val jsonApiTopLevelCollection: Unmarshaller[HttpEntity, TopLevel.Collection] = {
    Unmarshaller.byteStringUnmarshaller.map { data =>
      JsonParser(data.utf8String).asJsObject.convertTo[TopLevel.Collection]
    }
  }

  implicit val jsonApiErrorObject: Unmarshaller[HttpEntity, TopLevel.Errors] = {
    Unmarshaller.byteStringUnmarshaller.map { data =>
      JsonParser(data.utf8String).asJsObject.convertTo[TopLevel.Errors]
    }
  }

  implicit def jsonApiOneMarshaller[T](implicit writer: JsonApiWriter[T],
                                       printer: JsonPrinter = PrettyPrinter,
                                       metaProfiles: Set[MetaProfile] = Set.empty,
                                       sorting: JsonApiSorting = JsonApiSorting.Unsorted,
                                       sparseFields: Map[String, List[String]] = Map.empty): ToEntityMarshaller[T] =
    Marshaller.withFixedContentType(ct) { a =>
      HttpEntity(ct, rawOne(a))
    }

  implicit def relatedResponseMarshaller[A](
      implicit writer: JsonApiWriter[A],
      printer: JsonPrinter = PrettyPrinter,
      sorting: JsonApiSorting = JsonApiSorting.Unsorted,
      sparseFields: Map[String, List[String]] = Map.empty): ToEntityMarshaller[com.qvantel.jsonapi.RelatedResponse[A]] =
    PredefinedToEntityMarshallers.StringMarshaller.wrap(ct) { value =>
      printer.apply(value.toResponse)
    }

  implicit def jsonApiOneRequestUnmarshaller[T](implicit reader: JsonApiReader[T]): FromRequestUnmarshaller[T] =
    new FromRequestUnmarshaller[T] {
      override def apply(value: HttpRequest)(implicit ec: ExecutionContext, materializer: Materializer): Future[T] = {
        val include = value.uri.query().get("include").map(_.split(',').toSet).getOrElse(Set.empty[String])
        value.entity.toStrict(10.seconds).flatMap(strictEntity => extractEntity(strictEntity.data, include))
      }
    }

  implicit def jsonApiOneResponseUnmarshaller[T](implicit reader: JsonApiReader[T]): FromResponseUnmarshaller[T] =
    new FromResponseUnmarshaller[T] {
      override def apply(value: HttpResponse)(implicit ec: ExecutionContext, materializer: Materializer): Future[T] = {
        val include = value.headers
          .find(_.name == JsonApiSupport.JsonApiIncludeHeader)
          .map(_.value.split(',').toSet)
          .getOrElse(Set.empty[String])
        value.entity.toStrict(10.seconds).flatMap(strictEntity => extractEntity(strictEntity.data, include))
      }
    }

  def extractEntity[T](data: ByteString, include: Set[String])(implicit reader: JsonApiReader[T],
                                                               ec: ExecutionContext): Future[T] =
    Future {
      val json = JsonParser(data.decodeString("UTF-8")).asJsObject
      readOne[T](json, include)
    }

  def extractEntity[T](data: Source[ByteString, Any], include: Set[String])(implicit materializer: Materializer,
                                                                            reader: JsonApiReader[T],
                                                                            ec: ExecutionContext): Future[T] =
    data.runFold(ByteString(""))(_ ++ _).flatMap(extractEntity[T](_, include))

  def extractEntities[T](data: ByteString, include: Set[String])(implicit reader: JsonApiReader[T],
                                                                 ec: ExecutionContext): Future[Iterable[T]] =
    Future {
      val json = JsonParser(data.decodeString("UTF-8")).asJsObject
      readCollection[T](json, include).toList
    }

  def extractEntities[T](data: Source[ByteString, Any], include: Set[String])(
      implicit materializer: Materializer,
      reader: JsonApiReader[T],
      ec: ExecutionContext): Future[Iterable[T]] =
    data.runFold(ByteString(""))(_ ++ _).flatMap(extractEntities[T](_, include))
}

/** Custom SendReceive that adds the include params into X-Internal-Include
  * header that can be read by FromResponseUnmarshaller
  */
object JsonApiClientAkka extends RequestBuilding {
  import _root_.akka.actor._
  import _root_.akka.http.scaladsl.settings.{ClientConnectionSettings, ConnectionPoolSettings}

  import scala.concurrent.duration._

  def jsonApiSendReceive(implicit refFactory: ActorRefFactory,
                         executionContext: ExecutionContext,
                         system: ActorSystem,
                         futureTimeout: Timeout = 60.seconds): HttpRequest => Future[HttpResponse] = {

    val conSettings     = ClientConnectionSettings(system.settings.config).withIdleTimeout(futureTimeout.duration)
    val timeoutSettings = ConnectionPoolSettings(system.settings.config).withConnectionSettings(conSettings)
    req =>
      val response = Http().singleRequest(request = req, settings = timeoutSettings)
      req.uri.query().get("include") match {
        case Some(include) => response.map(_.withHeaders(RawHeader(JsonApiSupport.JsonApiIncludeHeader, include)))
        case None          => response
      }
  }
}

object JsonApiSupport extends JsonApiSupport {
  val JsonApiIncludeHeader: String = "X-Internal-Include"
}
