package com.qvantel.jsonapi.client.akka

import _root_.spray.json._
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.coding.{Deflate, Gzip, NoCoding}
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.HttpEncodings
import akka.http.scaladsl.unmarshalling.Unmarshaller._
import akka.http.scaladsl.unmarshalling._
import akka.stream.ActorMaterializer
import com.netaporter.uri
import com.netaporter.uri.dsl._
import monix.eval.Task

import com.qvantel.jsonapi._

object AkkaClient {
  def apply[A](implicit jac: JsonApiClient[A]) = implicitly[JsonApiClient[A]]

  implicit def instance[A](implicit rt: ResourceType[A],
                           reader: JsonApiReader[A],
                           m: ActorMaterializer,
                           system: ActorSystem,
                           endpoint: ApiEndpoint): JsonApiClient[A] = {
    def respToEntity(resp: HttpResponse, include: Set[String]): Task[Option[A]] =
      resp.status match {
        case StatusCodes.OK =>
          Task.deferFutureAction { implicit scheduler =>
            Unmarshal(resp).to[JsObject].map { obj =>
              Some(readOne[A](obj, include))
            }
          }
        case StatusCodes.NotFound => Task.now(None)
        case status =>
          Task.deferFutureAction { implicit scheduler =>
            Unmarshal(resp).to[String].map { body =>
              throw ApiError.HttpError(status.toString(), body)
            }
          }
      }

    def respoToEntities(resp: HttpResponse, include: Set[String]): Task[List[A]] =
      resp.status match {
        case StatusCodes.OK =>
          Task.deferFutureAction { implicit scheduler =>
            Unmarshal(resp).to[JsObject].map { obj =>
              readCollection[A](obj, include).toList
            }
          }
        case StatusCodes.NotFound => Task.now(List.empty)
        case status =>
          Task.deferFutureAction { implicit scheduler =>
            Unmarshal(resp).to[String].map { body =>
              throw ApiError.HttpError(status.toString(), body)
            }
          }
      }

    new JsonApiClient[A] {
      override def one(id: String, include: Set[String] = Set.empty): Task[Option[A]] =
        endpoint.uri.flatMap { baseUri =>
          val reqUri = baseUri / rt.resourceType / id

          mkRequest(reqUri, include).flatMap(respToEntity(_, include))
        }.memoizeOnSuccess

      override def many(ids: Set[String], include: Set[String]) = {
        val requests = ids.map { id =>
          one(id, include).map {
            case Some(entity) => Right(entity)
            case None         => Left(id)
          }
        }

        Task.gatherUnordered(requests).memoizeOnSuccess
      }

      override def pathOne(path: uri.Uri, include: Set[String]) =
        endpoint.uri.flatMap { baseUri =>
          val reqUri = baseUri.copy(pathParts = path.pathParts)

          mkRequest(reqUri, include).flatMap(respToEntity(_, include))
        }.memoizeOnSuccess

      override def pathMany(path: uri.Uri, include: Set[String]) =
        endpoint.uri.flatMap { baseUri =>
          val reqUri = baseUri.copy(pathParts = path.pathParts)

          mkRequest(reqUri, include).flatMap(respoToEntities(_, include))
        }.memoizeOnSuccess

      override def filter(filter: String, include: Set[String]) =
        endpoint.uri.flatMap { baseUri =>
          val reqUri = baseUri / rt.resourceType ? ("filter" -> filter)

          mkRequest(reqUri, include).flatMap(respoToEntities(_, include))
        }.memoizeOnSuccess
    }
  }

  private[this] def mkRequest[A](reqUri: uri.Uri, include: Set[String])(implicit m: ActorMaterializer,
                                                                        system: ActorSystem): Task[HttpResponse] = {
    val uriWithInclude = if (include.isEmpty) {
      reqUri
    } else {
      reqUri ? ("include" -> include.mkString(","))
    }

    Task.deferFutureAction { implicit scheduler =>
      Http()
        .singleRequest(
          HttpRequest(
            uri = Uri(uriWithInclude),
            headers = encodings
          )
        )
        .map(decodeResponse)
    }
  }

  private[this] val encodings: List[HttpHeader] = {
    val enc = HttpHeader.parse("Accept-Encoding", "deflate, gzip;q=1.0") match {
      case HttpHeader.ParsingResult.Ok(header, _) => header
      case _                                      => throw new Exception("this should not happen")
    }

    val accept = HttpHeader.parse("Accept", "application/vnd.api+json") match {
      case HttpHeader.ParsingResult.Ok(header, _) => header
      case _                                      => throw new Exception("this should not happen")
    }

    List(enc, accept)
  }

  private[this] val mt: MediaType = MediaType.customWithOpenCharset("application", "vnd.api+json")

  private[this] def decodeResponse(response: HttpResponse): HttpResponse = {
    val decoder = response.encoding match {
      case HttpEncodings.gzip     => Gzip
      case HttpEncodings.deflate  => Deflate
      case HttpEncodings.identity => NoCoding
    }

    decoder.decodeMessage(response)
  }

  private[this] implicit def unmarshaller: FromEntityUnmarshaller[JsObject] =
    Unmarshaller.byteStringUnmarshaller
      .forContentTypes(mt)
      .andThen(SprayJsonSupport.sprayJsValueByteStringUnmarshaller)
      .map(_.asJsObject)
}
