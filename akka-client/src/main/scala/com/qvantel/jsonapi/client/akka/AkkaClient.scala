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
import cats.{Applicative, Eval}
import cats.effect.IO
import com.netaporter.uri
import com.netaporter.uri.QueryString
import com.netaporter.uri.config.UriConfig
import com.netaporter.uri.dsl._

import com.qvantel.jsonapi._

object AkkaClient {
  def apply[A](implicit jac: JsonApiClient[A]) = implicitly[JsonApiClient[A]]

  implicit def instance[A](implicit rt: ResourceType[A],
                           reader: JsonApiReader[A],
                           identifiable: Identifiable[A],
                           m: ActorMaterializer,
                           system: ActorSystem,
                           endpoint: ApiEndpoint): JsonApiClient[A] = {
    import system.dispatcher

    def bodyToJsObject(resp: HttpResponse): IO[JsObject] =
      IO.fromFuture {
        Eval.later {
          Unmarshal(resp).to[JsObject]
        }
      }

    def bodyToError[T](resp: HttpResponse): IO[T] =
      IO.fromFuture {
          Eval.later {
            Unmarshal(resp).to[String]
          }
        }
        .flatMap { body =>
          IO.raiseError(ApiError.HttpError(resp.status.toString(), body))
        }

    def respToEntity(resp: HttpResponse, include: Set[String]): IO[Option[A]] =
      resp.status match {
        case StatusCodes.OK       => bodyToJsObject(resp).map(x => Some(readOne[A](x, include)))
        case StatusCodes.NotFound => IO.pure(None)
        case _                    => bodyToError(resp)
      }

    def respoToEntities(resp: HttpResponse, include: Set[String]): IO[List[A]] =
      resp.status match {
        case StatusCodes.OK       => bodyToJsObject(resp).map(x => readCollection[A](x, include).toList)
        case StatusCodes.NotFound => IO.pure(List.empty)
        case _                    => bodyToError(resp)
      }

    new JsonApiClient[A] {
      implicit val config: UriConfig = uriConfig

      override def one(id: String, include: Set[String] = Set.empty): IO[Option[A]] =
        endpoint.uri.flatMap { baseUri =>
          val reqUri = baseUri / rt.resourceType / id

          mkRequest(addInclude(reqUri, include).toString).flatMap(respToEntity(_, include))
        }

      override def many(ids: Set[String], include: Set[String]) = {
        val requests = ids.map { id =>
          one(id, include).flatMap {
            case Some(entity) => IO.pure(entity)
            case None         => IO.raiseError(ApiError.NoEntityForId(id, rt.resourceType))
          }
        }.toList

        import cats.instances.list._

        Applicative[IO].sequence(requests)
      }

      override def pathOne(path: uri.Uri, include: Set[String]) =
        endpoint.uri.flatMap { baseUri =>
          val reqUri = baseUri.copy(pathParts = path.pathParts)

          mkRequest(addInclude(reqUri, include).toString).flatMap(respToEntity(_, include))
        }

      override def pathMany(path: uri.Uri, include: Set[String]) =
        endpoint.uri.flatMap { baseUri =>
          val reqUri = baseUri.copy(pathParts = path.pathParts)

          mkRequest(addInclude(reqUri, include).toString).flatMap(respoToEntities(_, include))
        }

      override def filter(filter: String, include: Set[String]) =
        endpoint.uri.flatMap { baseUri =>
          val reqUri = baseUri / rt.resourceType ? ("filter" -> filter)

          mkRequest(addInclude(reqUri, include).toString).flatMap(respoToEntities(_, include))
        }
    }
  }

  private[this] def addInclude(orig: uri.Uri, include: Set[String]): uri.Uri =
    if (include.isEmpty) {
      orig
    } else {
      orig ? ("include" -> include.mkString(","))
    }

  private[this] def mkRequest[A](reqUri: String)(implicit m: ActorMaterializer,
                                                 system: ActorSystem): IO[HttpResponse] = {
    import system.dispatcher

    IO.fromFuture {
      Eval.later {
        Http()
          .singleRequest(
            HttpRequest(
              uri = Uri(reqUri),
              headers = encodings
            )
          )
          .map(decodeResponse)
      }
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
