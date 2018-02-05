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
import cats.effect.IO
import com.netaporter.uri
import com.netaporter.uri.config.UriConfig
import com.netaporter.uri.dsl._

import com.qvantel.jsonapi._

object AkkaClient {
  def apply[A](implicit jac: JsonApiClient[A]) = jac

  implicit def instance[A](implicit rt: ResourceType[A],
                           format: JsonApiFormat[A],
                           m: ActorMaterializer,
                           system: ActorSystem,
                           endpoint: ApiEndpoint): JsonApiClient[A] = {
    import system.dispatcher

    def bodyToJsObject(resp: HttpResponse): IO[JsObject] =
      IO.fromFuture {
        IO(Unmarshal(resp).to[JsObject])
      }

    def bodyToError[T](resp: HttpResponse): IO[T] =
      IO.fromFuture {
          IO(Unmarshal(resp).to[String])

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
        case StatusCodes.OK => bodyToJsObject(resp).map(x => readCollection[A](x, include).toList)
        case _              => bodyToError(resp)
      }

    new JsonApiClient[A] {
      implicit val config: UriConfig = uriConfig

      override def one(id: String, include: Set[String] = Set.empty)(implicit pt: PathToId[A]): IO[Option[A]] =
        endpoint.uri.flatMap { baseUri =>
          val reqUri = baseUri / pt.self(id)

          mkRequest(addInclude(reqUri, include).toString).flatMap(respToEntity(_, include))
        }

      override def many(ids: Set[String], include: Set[String])(implicit pt: PathToId[A]) = {
        import cats.instances.list._
        import cats.syntax.traverse._

        ids.toList.traverse { id =>
          one(id, include).flatMap {
            case Some(entity) => IO.pure(entity)
            case None         => IO.raiseError(ApiError.NoEntityForId(id, rt.resourceType))
          }
        }
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

      override def filter(filter: String, include: Set[String])(implicit pt: PathTo[A]) =
        endpoint.uri.flatMap { baseUri =>
          val reqUri = baseUri / pt.root ? ("filter" -> filter)

          mkRequest(addInclude(reqUri, include).toString).flatMap(respoToEntities(_, include))
        }

      override def post[Response](entity: A, include: Set[String])(implicit pt: PathTo[A],
                                                                   reader: JsonApiReader[Response]): IO[Response] =
        endpoint.uri.flatMap { baseUri =>
          val reqUri = baseUri / pt.entity(entity)

          mkRequest(reqUri.toString, HttpMethods.POST, HttpEntity(rawOne(entity).compactPrint)).flatMap { resp =>
            if (resp.status.isSuccess()) {
              bodyToJsObject(resp).map(readOne[Response])
            } else {
              bodyToError(resp)
            }
          }
        }

      override def put[Response](entity: A, include: Set[String])(implicit pt: PathTo[A],
                                                                  reader: JsonApiReader[Response]): IO[Response] =
        endpoint.uri.flatMap { baseUri =>
          val reqUri = baseUri / pt.entity(entity)

          mkRequest(reqUri.toString, HttpMethods.PUT, HttpEntity(rawOne(entity).compactPrint)).flatMap { resp =>
            if (resp.status.isSuccess()) {
              bodyToJsObject(resp).map(readOne[Response])
            } else {
              bodyToError(resp)
            }
          }
        }

      override def patch[Response](entity: A, include: Set[String])(implicit pt: PathTo[A],
                                                                    reader: JsonApiReader[Response]): IO[Response] =
        endpoint.uri.flatMap { baseUri =>
          val reqUri = baseUri / pt.entity(entity)

          mkRequest(reqUri.toString, HttpMethods.PATCH, HttpEntity(rawOne(entity).compactPrint)).flatMap { resp =>
            if (resp.status.isSuccess()) {
              bodyToJsObject(resp).map(readOne[Response])
            } else {
              bodyToError(resp)
            }
          }
        }

      override def delete[Response](entity: A, include: Set[String])(implicit pt: PathTo[A],
                                                                     reader: JsonApiReader[Response]): IO[Response] =
        endpoint.uri.flatMap { baseUri =>
          val reqUri = baseUri / pt.entity(entity)

          mkRequest(reqUri.toString, HttpMethods.DELETE).flatMap { resp =>
            if (resp.status.isSuccess()) {
              bodyToJsObject(resp).map(readOne[Response])
            } else {
              bodyToError(resp)
            }
          }
        }
    }
  }

  private[this] def addInclude(orig: uri.Uri, include: Set[String]): uri.Uri =
    if (include.isEmpty) {
      orig
    } else {
      orig ? ("include" -> include.mkString(","))
    }

  private[this] def mkRequest(reqUri: String,
                              method: HttpMethod = HttpMethods.GET,
                              entity: RequestEntity = HttpEntity.Empty)(implicit m: ActorMaterializer,
                                                                        system: ActorSystem): IO[HttpResponse] = {
    import system.dispatcher

    IO.fromFuture {
      IO(
        Http()
          .singleRequest(
            HttpRequest(
              uri = Uri(reqUri),
              headers = encodings,
              method = method,
              entity = entity
            )
          )
          .map(decodeResponse)
      )
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
