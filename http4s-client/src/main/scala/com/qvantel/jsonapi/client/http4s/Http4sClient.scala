package com.qvantel.jsonapi.client.http4s

import cats.effect._
import cats.instances.list._
import cats.syntax.traverse._
import io.lemonlabs.uri.typesafe.dsl._
import io.lemonlabs.uri.Url
import org.http4s.Header
import org.http4s.Status.Successful
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.client.{Client, UnexpectedStatus}
import org.http4s.dsl.io._

import com.qvantel.jsonapi._
import com.qvantel.jsonapi.client.http4s.JsonApiInstances._

trait Http4sClient extends Http4sClientDsl[IO] {
  private[this] def mkIncludeString(include: Set[String]): Option[String] =
    if (include.isEmpty) {
      None
    } else {
      Some(include.mkString(","))
    }

  private[this] def httpHeaders(headers: Map[String, String]): List[Header] =
    headers.map(pair => Header(pair._1, pair._2)).toList

  implicit def instance(implicit endpoint: ApiEndpoint, client: Client[IO]): JsonApiClient = new JsonApiClient {

    override def one[A](id: String, include: Set[String] = Set.empty)(implicit pt: PathToId[A],
                                                                      reader: JsonApiReader[A]): IO[Option[A]] =
      for {
        config   <- endpoint.config
        response <- pathOne(config.url.withPath(pt.self(id).path), include)
      } yield response

    override def many[A](ids: Set[String], include: Set[String] = Set.empty)(implicit pt: PathToId[A],
                                                                             reader: JsonApiReader[A]): IO[List[A]] =
      ids.toList.traverse { id =>
        one(id, include).flatMap {
          case Some(entity) => IO.pure(entity)
          case None         => IO.raiseError(ApiError.NoEntityForId(id, pt.root.toString))
        }
      }

    override def filter[A](filter: String, include: Set[String] = Set.empty)(implicit pt: PathTo[A],
                                                                             reader: JsonApiReader[A]): IO[List[A]] = {
      implicit val _include: Include = Include(include)

      for {
        config <- endpoint.config
        uri <- IO.fromEither(
          org.http4s.Uri
            .fromString((config.url
              .withPath(pt.root.path) ? ("filter" -> filter) ? ("include" -> mkIncludeString(include))).toString))
        response <- client.expect[List[A]](GET(uri, httpHeaders(config.headers): _*))
      } yield response
    }

    override def pathOne[A](path: Url, include: Set[String] = Set.empty)(
        implicit reader: JsonApiReader[A]): IO[Option[A]] = {
      implicit val _include: Include = Include(include)

      val request = for {
        config <- endpoint.config
        uri <- IO.fromEither(
          org.http4s.Uri
            .fromString((config.url.withPath(path.path) ? ("include" -> mkIncludeString(include))).toString))
        req <- GET(uri, httpHeaders(config.headers): _*)
      } yield req

      client.fetch(request) {
        case Successful(resp) => resp.as[A].map(Some(_))
        case NotFound(_)      => IO.pure(None)
        case failedResponse   => IO.raiseError(UnexpectedStatus(failedResponse.status))
      }
    }

    override def pathMany[A](path: Url, include: Set[String] = Set.empty)(
        implicit reader: JsonApiReader[A]): IO[List[A]] = {
      implicit val _include: Include = Include(include)

      for {
        config <- endpoint.config
        uri <- IO.fromEither(
          org.http4s.Uri
            .fromString((config.url.withPath(path.path) ? ("include" -> mkIncludeString(include))).toString))
        response <- client.expect[List[A]](GET(uri, httpHeaders(config.headers): _*))
      } yield response
    }

    override def post[A, Response](entity: A, include: Set[String] = Set.empty)(
        implicit pt: PathTo[A],
        writer: JsonApiWriter[A],
        reader: JsonApiReader[Response]): IO[Response] = {
      implicit val _include: Include = Include(include)

      val request = for {
        config <- endpoint.config
        uri    <- IO.fromEither(org.http4s.Uri.fromString((config.url / pt.entity(entity)).toString))
        req    <- POST(entity, uri, httpHeaders(config.headers): _*)
      } yield req

      client.fetch(request) {
        case Successful(resp) => resp.as[Response]
        case failedResponse   => IO.raiseError(UnexpectedStatus(failedResponse.status))
      }
    }

    override def put[A, Response](entity: A, include: Set[String] = Set.empty)(
        implicit pt: PathTo[A],
        writer: JsonApiWriter[A],
        reader: JsonApiReader[Response]): IO[Response] = {
      implicit val _include: Include = Include(include)

      for {
        config   <- endpoint.config
        uri      <- IO.fromEither(org.http4s.Uri.fromString((config.url / pt.entity(entity)).toString))
        req      <- PUT(entity, uri, httpHeaders(config.headers): _*)
        response <- client.fetchAs[Response](req)
      } yield response
    }

    override def patch[A, Response](entity: A, include: Set[String] = Set.empty)(
        implicit pt: PathTo[A],
        writer: JsonApiWriter[A],
        reader: JsonApiReader[Response]): IO[Response] = {
      implicit val _include: Include = Include(include)

      for {
        config   <- endpoint.config
        uri      <- IO.fromEither(org.http4s.Uri.fromString((config.url / pt.entity(entity)).toString))
        req      <- PATCH(entity, uri, httpHeaders(config.headers): _*)
        response <- client.fetchAs[Response](req)
      } yield response
    }

    override def delete[A, Response](entity: A, include: Set[String] = Set.empty)(
        implicit pt: PathTo[A],
        reader: JsonApiReader[Response]): IO[Response] = {
      implicit val _include: Include = Include(include)

      for {
        config   <- endpoint.config
        uri      <- IO.fromEither(org.http4s.Uri.fromString((config.url / pt.entity(entity)).toString))
        req      <- DELETE(uri, httpHeaders(config.headers): _*)
        response <- client.fetchAs[Response](req)
      } yield response
    }
  }
}

object Http4sClient extends Http4sClient
