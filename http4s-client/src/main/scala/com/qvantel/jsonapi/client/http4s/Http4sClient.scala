package com.qvantel.jsonapi.client.http4s

import cats.Applicative
import cats.effect._
import com.netaporter.uri.config.UriConfig
import com.netaporter.uri.{Uri => CoreUri}
import com.netaporter.uri.dsl._
import org.http4s.client.Client
import org.http4s.dsl.io._

import com.qvantel.jsonapi._
import com.qvantel.jsonapi.client.http4s.JsonApiInstances._

trait Http4sClient {
  def apply[A](implicit jac: JsonApiClient[A]) = implicitly[JsonApiClient[A]]

  private[this] def mkIncludeString(include: Set[String]): Option[String] =
    if (include.isEmpty) {
      None
    } else {
      Some(include.mkString(","))
    }

  implicit def instance[A](implicit rt: ResourceType[A],
                           reader: JsonApiReader[A],
                           identifiable: Identifiable[A],
                           endpoint: ApiEndpoint,
                           client: Client[IO]): JsonApiClient[A] = new JsonApiClient[A] {

    implicit val uConfig: UriConfig = uriConfig

    override def one(id: String, include: Set[String]): IO[Option[A]] =
      for {
        baseUri  <- endpoint.uri
        response <- pathOne(baseUri / rt.resourceType / id, include)
      } yield response

    override def many(ids: Set[String], include: Set[String]): IO[List[A]] = {
      import cats.instances.list._

      Applicative[IO].traverse(ids.toList) { id =>
        one(id, include).flatMap {
          case Some(entity) => IO.pure(entity)
          case None         => IO.raiseError(ApiError.NoEntityForId(id, rt.resourceType))
        }
      }
    }

    override def filter(filter: String, include: Set[String]): IO[List[A]] = {
      implicit val _include: Include = Include(include)

      for {
        baseUri <- endpoint.uri
        uri <- IO.fromEither(
          org.http4s.Uri.fromString(
            (baseUri / rt.resourceType ? ("filter" -> filter) ? ("include" -> mkIncludeString(include))).toString))
        response <- client.expect[List[A]](uri)
      } yield response
    }

    override def pathOne(path: CoreUri, include: Set[String]): IO[Option[A]] = {
      implicit val _include: Include = Include(include)

      for {
        baseUri <- endpoint.uri
        uri <- IO.fromEither(
          org.http4s.Uri
            .fromString((baseUri.copy(pathParts = path.pathParts) ? ("include" -> mkIncludeString(include))).toString))
        response <- client.expect[Option[A]](uri)
      } yield response
    }

    override def pathMany(path: CoreUri, include: Set[String]): IO[List[A]] = {
      implicit val _include: Include = Include(include)

      for {
        baseUri <- endpoint.uri
        uri <- IO.fromEither(
          org.http4s.Uri
            .fromString((baseUri.copy(pathParts = path.pathParts) ? ("include" -> mkIncludeString(include))).toString))
        response <- client.expect[List[A]](uri)
      } yield response
    }
  }
}

object Http4sClient extends Http4sClient
