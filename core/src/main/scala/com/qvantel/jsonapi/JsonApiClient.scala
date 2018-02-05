package com.qvantel.jsonapi

import cats.effect.IO
import com.netaporter.uri.Uri

trait JsonApiClient {
  def one[A](id: String, include: Set[String] = Set.empty)(implicit pt: PathToId[A],
                                                           reader: JsonApiReader[A]): IO[Option[A]]
  def many[A](ids: Set[String], include: Set[String] = Set.empty)(implicit pt: PathToId[A],
                                                                  reader: JsonApiReader[A]): IO[List[A]]
  def pathOne[A](path: Uri, include: Set[String] = Set.empty)(implicit reader: JsonApiReader[A]): IO[Option[A]]
  def pathMany[A](path: Uri, include: Set[String] = Set.empty)(implicit reader: JsonApiReader[A]): IO[List[A]]
  def filter[A](filter: String, include: Set[String] = Set.empty)(implicit pt: PathTo[A],
                                                                  reader: JsonApiReader[A]): IO[List[A]]

  def post[A, Response](entity: A, include: Set[String] = Set.empty)(implicit pt: PathTo[A],
                                                                     writer: JsonApiWriter[A],
                                                                     reader: JsonApiReader[Response]): IO[Response]
  def put[A, Response](entity: A, include: Set[String] = Set.empty)(implicit pt: PathTo[A],
                                                                    writer: JsonApiWriter[A],
                                                                    reader: JsonApiReader[Response]): IO[Response]
  def patch[A, Response](entity: A, include: Set[String] = Set.empty)(implicit pt: PathTo[A],
                                                                      writer: JsonApiWriter[A],
                                                                      reader: JsonApiReader[Response]): IO[Response]
  def delete[A, Response](entity: A, include: Set[String] = Set.empty)(implicit pt: PathTo[A],
                                                                       reader: JsonApiReader[Response]): IO[Response]
}

object JsonApiClient {
  def instance(implicit jac: JsonApiClient) = jac
}

sealed trait ApiError extends Throwable

object ApiError {
  final case class HttpError(status: String, response: String, cause: Option[Throwable] = None) extends ApiError
  final case class NoPath(message: String, cause: Option[Throwable] = None)                     extends ApiError
  final case class NoEntityForId(id: String, rt: String, cause: Option[Throwable] = None)       extends ApiError
  final case class NoEntityForIds(ids: Set[(String, String)], cause: Option[Throwable] = None)  extends ApiError
}
