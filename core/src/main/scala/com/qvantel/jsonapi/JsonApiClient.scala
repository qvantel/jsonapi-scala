package com.qvantel.jsonapi

import cats.effect.IO
import com.netaporter.uri.Uri

trait JsonApiClient[A] {
  def one(id: String, include: Set[String] = Set.empty)(implicit pt: PathToId[A]): IO[Option[A]]
  def many(ids: Set[String], include: Set[String] = Set.empty)(implicit pt: PathToId[A]): IO[List[A]]
  def pathOne(path: Uri, include: Set[String] = Set.empty): IO[Option[A]]
  def pathMany(path: Uri, include: Set[String] = Set.empty): IO[List[A]]
  def filter(filter: String, include: Set[String] = Set.empty)(implicit pt: PathTo[A]): IO[List[A]]

  def post[Response](entity: A, include: Set[String] = Set.empty)(implicit pt: PathTo[A],
                                                                  reader: JsonApiReader[Response],
                                                                  writer: JsonApiWriter[A]): IO[Response]
  def put[Response](entity: A, include: Set[String] = Set.empty)(implicit pt: PathTo[A],
                                                                 reader: JsonApiReader[Response],
                                                                 writer: JsonApiWriter[A]): IO[Response]
  def patch[Response](entity: A, include: Set[String] = Set.empty)(implicit pt: PathTo[A],
                                                                   reader: JsonApiReader[Response],
                                                                   writer: JsonApiWriter[A]): IO[Response]
  def delete[Response](entity: A, include: Set[String] = Set.empty)(implicit pt: PathTo[A],
                                                                    reader: JsonApiReader[Response]): IO[Response]
}

object JsonApiClient {
  def apply[A](implicit jac: JsonApiClient[A]) = implicitly[JsonApiClient[A]]
}

sealed trait ApiError extends Throwable

object ApiError {
  final case class HttpError(status: String, response: String, cause: Option[Throwable] = None) extends ApiError
  final case class NoPath(message: String, cause: Option[Throwable] = None)                     extends ApiError
  final case class NoEntityForId(id: String, rt: String, cause: Option[Throwable] = None)       extends ApiError
  final case class NoEntityForIds(ids: Set[(String, String)], cause: Option[Throwable] = None)  extends ApiError
}
