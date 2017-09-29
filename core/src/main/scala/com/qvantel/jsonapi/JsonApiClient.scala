package com.qvantel.jsonapi

import scala.language.existentials

import com.netaporter.uri.Uri
import monix.eval.Task

trait JsonApiClient[A] {
  def one(id: String, include: Set[String] = Set.empty): Task[Option[A]]
  def many(ids: Set[String], include: Set[String] = Set.empty): Task[List[Either[String, A]]]
  def pathOne(path: Uri, include: Set[String] = Set.empty): Task[Option[A]]
  def pathMany(path: Uri, include: Set[String] = Set.empty): Task[List[A]]
  def filter(filter: String, include: Set[String] = Set.empty): Task[List[A]]
}

object JsonApiClient {
  def apply[A](implicit jac: JsonApiClient[A]) = implicitly[JsonApiClient[A]]
}

sealed trait ApiError extends Throwable

object ApiError {
  final case class HttpError(status: String, response: String, cause: Option[Throwable] = None)    extends ApiError
  final case class NoPath(message: String, cause: Option[Throwable] = None)                        extends ApiError
  final case class NoEntityForId(id: String, rt: ResourceType[_], cause: Option[Throwable] = None) extends ApiError
}
