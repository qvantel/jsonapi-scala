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
package com.qvantel.jsonapi

import cats.effect.IO
import io.lemonlabs.uri.Url
import io.lemonlabs.uri.typesafe.dsl._

/**
  * Represents a relationship to zero or more objects of type A
  * [[com.qvantel.jsonapi.ToMany.IdsReference]] case class is used to represent a ToMany relationship where
  * the objects have not been loaded
  * [[com.qvantel.jsonapi.ToMany.Loaded]] case class is used to represent a ToMany relationship where
  * the objects have been loaded
  *
  * @tparam A Type of the object the relationships point to
  */
sealed trait ToMany[A] {
  def ids: Set[String]

  /** Loaded biased get method as a helper when you don't want to pattern match like crazy */
  def get: List[A]

  def load(implicit jac: JsonApiClient,
           rt: ResourceType[A],
           identifiable: Identifiable[A],
           pt: PathToId[A],
           reader: JsonApiReader[A]): IO[List[A]]
}

object ToMany {
  final case class IdsReference[A](ids: Set[String]) extends ToMany[A] {
    override def get: List[A] = List.empty

    def load(implicit jac: JsonApiClient,
             rt: ResourceType[A],
             identifiable: Identifiable[A],
             pt: PathToId[A],
             reader: JsonApiReader[A]): IO[List[A]] =
      jac.many[A](ids).flatMap { entities =>
        entities.filterNot(x => ids(identifiable.identify(x))) match {
          case Nil => IO.pure(entities)
          case missing =>
            IO.raiseError(ApiError.NoEntityForIds(missing.map(x => (identifiable.identify(x), rt.resourceType)).toSet))
        }
      }
  }

  final case class PathReference[A](path: Option[Url]) extends ToMany[A] {
    override def ids: Set[String] = Set.empty

    /** Loaded biased get method as a helper when you don't want to pattern match like crazy */
    override def get: List[A] = List.empty

    def load(implicit jac: JsonApiClient,
             rt: ResourceType[A],
             identifiable: Identifiable[A],
             pt: PathToId[A],
             reader: JsonApiReader[A]): IO[List[A]] =
      path match {
        case Some(url) => jac.pathMany[A](url)
        case None      => IO.pure(List.empty)
      }
  }

  final case class Loaded[A: Identifiable](entities: Iterable[A]) extends ToMany[A] {
    val ids: Set[String] = entities.map(implicitly[Identifiable[A]].identify).toSet

    override def get: List[A] = entities.toList

    def load(implicit jac: JsonApiClient,
             rt: ResourceType[A],
             identifiable: Identifiable[A],
             pt: PathToId[A],
             reader: JsonApiReader[A]): IO[List[A]] =
      IO.pure(entities.toList)
  }

  def reference[A]: ToMany[A]                                   = PathReference[A](None)
  def reference[A](ids: Set[String]): ToMany[A]                 = IdsReference[A](ids)
  def reference[A](url: Url): ToMany[A]                         = PathReference[A](Some(url))
  def reference[A](url: String): ToMany[A]                      = PathReference[A](Some(url))
  def loaded[A: Identifiable](entities: Iterable[A]): ToMany[A] = Loaded[A](entities)
}
