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

import scala.concurrent.Future
import com.qvantel
import monix.eval.Task

import com.qvantel.jsonapi

/**
  * Represents a relationship to object A
  * [[com.qvantel.jsonapi.ToOne.Reference]] case class is used when A is not loaded but we know it's id.
  * In case we do not know the id of the other end of the relationship wrap the relation in option and fill it with None
  * [[com.qvantel.jsonapi.ToOne.Loaded]] case class is used when A is loaded
  *
  * @tparam A Type of the object the relationship points to
  */
sealed trait ToOne[A] {
  def fold[B: Identifiable](fId: String => String, fEntity: A => B): ToOne[B]
  def id: String

  /** Loaded biased get method as a helper when you don't want to pattern match like crazy */
  def get: Option[A]

  def load(implicit jac: JsonApiClient[A], rt: ResourceType[A]): Task[A]
}

object ToOne {
  final case class Reference[A](id: String) extends ToOne[A] {
    override def fold[B: Identifiable](fId: String => String, _fEntity: A => B): ToOne[B] =
      ToOne.reference(fId(id))

    override def get: Option[A] = None

    override def load(implicit jac: JsonApiClient[A], rt: ResourceType[A]): Task[A] =
      jac.one(id).map(_.getOrElse(throw ApiError.NoEntityForId("id", rt)))
  }

  final case class Loaded[A: Identifiable](entity: A) extends ToOne[A] {
    override val id = implicitly[Identifiable[A]].identify(entity)
    override def fold[B: Identifiable](_fId: String => String, fEntity: A => B): ToOne[B] =
      ToOne.loaded(fEntity(entity))

    override def get: Option[A] = Some(entity)

    override def load(implicit jac: JsonApiClient[A], rt: ResourceType[A]): Task[A] = Task.now(entity)
  }

  def reference[A](id: String): ToOne[A]           = Reference(id)
  def loaded[A: Identifiable](entity: A): ToOne[A] = Loaded(entity)
}
