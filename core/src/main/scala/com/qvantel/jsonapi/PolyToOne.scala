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
import shapeless.Coproduct
import shapeless.ops.coproduct.Inject

sealed trait PolyToOne[A <: Coproduct] {
  def id: String
  def resourceType: String

  /** Loaded biased get method as a helper when you don't want to pattern match like crazy */
  def get: Option[A]

  def load(implicit jac: JsonApiClient, reader: JsonApiReader[A], pt: PathToId[A], rt: ResourceType[A]): IO[A]
}

object PolyToOne {
  final case class Reference[A <: Coproduct](id: String, resourceType: String) extends PolyToOne[A] {
    override def get: Option[A] = None

    override def load(implicit jac: JsonApiClient,
                      reader: JsonApiReader[A],
                      pt: PathToId[A],
                      rt: ResourceType[A]): IO[A] =
      jac.one[A](id).flatMap {
        case Some(x) => IO.pure(x)
        case None    => IO.raiseError(ApiError.NoEntityForId("id", rt.resourceType))
      }
  }

  final case class Loaded[A <: Coproduct](entity: A, id: String, resourceType: String) extends PolyToOne[A] {
    override def get: Option[A] = Some(entity)

    override def load(implicit jac: JsonApiClient,
                      reader: JsonApiReader[A],
                      pt: PathToId[A],
                      rt: ResourceType[A]): IO[A] = IO.pure(entity)
  }

  def reference[A <: Coproduct, E: ResourceType](id: String): PolyToOne[A] =
    Reference[A](id, implicitly[ResourceType[E]].resourceType)

  def loaded[A <: Coproduct, E](
      entity: E)(implicit rt: ResourceType[E], ida: Identifiable[E], inj: Inject[A, E]): PolyToOne[A] =
    Loaded[A](inj(entity), ida.identify(entity), rt.resourceType)
}
