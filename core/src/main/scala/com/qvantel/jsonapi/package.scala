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
package com.qvantel

import scala.language.experimental.macros

import scala.annotation.compileTimeOnly

import _root_.spray.json.DefaultJsonProtocol._
import _root_.spray.json._
import io.lemonlabs.uri.Url
import io.lemonlabs.uri.config.UriConfig
import io.lemonlabs.uri.encoding.PercentEncoder
import io.lemonlabs.uri.typesafe.dsl._
import shapeless.{:+:, CNil, Coproduct, Inl, Inr}

package object jsonapi {
  type NameMangler = String => String

  implicit val uriConfig: UriConfig = UriConfig(encoder = PercentEncoder() ++ '"')

  implicit object PathJsonFormat extends JsonFormat[Url] {
    override def write(obj: Url): JsValue = JsString(obj.toString)
    override def read(json: JsValue): Url = json match {
      case JsString(s) => s
      case other       => deserializationError(s"Expected Path as JsString but got ‘$other’")
    }
  }

  @compileTimeOnly("Macros can only be used at compile-time") def jsonApiWriter[A]: JsonApiFormat[A] =
    macro Macros.jsonApiWriterWithNoNameManglingImpl[A]
  @compileTimeOnly("Macros can only be used at compile-time") def jsonApiFormat[A]: JsonApiFormat[A] =
    macro Macros.jsonApiFormatWithNoNameManglingImpl[A]

  @compileTimeOnly("Macros can only be used at compile-time") def includes[A]: Includes[A] =
    macro Macros.createIncludes[A]

  implicit def coproductJsonApiWriter0[L](implicit wl: JsonApiWriter[L]): JsonApiWriter[L :+: CNil] =
    new JsonApiWriter[L :+: CNil] {
      override def included(obj: L :+: CNil, sparseFields: Map[String, List[String]] = Map.empty): Set[JsObject] =
        obj match {
          case Inl(l) => wl.included(l, sparseFields)
          case Inr(_) => ???
        }

      override def write(obj: L :+: CNil, sparseFields: Map[String, List[String]]): JsValue = obj match {
        case Inl(l) => wl.write(l, sparseFields)
        case Inr(_) => ???
      }
    }

  implicit def coproductJsonApiWriter1[L, R <: Coproduct](implicit wl: JsonApiWriter[L],
                                                          wr: JsonApiWriter[R]): JsonApiWriter[L :+: R] =
    new JsonApiWriter[L :+: R] {
      override def included(obj: L :+: R, sparseFields: Map[String, List[String]] = Map.empty): Set[JsObject] =
        obj match {
          case Inl(l) => wl.included(l, sparseFields)
          case Inr(r) => wr.included(r, sparseFields)
        }

      override def write(obj: L :+: R, sparseFields: Map[String, List[String]]): JsValue = obj match {
        case Inl(l) => wl.write(l, sparseFields)
        case Inr(r) => wr.write(r, sparseFields)
      }

    }

  private[this] def addMetaProfiles(obj: JsObject, metaProfiles: Set[MetaProfile]): JsObject =
    if (metaProfiles.isEmpty) {
      obj
    } else {
      val aliases: Map[String, JsValue] = Map(
        "aliases" -> JsObject(metaProfiles.map(x => x.alias -> JsString(x.link.toString)).toList: _*))
      val oldLinks: Map[String, JsValue] = obj.fields.get("links").map(_.asJsObject.fields).getOrElse(Map.empty)
      val newLinks: Map[String, JsValue] = Map(
        "profile" -> JsArray(metaProfiles.map(x => JsString(x.link.toString)).toVector))
      val links: Map[String, JsValue] = Map("links" -> JsObject(oldLinks ++ newLinks))
      JsObject(obj.fields ++ aliases ++ links)
    }

  private[this] def addTopPagination(obj: JsObject, pagination: JsonApiPagination): JsObject =
    pagination match {
      case JsonApiPagination.Empty => obj
      case paging =>
        val oldLinks: Map[String, JsValue]  = obj.fields.get("links").map(_.asJsObject.fields).getOrElse(Map.empty)
        val pageLinks: Map[String, JsValue] = paging.allLinksAsUrls.mapValues(url => PathJsonFormat.write(url)).toMap
        val links: Map[String, JsValue]     = Map("links" -> JsObject(oldLinks ++ pageLinks))
        JsObject(obj.fields ++ links)
    }

  def rawOne[T](entity: T)(implicit writer: JsonApiWriter[T],
                           metaProfiles: Set[MetaProfile] = Set.empty,
                           sorting: JsonApiSorting = JsonApiSorting.Unsorted,
                           sparseFields: Map[String, List[String]] = Map.empty): JsObject = {
    val primaryData = writer.write(entity, sparseFields)
    val included    = writer.included(entity, sparseFields)
    val res = if (included.nonEmpty) {
      sorting match {
        case JsonApiSorting.Unsorted =>
          JsObject(("data", primaryData), ("included", JsArray(included.toVector)))
        case s =>
          JsObject(("data", primaryData), ("included", JsArray(s.sort(included).toVector)))
      }
    } else {
      JsObject(("data", primaryData))
    }

    addMetaProfiles(res, metaProfiles)
  }

  def rawCollection[T](entities: Iterable[T])(
      implicit writer: JsonApiWriter[T],
      metaProfiles: Set[MetaProfile] = Set.empty,
      sorting: JsonApiSorting = JsonApiSorting.Unsorted,
      sparseFields: Map[String, List[String]] = Map.empty,
      pagination: JsonApiPagination.PaginationFunc = JsonApiPagination.EmptyFunc): JsObject = {
    val primary  = entities.map(x => writer.write(x, sparseFields).asJsObject)
    val included = entities.map(entity => writer.included(entity, sparseFields)).foldLeft(Set.empty[JsObject])(_ ++ _)
    val res = if (included.nonEmpty) {
      sorting match {
        case JsonApiSorting.Unsorted =>
          JsObject(("data", JsArray(primary.toVector)), ("included", JsArray(included.toVector)))
        case s =>
          JsObject(("data", JsArray(s.sort(primary).toVector)), ("included", JsArray(s.sort(included).toVector)))
      }
    } else {
      sorting match {
        case JsonApiSorting.Unsorted =>
          JsObject(("data", JsArray(primary.toVector)))
        case s =>
          JsObject(("data", JsArray(s.sort(primary).toVector)))
      }
    }

    addMetaProfiles(res, metaProfiles)
    addTopPagination(res, pagination(entities.size))
  }

  /** Reads one jsonapi entity. Due to no includes path being provided includes are ignored.
    */
  def readOne[T](json: JsObject)(implicit reader: JsonApiReader[T]): T = {
    val dataJson = json.fields("data")

    reader.read(dataJson, Map.empty[(String, String), JsObject], Set.empty[String], "")
  }

  /** Reads one jsonapi entity. Due to include paths being provided includes will be handled.
    */
  def readOne[T](json: JsObject, includes: Set[String])(implicit reader: JsonApiReader[T]): T = {
    val dataJson = json.fields("data").asJsObject
    val includedJson = json.fields
      .get("included")
      .filterNot(_.isInstanceOf[JsNull.type])
      .map(_.convertTo[Set[JsObject]])
      .getOrElse(Set.empty)

    val includedWithData = if (dataJson.fields.contains("id")) {
      includedJson + dataJson
    } else {
      includedJson
    }

    reader.read(dataJson, includedWithData, fillIncludes(includes), "")
  }

  /** Reads a collection of jsonapi entities. Due to no includes path being provided includes are ignored.
    */
  def readCollection[T](json: JsObject)(implicit reader: JsonApiReader[T]): Iterable[T] = {
    val dataJson = json.fields
      .get("data")
      .filterNot(_.isInstanceOf[JsNull.type])
      .map(_.convertTo[Set[JsObject]])
      .getOrElse(Set.empty)

    dataJson.map(x => reader.read(x, Map.empty[(String, String), JsObject], Set.empty[String], ""))
  }

  /** Reads a collection of jsonapi entities. Due to include paths being provided includes will be handled.
    */
  def readCollection[T](json: JsObject, includes: Set[String])(implicit reader: JsonApiReader[T]): Iterable[T] = {
    val dataJson = json.fields
      .get("data")
      .filterNot(_.isInstanceOf[JsNull.type])
      .map(_.convertTo[Set[JsObject]])
      .getOrElse(Set.empty)
    val includedJson = json.fields
      .get("included")
      .filterNot(_.isInstanceOf[JsNull.type])
      .map(_.convertTo[Set[JsObject]])
      .getOrElse(Set.empty)

    val includedWithData = includedJson ++ dataJson.filter(_.fields.contains("id"))

    dataJson.map(x => reader.read(x, includedWithData, fillIncludes(includes), ""))
  }

  private[this] def fillIncludes(includes: Set[String]): Set[String] = {
    def mkInclude(path: Option[String], include: String) = path.map(_ + ".").getOrElse("") + include

    def go(include: String, coll: Set[String], path: Option[String]): Set[String] =
      if (include.isEmpty) {
        Set.empty
      } else {
        include.split('.').toList match {
          case Nil         => coll
          case head :: Nil => coll + mkInclude(path, head)
          case head :: tail =>
            val p = mkInclude(path, head)
            go(tail.mkString("."), coll, Some(p)) + p
        }
      }
    includes.flatMap(go(_, Set.empty, None))
  }
}
