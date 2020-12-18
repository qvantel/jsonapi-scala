package com.qvantel.jsonapi

import com.netaporter.uri.dsl._
import org.specs2.mutable._
import spray.json.DefaultJsonProtocol._
import spray.json._
import com.netaporter.uri.Uri

class PaginationSpec extends Specification {
  implicit val apiRoot: com.qvantel.jsonapi.ApiRoot = ApiRoot(Some("/api"))

  @jsonApiResource final case class Test(id: String, attr: String)

  "pagination links" >> {
    "works for simplest case" >> {
      implicit val pagination: JsonApiPagination.PaginationFunc =
        num => {
          num should be equalTo 1
          JsonApiPagination(Uri.parse("/")).withNext("number" -> "2", "size" -> "5")
        }

      val entities = Seq(Test("id", "attr"))
      val json     = rawCollection(entities)

      json.getFields("links").head must be equalTo JsObject(
        "next" -> JsString("/?page%5Bnumber%5D=2&page%5Bsize%5D=5")
      )
    }

    "removes old pagination from original uri" >> {
      implicit val pagination: JsonApiPagination.PaginationFunc =
        num => {
          num should be equalTo 3
          JsonApiPagination(Uri.parse("/api/entity?page[size]=15"))
            .withNext("number" -> "3", "size" -> "10")
            .withPrev("number" -> "1", "size" -> "10")
        }

      val entities = 1.to(3).map(i => Test(i.toString, "attr"))
      val json     = rawCollection(entities)

      json.getFields("links").head must be equalTo JsObject(
        "prev" -> JsString("/api/entity?page%5Bnumber%5D=1&page%5Bsize%5D=10"),
        "next" -> JsString("/api/entity?page%5Bnumber%5D=3&page%5Bsize%5D=10")
      )
    }

    "handle all kinds" >> {
      implicit val pagination: JsonApiPagination.PaginationFunc =
        _ =>
          JsonApiPagination(Uri.parse("/api/entity?filter=(EQ id '1')&include=relB.relC&page[size]=15"))
            .withNext("number" -> "3", "size" -> "10")
            .withPrev("number" -> "1", "size" -> "10")
            .withFirst("number" -> "1", "size" -> "10")
            .withLast("number" -> "100", "size" -> "10")

      val entities = 1.to(3).map(i => Test(i.toString, "attr"))
      val json     = rawCollection(entities)

      json.getFields("links").head must be equalTo JsObject(
        "prev" -> JsString(
          "/api/entity?filter=%28EQ%20id%20%271%27%29&include=relB.relC&page%5Bnumber%5D=1&page%5Bsize%5D=10"),
        "next" -> JsString(
          "/api/entity?filter=%28EQ%20id%20%271%27%29&include=relB.relC&page%5Bnumber%5D=3&page%5Bsize%5D=10"),
        "first" -> JsString(
          "/api/entity?filter=%28EQ%20id%20%271%27%29&include=relB.relC&page%5Bnumber%5D=1&page%5Bsize%5D=10"),
        "last" -> JsString(
          "/api/entity?filter=%28EQ%20id%20%271%27%29&include=relB.relC&page%5Bnumber%5D=100&page%5Bsize%5D=10")
      )
    }

    "defaults to empty pagination" >> {
      val entities = 1.to(3).map(i => Test(i.toString, "attr"))
      val json     = rawCollection(entities)
      json.getFields("links") must be(Seq.empty)
    }

    "is empty when only original uri is set" >> {
      implicit val pagination: JsonApiPagination.PaginationFunc = _ => JsonApiPagination(Uri.parse("/"))

      val entities = Seq(Test("id", "attr"))
      val json     = rawCollection(entities)

      json.getFields("links").head must be equalTo JsObject()
    }
  }
}
