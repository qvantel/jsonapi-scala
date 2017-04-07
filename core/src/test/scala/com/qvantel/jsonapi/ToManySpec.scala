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
 * Neither the name of the <organization> nor the
      names of its contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package com.qvantel.jsonapi

import org.specs2.mutable.Specification
import _root_.spray.http.Uri.Path
import _root_.spray.json.DefaultJsonProtocol._
import _root_.spray.json._

final class ToManySpec extends Specification {
  implicit val apiRoot = ApiRoot(None)

  @jsonApiResource final case class Comment(id: String, content: String)
  @jsonApiResource final case class Article(id: String, title: String, comments: ToMany[Comment])

  "get" should {
    "return Some for Loaded" in {
      val comments = Seq(Comment("1", "1"), Comment("2", "2"))
      ToMany.loaded(comments).get must be equalTo (comments)
    }

    "return None for Reference" in {
      ToMany.reference(Set("1", "2")).get must be empty
    }
  }

  "renderRelation" should {
    "render a to-many reference relation" in {
      val article = Article("1", "boom", ToMany.reference)
      val expected = JsObject(
        "links" -> JsObject("self" -> (Path("/articles/1") / "relationships" / "comments").toJson,
                            "related" -> (Path("/articles/1") / "comments").toJson),
        "data" -> JsArray.empty
      )

      ToMany.renderRelation(article, "comments", article.comments) should be equalTo expected
    }

    "render a to-many loaded relation" in {
      val article = Article("1", "boom", ToMany.loaded(Seq(Comment("2", "hello"), Comment("3", "world"))))
      val expected = JsObject(
        "links" -> JsObject("self" -> (Path("/articles/1") / "relationships" / "comments").toJson,
                            "related" -> (Path("/articles/1") / "comments").toJson),
        "data" -> JsArray(
          JsObject("type" -> implicitly[ResourceType[Comment]].resourceType.toJson, "id" -> "2".toJson),
          JsObject("type" -> implicitly[ResourceType[Comment]].resourceType.toJson, "id" -> "3".toJson)
        )
      )

      ToMany.renderRelation(article, "comments", article.comments) should be equalTo expected
    }
  }

  "read" should {
    "throw exceptions if id or type is missing in data" in {
      val idMissingJson =
        """
          |{
          |  "id": "test",
          |  "type": "articles",
          |  "attributes": {
          |    "title": "boom"
          |  },
          |  "relationships": {
          |    "comments": {
          |      "data": [{
          |        "type": "comments"
          |      }]
          |    }
          |  }
          |}
        """.stripMargin.parseJson

      val typeMissingJson =
        """
          |{
          |  "id": "test",
          |  "type": "articles",
          |  "attributes": {
          |    "title": "boom"
          |  },
          |  "relationships": {
          |    "comments": {
          |      "data": [{
          |        "id": "test"
          |      }]
          |    }
          |  }
          |}
        """.stripMargin.parseJson

      implicitly[JsonApiFormat[Article]].read(idMissingJson, Set.empty) must throwA[DeserializationException](
        message = """'id' not found in \{\"type\"\:\"comments\"\}""")
      implicitly[JsonApiFormat[Article]].read(typeMissingJson, Set.empty) must throwA[DeserializationException](
        message = """'type' not found in \{\"id\"\:\"test\"\}""")
    }

    "parse into reference with ids when id and type is found in relationship but not in includes" in {
      val json =
        """
          |{
          |  "id": "test",
          |  "type": "articles",
          |  "attributes": {
          |    "title": "boom"
          |  },
          |  "relationships": {
          |    "comments": {
          |      "data": [{
          |        "id": "test",
          |        "type": "comments"
          |      }]
          |    }
          |  }
          |}
        """.stripMargin.parseJson

      implicitly[JsonApiFormat[Article]].read(json, Set.empty) must be equalTo Article("test",
                                                                                       "boom",
                                                                                       ToMany.reference(Set("test")))
    }

    "parse into reference when data is null" in {
      val json =
        """
          |{
          |  "id": "test",
          |  "type": "articles",
          |  "attributes": {
          |    "title": "boom"
          |  },
          |  "relationships": {
          |    "comments": {
          |      "data": null
          |    }
          |  }
          |}
        """.stripMargin.parseJson

      implicitly[JsonApiFormat[Article]].read(json, Set.empty) must be equalTo Article("test",
                                                                                       "boom",
                                                                                       ToMany.reference)
    }

    "parse into reference when a relationships content is null" in {
      val json =
        """
          |{
          |  "id": "test",
          |  "type": "articles",
          |  "attributes": {
          |    "title": "boom"
          |  },
          |  "relationships": {
          |    "comments": null
          |  }
          |}
        """.stripMargin.parseJson

      implicitly[JsonApiFormat[Article]].read(json, Set.empty) must be equalTo Article("test",
                                                                                       "boom",
                                                                                       ToMany.reference)
    }

    "parse into reference when relationships is null" in {
      val json =
        """
          |{
          |  "id": "test",
          |  "type": "articles",
          |  "attributes": {
          |    "title": "boom"
          |  },
          |  "relationships": null
          |}
        """.stripMargin.parseJson

      implicitly[JsonApiFormat[Article]].read(json, Set.empty) must be equalTo Article("test",
                                                                                       "boom",
                                                                                       ToMany.reference)
    }

    "fail with deserialization exception when one or more of the entities in relationship is of wrong type" in {
      val article             = Article("1", "boom", ToMany.loaded(Seq(Comment("2", "hello"), Comment("3", "world"))))
      val articleJson         = implicitly[JsonApiFormat[Article]].write(article)
      val articleIncludesJson = implicitly[JsonApiFormat[Article]].included(article)

      import _root_.spray.json.lenses.JsonLenses._

      val modifiedJson =
        articleJson.update('relationships / 'comments / 'data / * / 'type ! set[String]("wrong-type")).asJsObject
      val modifiedIncludes = articleIncludesJson.map(_.update('type ! set[String]("wrong-type"))).map(_.asJsObject)

      implicitly[JsonApiFormat[Article]].read(modifiedJson, modifiedIncludes, Set("comments"), "") must throwA[
        DeserializationException](message = "wrong type 'comments' expected but got 'wrong-type'")
    }
  }
}
