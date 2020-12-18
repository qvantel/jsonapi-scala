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

import scala.language.experimental.macros

import org.specs2.mutable.Specification
import _root_.spray.json.DefaultJsonProtocol._
import _root_.spray.json._
import org.specs2.matcher.MatcherMacros

final class ToManySpec extends Specification with MatcherMacros {
  implicit val apiRoot: com.qvantel.jsonapi.ApiRoot = ApiRoot(None)

  @jsonApiResource final case class Comment(id: String, content: String)
  @jsonApiResource final case class Article(id: String, title: String, comments: ToMany[Comment])

  "get" should {
    "return Some for Loaded" in {
      val comments = List(Comment("1", "1"), Comment("2", "2"))
      ToMany.loaded(comments).get must be equalTo (comments)
    }

    "return None for Reference" in {
      ToMany.reference(Set("1", "2")).get must be empty
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

      val emptyIdJson =
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
          |        "id": "",
          |        "type": "comments"
          |      }]
          |    }
          |  }
          |}
        """.stripMargin.parseJson

      implicitly[JsonApiFormat[Article]].read(idMissingJson, Set.empty) must throwA[DeserializationException](
        message = """'id' not found in \{\"type\"\:\"comments\"\}""")
      implicitly[JsonApiFormat[Article]].read(typeMissingJson, Set.empty) must throwA[DeserializationException](
        message = """'type' not found in \{\"id\"\:\"test\"\}""")
      implicitly[JsonApiFormat[Article]].read(emptyIdJson, Set.empty) must throwA[DeserializationException](
        "illegal id 'empty string' found in")
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

      implicitly[JsonApiFormat[Article]].read(json, Set.empty) must be equalTo Article("test", "boom", ToMany.reference)
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

      implicitly[JsonApiFormat[Article]].read(json, Set.empty) must be equalTo Article("test", "boom", ToMany.reference)
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

      implicitly[JsonApiFormat[Article]].read(json, Set.empty) must be equalTo Article("test", "boom", ToMany.reference)
    }

    "parse into path reference when only related links in relationship" in {
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
          |      "links": {
          |        "related": "/articles/test/comments"
          |      }
          |    }
          |  }
          |}
        """.stripMargin.parseJson

      implicitly[JsonApiFormat[Article]].read(json, Set.empty) must matchA[Article].comments(
        ToMany.reference[Comment]("/articles/test/comments"))
    }

    "fail with deserialization exception when one or more of the entities in relationship is of wrong type" in {
      val article             = Article("1", "boom", ToMany.loaded(Seq(Comment("2", "hello"), Comment("3", "world"))))
      val articleJson         = implicitly[JsonApiFormat[Article]].write(article)
      val articleIncludesJson = implicitly[JsonApiFormat[Article]].included(article)

      import _root_.spray.json.lenses.JsonLenses._

      val modifiedJson =
        articleJson.update("relationships" / "comments" / "data" / * / "type" ! set[String]("wrong-type")).asJsObject
      val modifiedIncludes =
        articleIncludesJson.map(_.update(Symbol("type") ! set[String]("wrong-type"))).map(_.asJsObject)

      implicitly[JsonApiFormat[Article]].read(modifiedJson, modifiedIncludes, Set("comments"), "") must throwA[
        DeserializationException](message = "wrong type 'comments' expected but got 'wrong-type'")
    }

    "all ids in data are references (not to be loaded)" >> {
      val article = Article("1", "boom", ToMany.reference(Set("1", "2")))
      val json    = rawOne(article)
      val parsed  = readOne[Article](json, Set("comments"))

      article must be equalTo parsed
    }

    "throw error when trying to load mixed to-many collection" >> {
      val json =
        """
          |{
          |  "data": {
          |    "attributes": {
          |      "title": "boom"
          |    },
          |    "relationships": {
          |      "comments": {
          |        "data": [{
          |          "type": "comments",
          |          "id": "2"
          |        }, {
          |          "type": "comments",
          |          "id": "3"
          |        }],
          |        "links": {
          |          "related": "/articles/1/comments"
          |        }
          |      }
          |    },
          |    "links": {
          |      "self": "/articles/1"
          |    },
          |    "id": "1",
          |    "type": "articles"
          |  },
          |  "included": [{
          |    "type": "comments",
          |    "attributes": {
          |      "content": "hello"
          |    },
          |    "id": "2",
          |    "links": {
          |      "self": "/comments/2"
          |    }
          |  }]
          |}
        """.stripMargin.parseJson.asJsObject
      readOne[Article](json, Set("comments")) must throwA[DeserializationException](
        "mixed reference and loaded types found")
    }
  }

  "write" should {
    "skip printing out data for JsonAbsent case of JsonOption[ToMany[X]]" in {
      @jsonApiResource final case class Test(id: String, opt: JsonOption[ToMany[Test]])

      val t = Test("id", JsonAbsent)

      val rawJson =
        """
          |{
          |  "data": {
          |    "relationships": {
          |      "opt": {
          |        "links": {
          |          "related": "/tests/id/opt"
          |        }
          |      }
          |    },
          |    "links": {
          |      "self": "/tests/id"
          |    },
          |    "id": "id",
          |    "type": "tests"
          |  }
          |}
        """.stripMargin.parseJson.asJsObject

      rawOne(t) must be equalTo rawJson
    }

    "print out data as null for JsonNull case of JsonOption[ToMany[X]]" in {
      @jsonApiResource final case class Test(id: String, opt: JsonOption[ToMany[Test]])

      val t = Test("id", JsonNull)

      val rawJson =
        """
          |{
          |  "data": {
          |    "relationships": {
          |      "opt": {
          |        "links": {
          |          "related": "/tests/id/opt"
          |        },
          |        "data": null
          |      }
          |    },
          |    "links": {
          |      "self": "/tests/id"
          |    },
          |    "id": "id",
          |    "type": "tests"
          |  }
          |}
        """.stripMargin.parseJson.asJsObject

      rawOne(t) must be equalTo rawJson
    }

    "work with sparse fieldsets" in {

      val article = Article("1", "boom", ToMany.loaded(Seq(Comment("2", "hello"), Comment("3", "world"))))

      implicit val sparseFields: Map[String, List[String]] =
        Map("articles" -> List("title"), "comments" -> List("fieldThatDoesNotExist"))

      val rawJson =
        """
          |{
          |  "data": {
          |    "attributes": {
          |       "title": "boom"
          |    },
          |    "links": {
          |      "self": "/articles/1"
          |    },
          |    "id": "1",
          |    "type": "articles"
          |  },
          |  "included": [
          |    {
          |      "id": "2",
          |      "links": {
          |        "self":"/comments/2"
          |      },
          |      "type": "comments"
          |    },
          |    {
          |      "id": "3",
          |      "links": {
          |        "self":"/comments/3"
          |      },
          |      "type": "comments"
          |    }
          |  ]
          |}
        """.stripMargin.parseJson.asJsObject

      rawOne(article) must be equalTo rawJson
    }
  }

  "read and write JsonOption relationship" in {
    @jsonApiResource("test", "no-id") final case class Test(name: String, x: JsonOption[ToMany[Comment]])

    val t      = Test("name", JsonAbsent)
    val json   = rawOne(t)
    val parsed = readOne[Test](json)
    parsed must be equalTo t

    val t2      = Test("name", JsonNull)
    val json2   = rawOne(t2)
    val parsed2 = readOne[Test](json2)
    parsed2 must be equalTo (t2)

    val t3      = Test("name", JsonSome(ToMany.reference[Comment](Set("test"))))
    val json3   = rawOne(t3)
    val parsed3 = readOne[Test](json3)
    parsed3 must be equalTo t3

    val comments = List(Comment("1", "1"), Comment("2", "2"))
    val t4       = Test("name", JsonSome(ToMany.loaded(comments)))
    val json4    = rawOne(t4)
    val parsed4  = readOne[Test](json4, Set("x"))
    parsed4 must be equalTo t4
  }
}
