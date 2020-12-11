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

import org.specs2.mutable.Specification
import _root_.spray.json.DefaultJsonProtocol._
import _root_.spray.json._

final class ToOneSpec extends Specification {
  implicit val apiRoot: com.qvantel.jsonapi.ApiRoot = ApiRoot(None)

  @jsonApiResource final case class Author(id: String, name: String)
  @jsonApiResource final case class Article(id: String, title: String, author: ToOne[Author])
  @jsonApiResource final case class Maybe(id: String, author: Option[ToOne[Author]])

  "get" should {
    "return Some for Loaded" in {
      val author = Author("id", "name")
      ToOne.loaded(author).get must beSome(author)
    }

    "return None for Reference" in {
      ToOne.reference("id").get must beNone
    }
  }

  "read" should {
    "throw exception when id or type is missing from relation" in {
      val properJson =
        """
          |{
          |  "id": "test",
          |  "type": "articles",
          |  "attributes": {
          |    "title": "boom"
          |  },
          |  "relationships": {
          |    "author": {
          |      "data": {
          |        "id": "test",
          |        "type": "authors"
          |      }
          |    }
          |  }
          |}
        """.stripMargin.parseJson

      val idMissingJson =
        """
          |{
          |  "id": "test",
          |  "type": "articles",
          |  "attributes": {
          |    "title": "boom"
          |  },
          |  "relationships": {
          |    "author": {
          |      "data": {
          |        "type": "authors"
          |      }
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
          |    "author": {
          |      "data": {
          |        "id": "test"
          |      }
          |    }
          |  }
          |}
        """.stripMargin.parseJson

      implicitly[JsonApiFormat[Article]].read(properJson, Set.empty) must be equalTo Article("test",
                                                                                             "boom",
                                                                                             ToOne.reference("test"))
      implicitly[JsonApiFormat[Article]].read(idMissingJson, Set.empty) must throwA[DeserializationException](
        message = "id and type expected")
      implicitly[JsonApiFormat[Article]].read(typeMissingJson, Set.empty) must throwA[DeserializationException](
        message = "id and type expected")
    }

    "throw exception when data is missing from relation" in {
      val properJson =
        """
          |{
          |  "id": "test",
          |  "type": "articles",
          |  "attributes": {
          |    "title": "boom"
          |  },
          |  "relationships": {
          |    "author": {
          |      "data": {
          |        "id": "test",
          |        "type": "authors"
          |      }
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
          |    "author": {
          |      "data": {
          |        "id": "",
          |        "type": "authors"
          |      }
          |    }
          |  }
          |}
        """.stripMargin.parseJson

      val missingDataJson =
        """
          |{
          |  "id": "test",
          |  "type": "articles",
          |  "attributes": {
          |    "title": "boom"
          |  },
          |  "relationships": {
          |    "author": {
          |      "id": "test",
          |      "type": "authors"
          |    }
          |  }
          |}
        """.stripMargin.parseJson

      val properJsonOptional =
        """
          |{
          |  "id": "test",
          |  "type": "maybes",
          |  "relationships": {
          |    "author": {
          |      "data": {
          |        "id": "test",
          |        "type": "authors"
          |      }
          |    }
          |  }
          |}
        """.stripMargin.parseJson

      val missingDataJsonOptional =
        """
          |{
          |  "id": "test",
          |  "type": "maybes",
          |  "relationships": {
          |    "author": {
          |      "id": "test",
          |      "type": "authors"
          |    }
          |  }
          |}
        """.stripMargin.parseJson

      val dataNullJsonOptional =
        """
          |{
          |  "id": "test",
          |  "type": "maybes",
          |  "relationships": {
          |    "author": {
          |      "data": null
          |    }
          |  }
          |}
        """.stripMargin.parseJson

      val authorNullJsonOptional =
        """
          |{
          |  "id": "test",
          |  "type": "maybes",
          |  "relationships": {
          |    "author": null
          |  }
          |}
        """.stripMargin.parseJson

      val authorNullArticle =
        """
          |{
          |  "id": "test",
          |  "type": "articles",
          |  "attributes": {
          |    "title": "boom"
          |  },
          |  "relationships": {
          |    "author": null
          |  }
          |}
        """.stripMargin.parseJson

      implicitly[JsonApiFormat[Article]].read(properJson, Set.empty) must be equalTo Article("test",
                                                                                             "boom",
                                                                                             ToOne.reference("test"))
      implicitly[JsonApiFormat[Article]].read(emptyIdJson, Set.empty) must throwA[DeserializationException](
        "illegal id 'empty string' found")
      implicitly[JsonApiFormat[Article]].read(missingDataJson, Set.empty) must throwA[DeserializationException](
        message = "expected 'data' in 'author' in relationships json")
      implicitly[JsonApiFormat[Maybe]].read(properJsonOptional, Set.empty) must be equalTo Maybe(
        "test",
        Some(ToOne.reference("test")))
      implicitly[JsonApiFormat[Maybe]].read(missingDataJsonOptional, Set.empty) must throwA[DeserializationException](
        message = "expected 'data', 'links' or 'meta' in 'author' in relationships json")
      implicitly[JsonApiFormat[Maybe]].read(dataNullJsonOptional, Set.empty) must be equalTo Maybe("test", None)
      implicitly[JsonApiFormat[Maybe]].read(authorNullJsonOptional, Set.empty) must be equalTo Maybe("test", None)
      implicitly[JsonApiFormat[Article]].read(authorNullArticle, Set.empty) must throwA[DeserializationException]
    }

    "fail with deserialization exception when the entity in the relationship is of wrong type" in {
      val article             = Article("1", "boom", ToOne.loaded(Author("john", "doe")))
      val articleJson         = implicitly[JsonApiFormat[Article]].write(article)
      val articleIncludesJson = implicitly[JsonApiFormat[Article]].included(article)

      import _root_.spray.json.lenses.JsonLenses._

      val modifiedArticleJson =
        articleJson.update("relationships" / "author" / "data" / "type" ! set[String]("wrong-type")).asJsObject
      val modifiedArticleIncludes =
        articleIncludesJson.map(_.update(Symbol("type") ! set[String]("wrong-type"))).map(_.asJsObject)

      implicitly[JsonApiFormat[Article]].read(modifiedArticleJson, modifiedArticleIncludes, Set("author"), "") must throwA[
        DeserializationException](message = "wrong type 'authors' expected but got 'wrong-type'")

      val maybe             = Maybe("1", Some(ToOne.loaded[Author](Author("test-id", "mario"))))
      val maybeJson         = implicitly[JsonApiFormat[Maybe]].write(maybe)
      val maybeIncludedJson = implicitly[JsonApiFormat[Maybe]].included(maybe)

      val modifiedMaybeJson =
        maybeJson.update("relationships" / "author" / "data" / "type" ! set[String]("wrong-type")).asJsObject
      val modifiedMaybeIncludes =
        maybeIncludedJson.map(_.update(Symbol("type") ! set[String]("wrong-type"))).map(_.asJsObject)

      implicitly[JsonApiFormat[Maybe]].read(modifiedMaybeJson, modifiedMaybeIncludes, Set("author"), "") must throwA[
        DeserializationException](message = "wrong type 'authors' expected but got 'wrong-type'")

      val wrongToOneTypeJson =
        """
          |{
          |  "id": "test",
          |  "type": "articles",
          |  "attributes": {
          |    "title": "boom"
          |  },
          |  "relationships": {
          |    "author": {
          |      "data": {
          |        "id": "test",
          |        "type": "foobar"
          |      }
          |    }
          |  }
          |}
        """.stripMargin.parseJson

      implicitly[JsonApiFormat[Article]].read(wrongToOneTypeJson, Set.empty) must throwA[DeserializationException](
        message = "wrong type 'authors' expected but got 'foobar'")
    }

    "handle nested structures" in {
      @jsonApiResource final case class Agreement(id: String)
      @jsonApiResource final case class CustomerAccount(id: String, agreement: ToOne[Agreement])
      @jsonApiResource final case class Organization(id: String, customerAccount: ToOne[CustomerAccount])

      val data = Organization("o", ToOne.loaded(CustomerAccount("ca", ToOne.loaded(Agreement("a")))))

      val json   = rawOne(data)
      val parsed = readOne[Organization](json, Set("customer-account.agreement"))

      data must be equalTo parsed
    }
  }

  "write" should {
    "print out data as null for None case of Option[ToOne[X]]" in {
      @jsonApiResource final case class Test(id: String, opt: Option[ToOne[Test]])

      val t = Test("id", None)

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

    "skip printing out data for JsonAbsent case of JsonOption[ToOne[X]]" in {
      @jsonApiResource final case class Test(id: String, opt: JsonOption[ToOne[Test]])

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

    "print out data as null for JsonNull case of JsonOption[ToOne[X]]" in {
      @jsonApiResource final case class Test(id: String, opt: JsonOption[ToOne[Test]])

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
      @jsonApiResource final case class Agreement(id: String, name: String, amount: Int)
      @jsonApiResource final case class CustomerAccount(id: String,
                                                        name: String,
                                                        amount: Int,
                                                        agreement: ToOne[Agreement])

      val data = CustomerAccount("customer-account-id",
                                 "customer-account-name",
                                 1,
                                 ToOne.loaded(Agreement("agreement-id", "agreement-name", 2)))
      implicit val sparseFields: Map[String, List[String]] =
        Map("customer-accounts" -> List("name", "agreement"), "agreements" -> List("amount"))

      val rawJson =
        """
          |{
          |  "data": {
          |    "attributes": {
          |       "name": "customer-account-name"
          |    },
          |    "relationships": {
          |      "agreement": {
          |        "links": {
          |          "related": "/customer-accounts/customer-account-id/agreement"
          |        },
          |        "data": {
          |          "id": "agreement-id",
          |          "type": "agreements"
          |        }
          |      }
          |    },
          |    "links": {
          |      "self": "/customer-accounts/customer-account-id"
          |    },
          |    "id": "customer-account-id",
          |    "type": "customer-accounts"
          |  },
          |  "included": [
          |    {
          |      "attributes": {
          |        "amount": 2
          |      },
          |      "id": "agreement-id",
          |      "links": {
          |        "self":"/agreements/agreement-id"
          |      },
          |      "type": "agreements"
          |    }
          |  ]
          |}
        """.stripMargin.parseJson.asJsObject

      rawOne(data) must be equalTo rawJson
    }
  }

  "read and write JsonOption relationship" in {
    @jsonApiResource("test", "no-id") final case class Test(name: String, x: JsonOption[ToOne[Author]])

    val t      = Test("name", JsonAbsent)
    val json   = rawOne(t)
    val parsed = readOne[Test](json)
    parsed must be equalTo t

    val t2      = Test("name", JsonNull)
    val json2   = rawOne(t2)
    val parsed2 = readOne[Test](json2)
    parsed2 must be equalTo (t2)

    val t3      = Test("name", JsonSome(ToOne.reference[Author]("test")))
    val json3   = rawOne(t3)
    val parsed3 = readOne[Test](json3)
    parsed3 must be equalTo t3

    val t4      = Test("name", JsonSome(ToOne.loaded(Author("id", "name"))))
    val json4   = rawOne(t4)
    val parsed4 = readOne[Test](json4, Set("x"))
    parsed4 must be equalTo t4
  }
}
