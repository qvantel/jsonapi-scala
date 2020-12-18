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
import shapeless.{:+:, CNil, Coproduct, Poly1}
import _root_.spray.json.DefaultJsonProtocol._
import _root_.spray.json._

import com.qvantel.jsonapi.PolyToMany.PolyWrongTypeException

final class PolyToManySpec extends Specification {
  implicit val apiRoot: com.qvantel.jsonapi.ApiRoot = ApiRoot(None)

  @jsonApiResource final case class Person(id: String, name: String)
  @jsonApiResource final case class Company(id: String, name: String)
  type Author = Person :+: Company :+: CNil
  private[this] implicit object AuthorPolyIdentifiable extends PolyIdentifiable[Author] {
    private[this] object polyIdentify extends Poly1 {
      implicit def casePerson  = at[Person](obj => obj.id)
      implicit def caseCompany = at[Company](obj => obj.id)
    }

    private[this] object polyResourceType extends Poly1 {
      implicit def casePerson  = at[Person](_ => implicitly[ResourceType[Person]].resourceType)
      implicit def caseCompany = at[Company](_ => implicitly[ResourceType[Company]].resourceType)
    }

    override def identify(a: Author): String     = a fold polyIdentify
    override def resourceType(a: Author): String = a fold polyResourceType
  }

  @jsonApiResource final case class Article(id: String, title: String, authors: PolyToMany[Author])

  type Looped = Loop :+: Person :+: TestThing :+: CNil
  private[this] implicit object LoopedPolyIdentifiable extends PolyIdentifiable[Looped] {
    private[this] object polyIdentify extends Poly1 {
      implicit def caseLoop      = at[Loop](obj => obj.id)
      implicit def casePerson    = at[Person](obj => obj.id)
      implicit def caseTestThing = at[TestThing](obj => obj.id)
    }

    private[this] object polyResourceType extends Poly1 {
      implicit def caseLoop      = at[Loop](_ => implicitly[ResourceType[Loop]].resourceType)
      implicit def casePerson    = at[Person](_ => implicitly[ResourceType[Person]].resourceType)
      implicit def caseTestThing = at[TestThing](_ => implicitly[ResourceType[TestThing]].resourceType)
    }

    override def identify(a: Looped): String     = a fold polyIdentify
    override def resourceType(a: Looped): String = a fold polyResourceType
  }

  @jsonApiResource final case class Loop(id: String, looped: PolyToMany[Looped])
  @jsonApiResource final case class TestThing(id: String, foo: Option[PolyToOne[Looped]])

  "get" should {
    "return Some for Loaded" in {
      val comments = Seq(Coproduct[Author](Person("1", "1")), Coproduct[Author](Person("2", "2")))
      PolyToMany.loaded(comments).get must be equalTo (comments)
    }

    "return None for Reference" in {
      val rt = implicitly[ResourceType[Person]].resourceType
      PolyToMany.reference[Author](Set("1" -> rt, "2" -> rt)).get must be empty
    }
  }

  "read" should {
    "properly read to-many reference" in {
      val article     = Article("1", "boom", PolyToMany.reference[Author]("/articles/1"))
      val articleJson = implicitly[JsonApiFormat[Article]].write(article)

      implicitly[JsonApiFormat[Article]].read(articleJson, Set.empty) must be equalTo article
    }

    "properly read to-many reference ids" in {
      val article     = Article("1", "boom", PolyToMany.reference[Author](Set("test" -> "people")))
      val articleJson = implicitly[JsonApiFormat[Article]].write(article)

      implicitly[JsonApiFormat[Article]].read(articleJson, Set.empty) must be equalTo article
    }

    "properly read to-many loaded" in {
      val article =
        Article("1",
                "boom",
                PolyToMany.loaded[Author](
                  Seq(Coproduct[Author](Person("john", "doe")), Coproduct[Author](Company("evil", "business")))))
      val articleJson         = implicitly[JsonApiFormat[Article]].write(article)
      val articleIncludedJson = implicitly[JsonApiFormat[Article]].included(article)

      implicitly[JsonApiFormat[Article]].read(articleJson, articleIncludedJson, Set("authors"), "") must be equalTo article
    }

    "handle null/non existing/empty relationships" in {
      val emptyRelationships =
        """
          |{
          |  "id": "test",
          |  "type": "articles",
          |  "attributes": {
          |    "title": "boom"
          |  },
          |  "relationships": {
          |
          |  }
          |}
        """.stripMargin.parseJson

      val emptyId =
        """
          |{
          |  "id": "test",
          |  "type": "articles",
          |  "attributes": {
          |    "title": "boom"
          |  },
          |  "relationships": {
          |    "authors": {
          |      "data": [{
          |        "id": "test",
          |        "type": "people"
          |      },{
          |        "id": "",
          |        "type": "people"
          |      }]
          |    }
          |  }
          |}
        """.stripMargin.parseJson

      val nullRelationships =
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

      val nonExistingRelationships =
        """
          |{
          |  "id": "test",
          |  "type": "articles",
          |  "attributes": {
          |    "title": "boom"
          |  }
          |}
        """.stripMargin.parseJson

      implicitly[JsonApiFormat[Article]].read(emptyRelationships, Set.empty) must be equalTo Article(
        "test",
        "boom",
        PolyToMany.reference)
      implicitly[JsonApiFormat[Article]].read(nullRelationships, Set.empty) must be equalTo Article(
        "test",
        "boom",
        PolyToMany.reference)
      implicitly[JsonApiFormat[Article]].read(nonExistingRelationships, Set.empty) must be equalTo Article(
        "test",
        "boom",
        PolyToMany.reference)

      implicitly[JsonApiFormat[Article]].read(emptyId, Set.empty) must throwA[DeserializationException](
        "illegal id 'empty string' found")
    }

    "fail with deserialization exception when one or more of the entities in relationship is of wrong type" in {
      val article =
        Article("1",
                "boom",
                PolyToMany.loaded[Author](
                  Seq(Coproduct[Author](Person("john", "doe")), Coproduct[Author](Company("evil", "business")))))
      val articleJson         = implicitly[JsonApiFormat[Article]].write(article)
      val articleIncludesJson = implicitly[JsonApiFormat[Article]].included(article)

      import _root_.spray.json.lenses.JsonLenses._

      val modifiedJson =
        articleJson.update('relationships / 'authors / 'data / * / 'type ! set[String]("wrong-type")).asJsObject
      val modifiedIncludes = articleIncludesJson.map(_.update('type ! set[String]("wrong-type"))).map(_.asJsObject)

      implicitly[JsonApiFormat[Article]].read(modifiedJson, modifiedIncludes) must throwA[DeserializationException](
        message = "relationship of type 'wrong-type' is not one of \\[people,companies\\]")
    }

    "throw exception when giving wrong type for reference ids" in {
      Article("1", "boom", PolyToMany.reference[Author](Set("test" -> "authors"))) must throwA[PolyWrongTypeException]
    }

    "all ids in data are references (not to be loaded)" >> {
      val article = Article("1", "boom", PolyToMany.reference[Author](Set("test" -> "people")))
      val json    = rawOne(article)
      val parsed  = readOne[Article](json, Set("authors"))

      parsed must be equalTo article
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
          |      "authors": {
          |        "data": [{
          |          "type": "people",
          |          "id": "john"
          |        }, {
          |          "type": "companies",
          |          "id": "evil"
          |        }],
          |        "links": {
          |          "related": "/articles/1/authors"
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
          |    "type": "people",
          |    "attributes": {
          |      "name": "doe"
          |    },
          |    "id": "john",
          |    "links": {
          |      "self": "/people/john"
          |    }
          |  }]
          |}
        """.stripMargin.parseJson.asJsObject
      readOne[Article](json, Set("authors")) must throwA[DeserializationException](
        "mixed reference and loaded types found")
    }
  }

  "write" should {
    "skip printing out data for JsonAbsent case of JsonOption[PolyToMany[X]]" in {
      @jsonApiResource final case class Test(id: String, opt: JsonOption[PolyToMany[Author]])

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

    "print out data as null for JsonNull case of JsonOption[PolyToMany[X]]" in {
      @jsonApiResource final case class Test(id: String, opt: JsonOption[PolyToMany[Author]])

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

    "correctly write sparse fieldsets (while supporting inclusion of the relationships even if it is not included in the sparse fieldset)" in {
      implicit val sparseFields: Map[String, List[String]] = Map("articles" -> List("title"))
      val article =
        Article("1",
                "boom",
                PolyToMany.loaded[Author](
                  Seq(Coproduct[Author](Person("john", "doe")), Coproduct[Author](Company("evil", "business")))))

      val rawJson =
        """
          |{
          |  "data": {
          |    "attributes": {
          |      "title": "boom"
          |    },
          |    "links": {
          |      "self":"/articles/1"
          |    },
          |    "id": "1",
          |    "type": "articles"
          |  },
          |  "included": [
          |    {
          |      "attributes": {
          |        "name": "doe"
          |      },
          |      "id": "john",
          |      "links": {
          |        "self": "/people/john"
          |      },
          |      "type": "people"
          |    },
          |    {
          |      "attributes": {
          |        "name": "business"
          |      },
          |      "id": "evil",
          |      "links": {
          |        "self": "/companies/evil"
          |      },
          |      "type": "companies"
          |    }
          |  ]
          |}
        """.stripMargin.parseJson.asJsObject

      rawOne[Article](article) must be equalTo rawJson
    }
  }

  "properly generate Includes type class for poly to many relationship" in {
    val includes = implicitly[Includes[Loop]]

    includes.includeAllowed("looped") must beTrue
    includes.includeAllowed("looped.foo.looped") must beTrue
    includes.includeAllowed("looped.looped.looped") must beTrue
    includes.includesAllowed("looped", "looped.foo.looped", "looped.looped.looped") must beTrue

    includes.includeAllowed("foo") must beFalse
    includes.includeAllowed("notlooped") must beFalse
    includes.includeAllowed("looped.brake.looped") must beFalse
    includes.includeAllowed("looped.foo.brake") must beFalse
    includes.includesAllowed("foo", "notlooped", "looped.brake.looped", "looped.foo.brake") must beFalse

    includes.includesAllowed("looped", "notlooped") must beFalse
  }

  "read and write JsonOption relationship" in {
    @jsonApiResource("test", "no-id") final case class Test(name: String, x: JsonOption[PolyToMany[Author]])

    val t      = Test("name", JsonAbsent)
    val json   = rawOne(t)
    val parsed = readOne[Test](json)
    parsed must be equalTo t

    val t2      = Test("name", JsonNull)
    val json2   = rawOne(t2)
    val parsed2 = readOne[Test](json2)
    parsed2 must be equalTo (t2)

    val t3      = Test("name", JsonSome(PolyToMany.reference[Author](Set(("test", "people")))))
    val json3   = rawOne(t3)
    val parsed3 = readOne[Test](json3)
    parsed3 must be equalTo t3

    val authors = Seq(Coproduct[Author](Person("1", "1")), Coproduct[Author](Person("2", "2")))
    val t4      = Test("name", JsonSome(PolyToMany.loaded(authors)))
    val json4   = rawOne(t4)
    val parsed4 = readOne[Test](json4, Set("x"))
    parsed4 must be equalTo t4
  }
}
