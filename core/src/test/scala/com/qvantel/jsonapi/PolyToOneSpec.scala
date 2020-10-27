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

final class PolyToOneSpec extends Specification {
  sequential

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

  @jsonApiResource final case class Article(id: String, title: String, author: PolyToOne[Author])

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

  @jsonApiResource final case class Loop(id: String, looped: PolyToOne[Looped])
  @jsonApiResource final case class Maybe(id: String, maybe: Option[PolyToOne[Author]])
  @jsonApiResource final case class TestThing(id: String, foo: Option[PolyToOne[Looped]])

  "get" should {
    "return Some for Loaded" in {
      val person = Person("id", "name")
      val author = Coproduct[Author](person)
      PolyToOne.loaded[Author, Person](person).get must beSome(author)
    }

    "return None for Reference" in {
      PolyToOne.reference[Author, Person]("id").get must beNone
    }
  }

  "readers" should {
    "properly read in simple poly to-one reference" in {
      val article     = Article("1", "boom", PolyToOne.reference[Author, Person]("john"))
      val articleJson = implicitly[JsonApiFormat[Article]].write(article)

      implicitly[JsonApiFormat[Article]].read(articleJson, Set.empty) must be equalTo article
    }

    "properly read in simple poly to-one loaded" in {
      val article = Article("1", "boom", PolyToOne.loaded[Author, Person](Person("test-id", "mario")))
      val json    = rawOne[Article](article)

      readOne[Article](json, Set("author")) must be equalTo article
    }

    "properly read in looped poly to-one reference" in {
      val loop     = Loop("1", PolyToOne.reference[Looped, Person]("john"))
      val loopJson = implicitly[JsonApiFormat[Loop]].write(loop)

      implicitly[JsonApiFormat[Loop]].read(loopJson, Set.empty) must be equalTo loop
    }

    "properly read in looped poly to-one loaded" in {
      val loop =
        Loop("1", PolyToOne.loaded[Looped, Loop](Loop("2", PolyToOne.loaded[Looped, Person](Person("john", "doe")))))
      val json = rawOne[Loop](loop)

      readOne[Loop](json, Set("looped.looped")) must be equalTo loop
    }

    "properly read in optional poly to one reference" in {
      val maybe     = Maybe("1", None)
      val maybeJson = implicitly[JsonApiFormat[Maybe]].write(maybe)

      implicitly[JsonApiFormat[Maybe]].read(maybeJson, Set.empty) must be equalTo maybe
    }

    "properly read in optional poly to one reference" in {
      val maybe     = Maybe("1", Some(PolyToOne.loaded[Author, Person](Person("test-id", "mario"))))
      val maybeJson = rawOne[Maybe](maybe)

      readOne[Maybe](maybeJson, Set("maybe")) must be equalTo maybe
    }

    "handle null/non existing/empty relationships" in {
      val emptyRelationships =
        """
          |{
          |  "id": "test",
          |  "type": "maybes",
          |  "relationships": {
          |
          |  }
          |}
        """.stripMargin.parseJson

      val emptyIdRelationship =
        """
          |{
          |  "id": "test",
          |  "type": "maybes",
          |  "relationships": {
          |    "maybe": {
          |      "data": {
          |        "id": "",
          |        "type": "people"
          |      }
          |    }
          |  }
          |}
        """.stripMargin.parseJson

      val nullRelationships =
        """
          |{
          |  "id": "test",
          |  "type": "maybes",
          |  "relationships": null
          |}
        """.stripMargin.parseJson

      val nonExistingRelationships =
        """
          |{
          |  "id": "test",
          |  "type": "maybes"
          |}
        """.stripMargin.parseJson

      implicitly[JsonApiFormat[Maybe]].read(emptyRelationships, Set.empty) must be equalTo Maybe("test", None)
      implicitly[JsonApiFormat[Maybe]].read(nullRelationships, Set.empty) must be equalTo Maybe("test", None)
      implicitly[JsonApiFormat[Maybe]].read(nonExistingRelationships, Set.empty) must be equalTo Maybe("test", None)

      implicitly[JsonApiFormat[Maybe]].read(emptyIdRelationship, Set.empty) must throwA[DeserializationException](
        "illegal id 'empty string' found")
    }

    "fail with deserialization exception when the entity in the relationship is of wrong type" in {
      val article             = Article("1", "boom", PolyToOne.loaded[Author, Person](Person("test-id", "mario")))
      val articleJson         = implicitly[JsonApiFormat[Article]].write(article)
      val articleIncludesJson = implicitly[JsonApiFormat[Article]].included(article)

      import _root_.spray.json.lenses.JsonLenses._

      val modifiedArticleJson =
        articleJson.update("relationships" / "author" / "data" / "type" ! set[String]("wrong-type")).asJsObject
      val modifiedArticleIncludes =
        articleIncludesJson.map(_.update(Symbol("type") ! set[String]("wrong-type"))).map(_.asJsObject)

      implicitly[JsonApiFormat[Article]]
        .read(modifiedArticleJson, modifiedArticleIncludes) must throwA[DeserializationException](
        message = "relationship of type 'wrong-type' is not part of coproduct 'PolyToOneSpec.this.Author'")

      val maybe             = Maybe("1", Some(PolyToOne.loaded[Author, Person](Person("test-id", "mario"))))
      val maybeJson         = implicitly[JsonApiFormat[Maybe]].write(maybe)
      val maybeIncludedJson = implicitly[JsonApiFormat[Maybe]].included(maybe)

      val modifiedMaybeJson =
        maybeJson.update("relationships" / "maybe" / "data" / "type" ! set[String]("wrong-type")).asJsObject
      val modifiedMaybeIncludes =
        maybeIncludedJson.map(_.update(Symbol("type") ! set[String]("wrong-type"))).map(_.asJsObject)

      implicitly[JsonApiFormat[Maybe]]
        .read(modifiedMaybeJson, modifiedMaybeIncludes) must throwA[DeserializationException](
        message = "relationship of type 'wrong-type' is not part of coproduct 'PolyToOneSpec.this.Author'")
    }
  }

  "write" >> {
    "print out data as null for None case of Option[PolyToOne[X]]" >> {
      val t = Maybe("id", None)

      val rawJson =
        """
          |{
          |  "data": {
          |    "relationships": {
          |      "maybe": {
          |        "data": null,
          |        "links": {
          |          "related": "/maybes/id/maybe"
          |        }
          |      }
          |    },
          |    "links": {
          |      "self": "/maybes/id"
          |    },
          |    "id": "id",
          |    "type": "maybes"
          |  }
          |}
        """.stripMargin.parseJson.asJsObject

      rawOne(t) must be equalTo rawJson
    }

    "correctly write sparse fieldsets (while supporting inclusion of the relationship even if it is not included in the sparse fieldset)" >> {
      implicit val sparseFields: Map[String, List[String]] = Map("articles" -> List("title"))
      val article                                          = Article("1", "boom", PolyToOne.loaded[Author, Person](Person("test-id", "mario")))

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
          |        "name": "mario"
          |      },
          |      "id": "test-id",
          |      "links": {
          |        "self": "/people/test-id"
          |      },
          |      "type": "people"
          |    }
          |  ]
          |}
        """.stripMargin.parseJson.asJsObject

      rawOne[Article](article) must be equalTo rawJson
    }
  }

  "properly generate Includes type class for poly to one relationship" in {
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
}
