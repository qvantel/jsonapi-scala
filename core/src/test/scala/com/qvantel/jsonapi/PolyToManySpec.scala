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
import _root_.spray.http.Uri.Path
import _root_.spray.json.DefaultJsonProtocol._
import _root_.spray.json._

import com.qvantel.jsonapi.PolyToMany.PolyWrongTypeException

final class PolyToManySpec extends Specification {
  implicit val apiRoot = ApiRoot(None)

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
      PolyToMany.reference[Author](Map("1" -> rt, "2" -> rt)).get must be empty
    }
  }

  "renderRelation" should {
    "render a to-many reference relation" in {
      val article = Article("1", "boom", PolyToMany.reference[Author])
      val expected = JsObject(
        "links" -> JsObject("self" -> (Path("/articles/1") / "relationships" / "authors").toJson,
                            "related" -> (Path("/articles/1") / "authors").toJson),
        "data" -> JsArray.empty
      )

      PolyToMany.renderRelation(article, "authors", article.authors) should be equalTo expected

      val loop = Loop("2", PolyToMany.reference[Looped])
      val expected2 = JsObject("links" -> JsObject("self" -> (Path("/loops/2") / "relationships" / "looped").toJson,
                                                   "related" -> (Path("/loops/2") / "looped").toJson),
                               "data" -> JsArray.empty)

      PolyToMany.renderRelation(loop, "looped", loop.looped) should be equalTo expected2
    }

    "render a to-many loaded relation" in {
      val article =
        Article("1",
                "boom",
                PolyToMany.loaded[Author](
                  Seq(Coproduct[Author](Person("john", "doe")), Coproduct[Author](Company("evil", "business")))))
      val expected = JsObject(
        "links" -> JsObject("self" -> (Path("/articles/1") / "relationships" / "authors").toJson,
                            "related" -> (Path("/articles/1") / "authors").toJson),
        "data" -> JsArray(JsObject("type" -> "people".toJson, "id" -> "john".toJson),
                          JsObject("type" -> "companies".toJson, "id" -> "evil".toJson))
      )

      PolyToMany.renderRelation(article, "authors", article.authors) should be equalTo expected

      val loop =
        Loop("root",
             PolyToMany.loaded[Looped](
               Seq(Coproduct[Looped](Person("john", "doe")), Coproduct[Looped](Loop("test", PolyToMany.reference)))))
      val expected2 = JsObject(
        "links" -> JsObject("self" -> (Path("/loops/root") / "relationships" / "looped").toJson,
                            "related" -> (Path("/loops/root") / "looped").toJson),
        "data" -> JsArray(JsObject("type" -> "people".toJson, "id" -> "john".toJson),
                          JsObject("type" -> "loops".toJson, "id" -> "test".toJson))
      )

      PolyToMany.renderRelation(loop, "looped", loop.looped) should be equalTo expected2
    }
  }

  "read" should {
    "properly read to-many reference" in {
      val article     = Article("1", "boom", PolyToMany.reference[Author])
      val articleJson = implicitly[JsonApiFormat[Article]].write(article)

      implicitly[JsonApiFormat[Article]].read(articleJson, Set.empty) must be equalTo article
    }

    "properly read to-many reference ids" in {
      val article     = Article("1", "boom", PolyToMany.reference[Author](Map("test" -> "people")))
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
        message = "is of type 'wrong-type' which is not part of the coproduct 'PolyToManySpec.this.Author'")
    }

    "throw exception when giving wrong type for reference ids" in {
      Article("1", "boom", PolyToMany.reference[Author](Map("test" -> "authors"))) must throwA[PolyWrongTypeException]
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
}
