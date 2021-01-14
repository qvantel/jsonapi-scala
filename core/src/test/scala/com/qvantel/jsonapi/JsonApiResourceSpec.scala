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

import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import _root_.spray.json.DefaultJsonProtocol._
import _root_.spray.json._
import io.lemonlabs.uri.typesafe.dsl.stringToUri

import com.qvantel.jsonapi.Helpers._

final class JsonApiResourceSpec extends Specification with ScalaCheck {
  implicit val apiRoot: com.qvantel.jsonapi.ApiRoot = ApiRoot(None)

  @jsonApiResource final case class Simple(id: String, n: Int)
  @jsonApiResource final case class Relations(id: String,
                                              n: Int,
                                              a: ToOne[Simple],
                                              b: ToMany[Simple],
                                              c: Option[ToOne[Simple]])

  @jsonApiResource final case class Bottom(id: String, a: ToOne[Top])
  @jsonApiResource final case class Top(id: String, a: ToOne[Bottom])

  trait Bogus

  @jsonApiResource final case class HasCompanion(id: String, n: Int)
  object HasCompanion

  @jsonApiResource final case class HasCompanionWithTrait(id: String, n: Int)
  object HasCompanionWithTrait extends Bogus

  "jsonApiResource annotation" should {

    "work with a class that already has a companion object" in {
      implicitly[Identifiable[HasCompanion]].identify(HasCompanion("a", 1)) must be equalTo "a"
    }

    "work with a class that already has a companion object that extends some trait" in {
      implicitly[Identifiable[HasCompanionWithTrait]].identify(HasCompanionWithTrait("a", 1)) must be equalTo "a"
    }

    "name the resource type according to JSON API spec" in {
      implicitly[ResourceType[HasCompanion]].resourceType must be equalTo "has-companions"
    }

    "fill in Identifiable in a case class" in {
      implicitly[Identifiable[Simple]].identify(Simple("a", 1)) must be equalTo "a"
    }

    "fill in ResourceType in a case class" in {
      implicitly[ResourceType[Simple]].resourceType must be equalTo "simples"
    }

    "fill in PathTo in a case class" in {
      implicitly[PathToId[Simple]].self("1") must be equalTo "/simples/1"
    }

    "fill in Includes in a simple case class" in {
      val includes = implicitly[Includes[Simple]]

      prop { part: String =>
        includes.includeAllowed(part) must beFalse
      }.setGen(genInclude)
    }

    "fill in JsonApiFormat in a case class" in {
      implicitly[JsonApiFormat[Simple]].write(Simple("1", 2)) must be equalTo JsObject(
        "type"       -> "simples".toJson,
        "id"         -> "1".toJson,
        "attributes" -> JsObject("n" -> 2.toJson),
        "links"      -> JsObject("self" -> "/simples/1".toJson))
    }

    "create a proper JsonApiFormat for a composite entity" in {
      val obj = Relations("1", 2, ToOne.reference("a"), ToMany.loaded(Seq(Simple("2", 3))), None)
      implicitly[JsonApiFormat[Relations]].write(obj) must be equalTo JsObject(
        "type"       -> "relations".toJson,
        "id"         -> "1".toJson,
        "attributes" -> JsObject("n" -> 2.toJson),
        "relationships" -> JsObject(
          "a" -> JsObject("data" -> JsObject("type" -> "simples".toJson, "id" -> "a".toJson),
                          "links" -> JsObject("related" -> "/relations/1/a".toJson)),
          "b" -> JsObject("data" -> JsArray(JsObject("type" -> "simples".toJson, "id" -> "2".toJson)),
                          "links" -> JsObject("related" -> "/relations/1/b".toJson)),
          "c" -> JsObject("data" -> JsNull, "links" -> JsObject("related" -> "/relations/1/c".toJson))
        ),
        "links" -> JsObject("self" -> "/relations/1".toJson)
      )
    }

    "create a proper JsonApiFormat for a composite entity with sparse fields passed to the JsonApiWriter" in {
      val obj = Relations("1", 2, ToOne.reference("a"), ToMany.loaded(Seq(Simple("2", 3))), None)
      implicitly[JsonApiFormat[Relations]].write(obj, Map("relations" -> List("n", "b"))) must be equalTo JsObject(
        "type"       -> "relations".toJson,
        "id"         -> "1".toJson,
        "attributes" -> JsObject("n" -> 2.toJson),
        "relationships" -> JsObject(
          "b" -> JsObject("data" -> JsArray(JsObject("type" -> "simples".toJson, "id" -> "2".toJson)),
                          "links" -> JsObject("related" -> "/relations/1/b".toJson))
        ),
        "links" -> JsObject("self" -> "/relations/1".toJson)
      )
    }

    "create a proper JsonApiFormat for a composite entity that has no ID and sparse fields passed to the JsonApiWriter" in {
      @jsonApiResource("normal", "no-id") final case class RelationsNoID(n: Int,
                                                                         a: ToOne[Simple],
                                                                         b: ToMany[Simple],
                                                                         c: Option[ToOne[Simple]])

      val obj = RelationsNoID(2, ToOne.reference("a"), ToMany.loaded(Seq(Simple("2", 3))), None)
      implicitly[JsonApiFormat[RelationsNoID]]
        .write(obj, Map("relations-no-ids" -> List("n", "b"))) must be equalTo JsObject(
        "type"       -> "relations-no-ids".toJson,
        "attributes" -> JsObject("n" -> 2.toJson),
        "relationships" -> JsObject(
          "b" -> JsObject("data" -> JsArray(JsObject("type" -> "simples".toJson, "id" -> "2".toJson)))
        )
      )
    }

    "create a proper JsonApiReader for a simple case class" in {
      val s     = Simple("1", 1)
      val sJson = implicitly[JsonApiFormat[Simple]].write(s)
      s must be equalTo implicitly[JsonApiFormat[Simple]].read(sJson, Set.empty[JsObject])
    }

    "create a proper JsonApiReader for simple relationships" in {
      val obj  = Relations("1", 2, ToOne.reference("a"), ToMany.loaded(Seq(Simple("2", 3))), None)
      val json = rawOne(obj)

      obj must be equalTo readOne[Relations](json, Set("a", "b", "c"))
    }

    "fail with deserialization exception when type is wrong in json" in {
      val json =
        """
          |{
          |  "id": "test",
          |  "type": "derp"
          |}
        """.stripMargin.parseJson

      implicitly[JsonApiFormat[Relations]].read(json, Set.empty) must throwA[DeserializationException](
        message = "wrong type 'relations' expected but got 'derp'")
    }

    "fail with deserialization exception when id is missing and it is required" in {
      val json =
        """
          |{
          |  "type": "relations"
          |}
        """.stripMargin.parseJson

      implicitly[JsonApiFormat[Relations]].read(json, Set.empty) must throwA[DeserializationException](
        message = "expected field 'id' in json")
    }

    "fail with deserialization exception when attributes is missing and it is required" in {
      val json =
        """
          |{
          |  "id": "test",
          |  "type": "relations"
          |}
        """.stripMargin.parseJson

      implicitly[JsonApiFormat[Relations]].read(json, Set.empty) must throwA[DeserializationException](
        message = "expected field 'n' in attributes json")
    }

    "fail with deserialization exception when relationships is missing and it is required" in {
      val json =
        """
          |{
          |  "id": "test",
          |  "type": "relations",
          |  "attributes": {
          |    "n": 1
          |  }
          |}
        """.stripMargin.parseJson

      implicitly[JsonApiFormat[Relations]].read(json, Set.empty) must throwA[DeserializationException](
        message = "'relationships' object missing in json")
    }

    "optional attributes" in {
      @jsonApiResource final case class Test(id: String, maybe: Option[Int])

      "read attribute as None when attribute does not exist" in {
        val json =
          """
            |{
            |  "id": "test",
            |  "type": "tests",
            |  "attributes": {
            |  }
            |}
          """.stripMargin.parseJson

        implicitly[JsonApiFormat[Test]].read(json, Set.empty) must be equalTo Test("test", None)
      }

      "read attribute as None when attribute is null" in {
        val json =
          """
            |{
            |  "id": "test",
            |  "type": "tests",
            |  "attributes": {
            |    "maybe": null
            |  }
            |}
          """.stripMargin.parseJson

        implicitly[JsonApiFormat[Test]].read(json, Set.empty) must be equalTo Test("test", None)
      }

      "read attribute as None when it is the only attribute and attributes struct is missing" in {
        val json =
          """
            |{
            |  "id": "test",
            |  "type": "tests"
            |}
          """.stripMargin.parseJson

        implicitly[JsonApiFormat[Test]].read(json, Set.empty) must be equalTo Test("test", None)
      }
    }

    "allow relationships object to be null or non existing if all relationships are optional/to many" in {
      @jsonApiResource final case class Test(id: String, one: Option[ToOne[Simple]], many: ToMany[Simple])

      val emptyJson =
        """
          |{
          |  "id": "test",
          |  "type": "tests",
          |  "relationships": {
          |
          |  }
          |}
        """.stripMargin.parseJson

      val nullJson =
        """
          |{
          |  "id": "test",
          |  "type": "tests",
          |  "relationships": {
          |
          |  }
          |}
        """.stripMargin.parseJson

      val nonExistingJson =
        """
          |{
          |  "id": "test",
          |  "type": "tests",
          |  "relationships": {
          |
          |  }
          |}
        """.stripMargin.parseJson

      implicitly[JsonApiFormat[Test]].read(emptyJson, Set.empty) must be equalTo Test("test",
                                                                                      None,
                                                                                      ToMany.reference[Simple])
      implicitly[JsonApiFormat[Test]].read(nullJson, Set.empty) must be equalTo Test("test",
                                                                                     None,
                                                                                     ToMany.reference[Simple])
      implicitly[JsonApiFormat[Test]].read(nonExistingJson, Set.empty) must be equalTo Test("test",
                                                                                            None,
                                                                                            ToMany.reference[Simple])
    }

    "allow annotation params to change the type" in {
      @jsonApiResource("we-changed-this") final case class ChangeType(id: String)
      implicitly[ResourceType[ChangeType]].resourceType must be equalTo "we-changed-this"
      val ct = ChangeType("id")

      val json = """
                   |{
                   |  "type": "we-changed-this",
                   |  "id": "id",
                   |  "links": {
                   |    "self": "/we-changed-this/id"
                   |  }
                   |}
       """.stripMargin.parseJson

      implicitly[JsonApiFormat[ChangeType]].write(ct) must be equalTo json
    }

    "allow entity without id if given correct param" in {
      @jsonApiResource("normal", "no-id") final case class ChangeType(name: String)
      implicitly[ResourceType[ChangeType]].resourceType must be equalTo "change-types"

      val ct = ChangeType("name")

      val json =
        """
          |{
          |  "type": "change-types",
          |  "attributes": {
          |    "name": "name"
          |  }
          |}
        """.stripMargin.parseJson

      implicitly[JsonApiFormat[ChangeType]].write(ct) must be equalTo json
    }

    "allow to change both type and have no id with params" in {
      @jsonApiResource("we-changed-this", "no-id") final case class ChangeType(name: String)
      implicitly[ResourceType[ChangeType]].resourceType must be equalTo "we-changed-this"

      val ct = ChangeType("name")

      val json =
        """
          |{
          |  "type": "we-changed-this",
          |  "attributes": {
          |    "name": "name"
          |  }
          |}
        """.stripMargin.parseJson

      implicitly[JsonApiFormat[ChangeType]].write(ct) must be equalTo json
    }

    "cut loops out of structure" in {
      @jsonApiResource final case class Root(id: String, loop: ToMany[Loop])
      @jsonApiResource final case class Loop(id: String, looped: ToOne[Foo])
      @jsonApiResource final case class Foo(id: String, loop: ToOne[Loop])

      val foo = Foo("foo", ToOne.reference("1"))

      val l1 = Loop("1", ToOne.loaded(foo))
      val l2 = Loop("2", ToOne.loaded(foo))
      val l3 = Loop("3", ToOne.loaded(foo))

      val root = Root("root", ToMany.loaded(Seq(l1, l2, l3)))

      val json = rawOne[Root](root)

      val parsed = readOne[Root](json)
      ok
    }

    "handle iterable attributes" in {
      @jsonApiResource final case class Test(id: String,
                                             list: List[String],
                                             seq: Seq[String],
                                             set: Set[String],
                                             iterable: Iterable[String],
                                             map: Map[String, String])

      readOne[Test]("""
                      |{
                      |  "data": {
                      |    "type": "tests",
                      |    "attributes": {
                      |
                      |    },
                      |    "id": "1"
                      |  }
                      |}
        """.stripMargin.parseJson.asJsObject) must be equalTo Test("1",
                                                                   List.empty,
                                                                   Seq.empty,
                                                                   Set.empty,
                                                                   Iterable.empty,
                                                                   Map.empty)

      readOne[Test]("""
                      |{
                      |  "data": {
                      |    "type": "tests",
                      |    "attributes": {
                      |      "list": ["test"],
                      |      "seq": ["test"],
                      |      "set": ["test"],
                      |      "iterable": ["test"],
                      |      "map": { "test": "test" }
                      |    },
                      |    "id": "1"
                      |  }
                      |}
        """.stripMargin.parseJson.asJsObject) must be equalTo Test("1",
                                                                   List("test"),
                                                                   Seq("test"),
                                                                   Set("test"),
                                                                   Iterable("test"),
                                                                   Map("test" -> "test"))

    }
  }

  "'/' in id should be escaped correctly in the urls" >> {
    @jsonApiResource final case class Test(id: String, many: Option[ToOne[Test]], one: ToMany[Test])

    val t = Test("foo/bar", None, ToMany.reference)

    val json = rawOne[Test](t)

    import _root_.spray.json.lenses.JsonLenses._
    import _root_.spray.json.DefaultJsonProtocol._

    json.extract[String]("data" / "links" / "self") must be equalTo "/tests/foo%2Fbar"
    json.extract[String]("data" / "relationships" / "one" / "links" / "related") must be equalTo "/tests/foo%2Fbar/one"
    json.extract[String]("data" / "relationships" / "many" / "links" / "related") must be equalTo "/tests/foo%2Fbar/many"
  }
}
