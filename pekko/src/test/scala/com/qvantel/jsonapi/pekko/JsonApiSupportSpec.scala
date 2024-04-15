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
package com.qvantel.jsonapi.pekko

import _root_.org.apache.pekko.http.scaladsl.marshalling.ToEntityMarshaller
import _root_.org.apache.pekko.http.scaladsl.model._
import _root_.org.apache.pekko.http.scaladsl.server.Directive._
import _root_.org.apache.pekko.http.scaladsl.server.Directives._
import _root_.org.apache.pekko.http.scaladsl.server._
import _root_.org.apache.pekko.http.scaladsl.testkit.Specs2RouteTest
import _root_.spray.json.DefaultJsonProtocol._
import _root_.spray.json._
import com.qvantel.jsonapi._
import com.qvantel.jsonapi.model.{ErrorObject, TopLevel}
import com.qvantel.jsonapi.pekko.JsonApiSupport._
import io.lemonlabs.uri.Url
import io.lemonlabs.uri.typesafe.dsl._
import org.specs2.mutable.Specification
import shapeless._

import scala.concurrent.Future

final class JsonApiSupportSpec extends Specification with Specs2RouteTest {
  val ct = ContentType(MediaTypes.`application/vnd.api+json`)

  implicit val apiRoot: com.qvantel.jsonapi.ApiRoot = ApiRoot(None)

  final case class Root(id: String,
                        nameMangling: String,
                        loaded: ToOne[Child],
                        referenced: ToOne[Child],
                        many: ToMany[Child],
                        manyReferenced: ToMany[Child],
                        article: ToOne[Article])

  object Root {
    implicit val resourceType: ResourceType[Root] = ResourceType[Root]("root")
    implicit val identifiable: Identifiable[Root] = Identifiable.by(_.id)
    implicit val pathTo: PathTo[Root] = new PathToId[Root] {
      override def root: Url = "/roots"
    }
    implicit val format: JsonApiFormat[Root] = jsonApiFormat[Root]
  }

  final case class Child(id: String, name: String)

  object Child {
    implicit val resourceType: ResourceType[Child] = ResourceType[Child]("child")
    implicit val identifiable: Identifiable[Child] = Identifiable.by(_.id)
    implicit val pathTo: PathTo[Child] = new PathToId[Child] {
      override def root: Url = "/children"
    }
    implicit val format: JsonApiFormat[Child] = jsonApiFormat[Child]
  }

  final case class Article(id: String, name: String)

  object Article {
    implicit val resourceType: ResourceType[Article] = ResourceType[Article]("article")
    implicit val identifiable: Identifiable[Article] = Identifiable.by(_.id)
    implicit val pathTo: PathTo[Article] = new PathToId[Article] {
      override def root: Url = "/articles"
    }
    implicit val format: JsonApiFormat[Article] = jsonApiFormat[Article]
  }

  def actorRefFactory = system

  val child = Child("1", "test")
  val thang = Thang("idThang", "nameThang", 20)
  val many  = Seq(Child("3", "test 3"), Child("4", "test 4"), Child("5", "test 5"))
  val data = Root("1",
                  "test data",
                  ToOne.loaded(child),
                  ToOne.reference("2"),
                  ToMany.loaded(many),
                  ToMany.reference,
                  ToOne.loaded(Article("55", "test")))
  val data2 = Root("2",
                   "test data",
                   ToOne.loaded(child),
                   ToOne.reference("2"),
                   ToMany.loaded(many),
                   ToMany.reference,
                   ToOne.loaded(Article("55", "test")))

  @jsonApiResource final case class Thang(id: String, name: String, age: Int)
  @jsonApiResource("name-changed", "no-id") final case class Thing(name: String, thang: ToOne[Thang])

  val route: Route =
    get {
      complete {
        data
      }
    } ~
      (path("single") & post & entity(as[Root])) { obj =>
        complete {
          obj
        }
      } ~
      (path("collection") & post & entity(as[Iterable[Root]])) { obj =>
        complete {
          obj
        }
      } ~
      (path("thing") & post & entity(as[Thing])) { obj =>
        complete {
          obj
        }
      }

  def recursiveJsObjectComparsion(jsObject1: JsObject, jsObject2: JsObject): Boolean = {
    def recursiveJsValueComparsion(jsValue1: JsValue, jsValue2: JsValue): Boolean =
      (jsValue1, jsValue2) match {
        case (fieldValue: JsObject, fieldValue2: JsObject) =>
          fieldValue.fields.forall {
            case (nestedFieldName, nestedFieldValue) if fieldValue2.fields.isDefinedAt(nestedFieldName) =>
              recursiveJsValueComparsion(nestedFieldValue, fieldValue2.fields(nestedFieldName))
            case _ => false
          }
        case (fieldValue: JsArray, fieldValue2: JsArray) =>
          fieldValue.elements.forall { nestedField =>
            fieldValue2.elements.exists(recursiveJsValueComparsion(_, nestedField))
          }
        case _ => jsValue1 == jsValue2
      }

    if (jsObject1.fields.forall {
          case (fieldName, fieldValue) =>
            recursiveJsValueComparsion(
              fieldValue,
              jsObject2.fields
                .getOrElse(fieldName, throw new Exception(jsObject1.prettyPrint + " != " + jsObject2.prettyPrint)))
        }) {
      true
    } else {
      throw new Exception(jsObject1.prettyPrint + " != " + jsObject2.prettyPrint)
    }
  }

  "JsonApiSupport" should {
    "rawOne correctly prints jsonapi json for one entity" in {
      val json =
        """
          |{
          |  "data": {
          |    "attributes": {
          |      "name-mangling": "test data"
          |    },
          |    "relationships": {
          |      "loaded": {
          |        "data": {
          |          "type": "child",
          |          "id": "1"
          |        },
          |        "links": {
          |          "related": "/roots/1/loaded"
          |        }
          |      },
          |      "article": {
          |        "data": {
          |          "type": "article",
          |          "id": "55"
          |        },
          |        "links": {
          |          "related": "/roots/1/article"
          |        }
          |      },
          |      "referenced": {
          |        "data": {
          |          "type": "child",
          |          "id": "2"
          |        },
          |        "links": {
          |          "related": "/roots/1/referenced"
          |        }
          |      },
          |      "many-referenced": {
          |        "links": {
          |          "related": "/roots/1/many-referenced"
          |        }
          |      },
          |      "many": {
          |        "data": [{
          |          "type": "child",
          |          "id": "3"
          |        }, {
          |          "type": "child",
          |          "id": "4"
          |        }, {
          |          "type": "child",
          |          "id": "5"
          |        }],
          |        "links": {
          |          "related": "/roots/1/many"
          |        }
          |      }
          |    },
          |    "links": {
          |      "self": "/roots/1"
          |    },
          |    "id": "1",
          |    "type": "root"
          |  },
          |  "included": [{
          |    "type": "article",
          |    "attributes": {
          |      "name": "test"
          |    },
          |    "id": "55",
          |    "links": {
          |      "self": "/articles/55"
          |    }
          |  }, {
          |    "type": "child",
          |    "attributes": {
          |      "name": "test 3"
          |    },
          |    "id": "3",
          |    "links": {
          |      "self": "/children/3"
          |    }
          |  }, {
          |    "type": "child",
          |    "attributes": {
          |      "name": "test"
          |    },
          |    "id": "1",
          |    "links": {
          |      "self": "/children/1"
          |    }
          |  }, {
          |    "type": "child",
          |    "attributes": {
          |      "name": "test 4"
          |    },
          |    "id": "4",
          |    "links": {
          |      "self": "/children/4"
          |    }
          |  }, {
          |    "type": "child",
          |    "attributes": {
          |      "name": "test 5"
          |    },
          |    "id": "5",
          |    "links": {
          |      "self": "/children/5"
          |    }
          |  }]
          |}
        """.stripMargin.parseJson.asJsObject
      recursiveJsObjectComparsion(rawOne(data), json)
    }

    "rawOne correctly prints jsonapi json for one entity with sparse fields defined" in {
      val json =
        """
          |{
          |  "data": {
          |    "attributes": {
          |      "name-mangling": "test data"
          |    },
          |    "relationships": {
          |      "article": {
          |        "data": {
          |          "type": "article",
          |          "id": "55"
          |        },
          |        "links": {
          |          "related": "/roots/1/article"
          |        }
          |      },
          |      "referenced": {
          |        "data": {
          |          "type": "child",
          |          "id": "2"
          |        },
          |        "links": {
          |          "related": "/roots/1/referenced"
          |        }
          |      }
          |    },
          |    "links": {
          |      "self": "/roots/1"
          |    },
          |    "id": "1",
          |    "type": "root"
          |  },
          |  "included": [{
          |    "type": "child",
          |    "id": "4",
          |    "links": {
          |      "self": "/children/4"
          |    }
          |  }, {
          |    "type": "child",
          |    "id": "3",
          |    "links": {
          |      "self": "/children/3"
          |    }
          |  }, {
          |    "type": "child",
          |    "id": "5",
          |    "links": {
          |      "self": "/children/5"
          |    }
          |  }, {
          |    "type": "article",
          |    "attributes": {
          |      "name": "test"
          |    },
          |    "id": "55",
          |    "links": {
          |      "self": "/articles/55"
          |    }
          |  }, {
          |    "type": "child",
          |    "id": "1",
          |    "links": {
          |      "self": "/children/1"
          |    }
          |  }]
          |}
        """.stripMargin.parseJson.asJsObject

      implicit val sparseFields: Map[String, List[String]] =
        Map("root"    -> List("name-mangling", "referenced", "article"),
            "child"   -> List("fieldThatDoesNotExist"),
            "article" -> List("name"))

      recursiveJsObjectComparsion(rawOne(data), json)
    }

    "rawCollection correctly prints jsonapi json for two entities" in {
      val json =
        """
          |{
          |  "data": [{
          |    "attributes": {
          |      "name-mangling": "test data"
          |    },
          |    "relationships": {
          |      "loaded": {
          |        "data": {
          |          "type": "child",
          |          "id": "1"
          |        },
          |        "links": {
          |          "related": "/roots/1/loaded"
          |        }
          |      },
          |      "article": {
          |        "data": {
          |          "type": "article",
          |          "id": "55"
          |        },
          |        "links": {
          |          "related": "/roots/1/article"
          |        }
          |      },
          |      "referenced": {
          |        "data": {
          |          "type": "child",
          |          "id": "2"
          |        },
          |        "links": {
          |          "related": "/roots/1/referenced"
          |        }
          |      },
          |      "many-referenced": {
          |        "links": {
          |          "related": "/roots/1/many-referenced"
          |        }
          |      },
          |      "many": {
          |        "data": [{
          |          "type": "child",
          |          "id": "3"
          |        }, {
          |          "type": "child",
          |          "id": "4"
          |        }, {
          |          "type": "child",
          |          "id": "5"
          |        }],
          |        "links": {
          |          "related": "/roots/1/many"
          |        }
          |      }
          |    },
          |    "links": {
          |      "self": "/roots/1"
          |    },
          |    "id": "1",
          |    "type": "root"
          |  }, {
          |    "attributes": {
          |      "name-mangling": "test data"
          |    },
          |    "relationships": {
          |      "loaded": {
          |        "data": {
          |          "type": "child",
          |          "id": "1"
          |        },
          |        "links": {
          |          "related": "/roots/2/loaded"
          |        }
          |      },
          |      "article": {
          |        "data": {
          |          "type": "article",
          |          "id": "55"
          |        },
          |        "links": {
          |          "related": "/roots/2/article"
          |        }
          |      },
          |      "referenced": {
          |        "data": {
          |          "type": "child",
          |          "id": "2"
          |        },
          |        "links": {
          |          "related": "/roots/2/referenced"
          |        }
          |      },
          |      "many-referenced": {
          |        "links": {
          |          "related": "/roots/2/many-referenced"
          |        }
          |      },
          |      "many": {
          |        "data": [{
          |          "type": "child",
          |          "id": "3"
          |        }, {
          |          "type": "child",
          |          "id": "4"
          |        }, {
          |          "type": "child",
          |          "id": "5"
          |        }],
          |        "links": {
          |          "related": "/roots/2/many"
          |        }
          |      }
          |    },
          |    "links": {
          |      "self": "/roots/2"
          |    },
          |    "id": "2",
          |    "type": "root"
          |  }],
          |  "included": [{
          |    "type": "article",
          |    "attributes": {
          |      "name": "test"
          |    },
          |    "id": "55",
          |    "links": {
          |      "self": "/articles/55"
          |    }
          |  }, {
          |    "type": "child",
          |    "attributes": {
          |      "name": "test 3"
          |    },
          |    "id": "3",
          |    "links": {
          |      "self": "/children/3"
          |    }
          |  }, {
          |    "type": "child",
          |    "attributes": {
          |      "name": "test"
          |    },
          |    "id": "1",
          |    "links": {
          |      "self": "/children/1"
          |    }
          |  }, {
          |    "type": "child",
          |    "attributes": {
          |      "name": "test 4"
          |    },
          |    "id": "4",
          |    "links": {
          |      "self": "/children/4"
          |    }
          |  }, {
          |    "type": "child",
          |    "attributes": {
          |      "name": "test 5"
          |    },
          |    "id": "5",
          |    "links": {
          |      "self": "/children/5"
          |    }
          |  }]
          |}
        """.stripMargin.parseJson.asJsObject
      recursiveJsObjectComparsion(rawCollection(Iterable(data, data2)), json)
    }

    "rawCollection correctly prints jsonapi json for two entities with sparse fields defined" in {
      val json =
        """
          |{
          |  "data": [{
          |    "attributes": {
          |      "name-mangling": "test data"
          |    },
          |    "relationships": {
          |      "article": {
          |        "data": {
          |          "type": "article",
          |          "id": "55"
          |        },
          |        "links": {
          |          "related": "/roots/1/article"
          |        }
          |      },
          |      "referenced": {
          |        "data": {
          |          "type": "child",
          |          "id": "2"
          |        },
          |        "links": {
          |          "related": "/roots/1/referenced"
          |        }
          |      }
          |    },
          |    "links": {
          |      "self": "/roots/1"
          |    },
          |    "id": "1",
          |    "type": "root"
          |  }, {
          |    "attributes": {
          |      "name-mangling": "test data"
          |    },
          |    "relationships": {
          |      "article": {
          |        "data": {
          |          "type": "article",
          |          "id": "55"
          |        },
          |        "links": {
          |          "related": "/roots/2/article"
          |        }
          |      },
          |      "referenced": {
          |        "data": {
          |          "type": "child",
          |          "id": "2"
          |        },
          |        "links": {
          |          "related": "/roots/2/referenced"
          |        }
          |      }
          |    },
          |    "links": {
          |      "self": "/roots/2"
          |    },
          |    "id": "2",
          |    "type": "root"
          |  }],
          |  "included": [{
          |    "type": "child",
          |    "id": "4",
          |    "links": {
          |      "self": "/children/4"
          |    }
          |  }, {
          |    "type": "child",
          |    "id": "3",
          |    "links": {
          |      "self": "/children/3"
          |    }
          |  }, {
          |    "type": "child",
          |    "id": "5",
          |    "links": {
          |      "self": "/children/5"
          |    }
          |  }, {
          |    "type": "article",
          |    "attributes": {
          |      "name": "test"
          |    },
          |    "id": "55",
          |    "links": {
          |      "self": "/articles/55"
          |    }
          |  }, {
          |    "type": "child",
          |    "id": "1",
          |    "links": {
          |      "self": "/children/1"
          |    }
          |  }]
          |}
        """.stripMargin.parseJson.asJsObject

      implicit val sparseFields: Map[String, List[String]] =
        Map("root"    -> List("name-mangling", "referenced", "article"),
            "child"   -> List("fieldThatDoesNotExist"),
            "article" -> List("name"))

      recursiveJsObjectComparsion(rawCollection(Iterable(data, data2)), json)
    }

    "return correct media type" in {
      Get() ~> route ~> check {
        contentType must_== ct
      }
    }

    "return relationships" in {
      Get() ~> route ~> check {
        import _root_.spray.json.DefaultJsonProtocol._
        import _root_.spray.json.lenses.JsonLenses._

        contentType must_== ct

        val json = JsonParser(responseAs[String])

        json.extract[String](Symbol("data") / "id") must_== data.id

        json.extract[String](Symbol("data") / "relationships" / "loaded" / "data" / "type") must_== Child.resourceType.resourceType
        json.extract[String](Symbol("data") / "relationships" / "loaded" / "data" / "id") must_== child.id
        json.extract[String](Symbol("data") / "relationships" / "loaded" / "links" / "related") must_== "/roots/1/loaded"

        json.extract[String](Symbol("data") / "relationships" / "referenced" / "data" / "type") must_== Child.resourceType.resourceType
        json.extract[String](Symbol("data") / "relationships" / "referenced" / "data" / "id") must_== "2"
        json.extract[String](Symbol("data") / "relationships" / "referenced" / "links" / "related") must_== "/roots/1/referenced"

        json.extract[String](Symbol("data") / "relationships" / "many" / "data" / element(0) / "type") must_== Child.resourceType.resourceType
        json.extract[String](Symbol("data") / "relationships" / "many" / "data" / element(0) / "id") must_== "3"
        json.extract[String](Symbol("data") / "relationships" / "many" / "data" / element(1) / "type") must_== Child.resourceType.resourceType
        json.extract[String](Symbol("data") / "relationships" / "many" / "data" / element(1) / "id") must_== "4"
        json.extract[String](Symbol("data") / "relationships" / "many" / "data" / element(2) / "type") must_== Child.resourceType.resourceType
        json.extract[String](Symbol("data") / "relationships" / "many" / "data" / element(2) / "id") must_== "5"
        json.extract[String](Symbol("data") / "relationships" / "many" / "links" / "related") must_== "/roots/1/many"

        val included: Seq[JsValue] = json.extract[JsArray]("included").elements

        def checkIdAndType(id: String, `type`: String): Boolean =
          included.exists { element =>
            val fields = element.asJsObject.fields.toSeq
            fields.contains("id" -> JsString(id)) && fields.contains("type" -> JsString(`type`))
          }

        checkIdAndType(child.id, Child.resourceType.resourceType) must_== true
        checkIdAndType("3", Child.resourceType.resourceType) must_== true
        checkIdAndType("4", Child.resourceType.resourceType) must_== true
        checkIdAndType("5", Child.resourceType.resourceType) must_== true
        checkIdAndType("55", Article.resourceType.resourceType) must_== true
      }
    }

    "handle UTF-8 correctly" in {
      val utf = "Ð²ÑÅÄÖåäöæøå"
      val root = Root(
        "foo",
        utf,
        ToOne.reference("foo"),
        ToOne.reference("foo"),
        ToMany.reference("/roots/foo/many"),
        ToMany.reference("/roots/foo/many-referenced"),
        ToOne.reference("foo")
      )
      Post("/single", root) ~> route ~> check {
        contentType must_== ct

        val parsed = readOne[Root](JsonParser(responseAs[String]).asJsObject)

        parsed must_== root
      }
    }

    "render poly lists as jsonapi" in {
      @jsonApiResource case class TestA(id: String)
      @jsonApiResource case class TestB(id: String)

      type AorB = TestA :+: TestB :+: CNil

      implicitly[JsonApiWriter[AorB]].write(Coproduct[AorB](TestA("1"))) must_== TestA("1").toJson
      implicitly[JsonApiWriter[AorB]].write(Coproduct[AorB](TestB("2"))) must_== TestB("2").toJson
    }

    "marshal a RelatedResponse[Thang] to JSON" in {
      val route = (path("notSparse") & get) {
        implicit val marshaller: ToEntityMarshaller[RelatedResponse[Thang]] = relatedResponseMarshaller[Thang]
        complete {
          Future.successful(Some(RelatedResponse.apply(thang)))
        }
      }

      Get("/notSparse") ~> route ~> check {
        val json = JsonParser(responseAs[String])

        json must be equalTo rawOne[Thang](thang)
      }
    }

    "marshal a RelatedResponse[Thang] to JSON with sparse fields" in {
      // The sparseFields implicit has to be before the marshaller as the value of it will be "hard-coded" inside of it and not reloaded for each "complete()".
      implicit val sparseFields: Map[String, List[String]]                = Map("thangs" -> List("age"))
      implicit val marshaller: ToEntityMarshaller[RelatedResponse[Thang]] = relatedResponseMarshaller[Thang]

      val route =
        (path("sparse") & get) {
          complete {
            Future.successful(Some(RelatedResponse.apply(thang)))
          }
        }

      Get("/sparse") ~> route ~> check {
        val json = JsonParser(responseAs[String])
        json must be equalTo rawOne[Thang](thang)
      }
    }

    "unmarshaller a single jsonapi object" in {
      Post("/single?include=loaded,article,referenced,many-referenced,many", data) ~> route ~> check {
        contentType must_== ct

        val json    = JsonParser(responseAs[String])
        val printed = rawOne[Root](data)

        json must be equalTo printed
      }
    }

    "unmarshaller a single jsonapi object that does not have an id" in {
      val thingData = Thing("test", ToOne.loaded(thang))

      Post("/thing?include=thang", thingData) ~> route ~> check {
        contentType must_== ct

        val json = JsonParser(responseAs[String])

        val writtenJson = rawOne[Thing](thingData)

        json must be equalTo writtenJson
      }
    }

    "unmarshaller collection jsonapi object" in {
      Post("/collection?include=loaded,article,referenced,many-referenced,many", Iterable(data, data2)) ~> route ~> check {
        contentType must_== ct

        val json = JsonParser(responseAs[String])

        val marshalledJson = rawCollection[Root](List(data, data2))

        json must be equalTo marshalledJson
      }
    }

    "readOne correctly handles relationships looping back to main entity" in {
      @jsonApiResource case class A(id: String, a: ToOne[A])

      val a = A("1", ToOne.Loaded(A("2", ToOne.reference("1"))))

      val json = rawOne[A](a)

      val parsed = readOne[A](json, Set("a.a"))

      parsed.id must be equalTo "1"
      parsed.a.id must be equalTo "2"

      val rel = parsed.a.asInstanceOf[ToOne.Loaded[A]].entity.a

      rel must beAnInstanceOf[ToOne.Loaded[A]]

      val loaded = rel.asInstanceOf[ToOne.Loaded[A]].entity

      loaded must be equalTo A("1", ToOne.reference("2"))
    }

    "readCollection correctly handles relationships looping back main entity" in {
      @jsonApiResource case class A(id: String, a: ToOne[A])

      val a = A("a1", ToOne.Loaded(A("a2", ToOne.reference("a1"))))
      val b = A("b1", ToOne.Loaded(A("b2", ToOne.reference("b1"))))

      val json = rawCollection[A](List(a, b))

      val data = readCollection[A](json, Set("a.a"))

      data must contain { (parsed: A) =>
        parsed.id must be equalTo "a1"
        parsed.a.id must be equalTo "a2"

        val rel = parsed.a.asInstanceOf[ToOne.Loaded[A]].entity.a

        rel must beAnInstanceOf[ToOne.Loaded[A]]

        val loaded = rel.asInstanceOf[ToOne.Loaded[A]].entity

        loaded must be equalTo A("a1", ToOne.reference("a2"))
      }.exactly(1.times)

      data must contain { (parsed: A) =>
        parsed.id must be equalTo "b1"
        parsed.a.id must be equalTo "b2"

        val rel = parsed.a.asInstanceOf[ToOne.Loaded[A]].entity.a

        rel must beAnInstanceOf[ToOne.Loaded[A]]

        val loaded = rel.asInstanceOf[ToOne.Loaded[A]].entity

        loaded must be equalTo A("b1", ToOne.reference("b2"))
      }.exactly(1.times)
    }
    "unmarshal TopLevel.Single" in {

      val route = get {
        complete(
          HttpResponse(
            status = StatusCodes.BadRequest,
            entity = HttpEntity(
              MediaTypes.`application/vnd.api+json`,
              TopLevel
                .Single(
                  data = None,
                  links = Map.empty,
                  meta = Map.empty,
                  jsonapi = None,
                  included = Map.empty
                )
                .toJson
                .prettyPrint
            )
          ))
      }
      Get("/") ~> route ~> check {
        val single = responseAs[TopLevel.Single]
        single.data must beNone
      }
    }
    "unmarshal response to TopLevel.Collection" in {
      val route = get {
        complete(
          HttpResponse(
            status = StatusCodes.BadRequest,
            entity = HttpEntity(
              MediaTypes.`application/vnd.api+json`,
              TopLevel
                .Collection(
                  data = Map.empty,
                  links = Map.empty,
                  meta = Map.empty,
                  jsonapi = None,
                  included = Map.empty
                )
                .toJson
                .prettyPrint
            )
          ))
      }
      Get("/") ~> route ~> check {
        val collection = responseAs[TopLevel.Collection]
        collection.data must be empty
      }
    }
    "unmarshal response to TopLevel.Errors" in {
      val route = get {
        complete(
          HttpResponse(
            status = StatusCodes.BadRequest,
            entity = HttpEntity(
              MediaTypes.`application/vnd.api+json`,
              TopLevel
                .Errors(
                  meta = Map.empty,
                  jsonapi = None,
                  links = Map.empty,
                  errors = Set(ErrorObject(
                    id = None,
                    links = Map.empty,
                    status = Some(StatusCodes.BadRequest.intValue.toString),
                    code = None,
                    title = Some("title"),
                    detail = Some("detail"),
                    source = None,
                    meta = Map.empty
                  ))
                )
                .toJson
                .prettyPrint
            )
          ))
      }
      Get("/") ~> route ~> check {
        val errors     = responseAs[TopLevel.Errors]
        val firstError = errors.errors.head

        firstError.title must beSome("title")
      }
    }
    "unmarshal response to jsonapi entity" in {
      val route = get {
        complete(HttpResponse(StatusCodes.OK, entity = HttpEntity(rawOne(thang).prettyPrint)))
      }
      Get("/") ~> route ~> check {
        val thangResponse = responseAs[Thang]
        thangResponse must be equalTo thang
      }
    }
  }
}
