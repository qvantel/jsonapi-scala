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

import org.specs2.mutable._
import shapeless.{:+:, CNil, Poly1}
import _root_.spray.json.DefaultJsonProtocol._
import _root_.spray.json._

class JsonOptionSpec extends Specification {
  var nullRef: AnyRef = null

  "apply" should {
    "work for None" in {
      JsonOption(None) must be equalTo JsonAbsent
    }
    "work for Some" in {
      JsonOption(Some("test")) must be equalTo JsonSome("test")
    }
    "work for null references" in {
      JsonOption(nullRef) must be equalTo JsonNull
    }
    "work for actual data" in {
      JsonOption(1) must be equalTo JsonSome(1)
    }
  }

  "toOption" should {
    "work for JsonAbsent" in {
      JsonOption(None).toOption must be equalTo None
    }
    "work for JsonNull" in {
      JsonOption(nullRef).toOption must be equalTo None
    }
    "work for JsonSome" in {
      JsonOption(1).toOption must be equalTo Some(1)
    }
  }

  "map" should {
    "work for JsonAbsent" in {
      JsonOption(None).map(_.toString) must be equalTo JsonAbsent
    }
    "work for JsonNull" in {
      JsonOption(nullRef).map(_.toString) must be equalTo JsonNull
    }
    "work for JsonSome" in {
      JsonOption(1).map(_.toString) must be equalTo JsonSome("1")
    }
  }

  // also implements the complete scala Option api (except the toIterable auto implicit conversion)

  "JsonOptionFormat" should {
    "read" in {
      "parse existing value" in {
        val json = """ 1 """.parseJson
        json.convertTo[JsonOption[Int]] must be equalTo JsonSome(1)
      }
      "parse in null value" in {
        val json = """ null """.parseJson
        json.convertTo[JsonOption[Int]] must be equalTo JsonNull
      }
    }
    "write" in {
      "write JsonSome" in {
        val json = JsonOption(1).toJson
        json must be equalTo """1""".parseJson
      }
      "write JsonAbsent" in {
        val json = (JsonAbsent: JsonOption[Int]).toJson
        json must be equalTo """null""".parseJson
      }
      "write JsonNull" in {
        val json = (JsonNull: JsonOption[Int]).toJson
        json must be equalTo """null""".parseJson
      }
    }
  }

  "macro support" should {
    implicit val apiRoot = ApiRoot.empty

    @jsonApiResource final case class Test(id: String, test: Boolean, t: JsonOption[Int], number: Int)
    val tSome = Test("t", true, JsonSome(1), 1)
    val tNone = Test("t", true, JsonAbsent, 1)
    val tNull = Test("t", true, JsonNull, 1)

    @jsonApiResource("tests", "no-id") final case class NoIdTest(test: Boolean, t: JsonOption[Int], number: Int)
    val noIdSome = NoIdTest(true, JsonSome(1), 1)
    val noIdNone = NoIdTest(true, JsonAbsent, 1)
    val noIdNull = NoIdTest(true, JsonNull, 1)

    "produce correct JsonApiWriter for JsonSome case" in {
      val generated = rawOne[Test](tSome)
      val reference =
        """
          |{
          |	"data": {
          |		"type": "tests",
          |		"attributes": {
          |			"t": 1,
          |     "test": true,
          |     "number": 1
          |		},
          |		"id": "t",
          |		"links": {
          |			"self": "/tests/t"
          |		}
          |	}
          |}
        """.stripMargin.parseJson.asJsObject

      generated must be equalTo reference

      val generated2 = rawOne[NoIdTest](noIdSome)
      val reference2 =
        """
          |{
          |	"data": {
          |		"type": "tests",
          |		"attributes": {
          |			"t": 1,
          |     "test": true,
          |     "number": 1
          |		}
          |	}
          |}
        """.stripMargin.parseJson.asJsObject

      generated2 must be equalTo reference2
    }

    "produce correct JsonApiWriter for JsonAbsent case" in {
      val generated = rawOne[Test](tNone)
      val reference =
        """
          |{
          |	"data": {
          |		"type": "tests",
          |		"attributes": {
          |     "test": true,
          |     "number": 1
          |		},
          |		"id": "t",
          |		"links": {
          |			"self": "/tests/t"
          |		}
          |	}
          |}
        """.stripMargin.parseJson.asJsObject

      generated must be equalTo reference

      val generated2 = rawOne[NoIdTest](noIdNone)
      val reference2 =
        """
          |{
          |	"data": {
          |		"type": "tests",
          |		"attributes": {
          |     "test": true,
          |     "number": 1
          |		}
          |	}
          |}
        """.stripMargin.parseJson.asJsObject

      generated2 must be equalTo reference2
    }

    "produce correct JsonApiWriter for JsonNull case" in {
      val generated = rawOne[Test](tNull)
      val reference =
        """
          |{
          |	"data": {
          |		"type": "tests",
          |		"attributes": {
          |			"t": null,
          |     "test": true,
          |     "number": 1
          |		},
          |		"id": "t",
          |		"links": {
          |			"self": "/tests/t"
          |		}
          |	}
          |}
        """.stripMargin.parseJson.asJsObject

      generated must be equalTo reference

      val generated2 = rawOne[NoIdTest](noIdNull)
      val reference2 =
        """
          |{
          |	"data": {
          |		"type": "tests",
          |		"attributes": {
          |     "t": null,
          |     "test": true,
          |     "number": 1
          |		}
          |	}
          |}
        """.stripMargin.parseJson.asJsObject

      generated2 must be equalTo reference2
    }

    "produce correct JsonApiReader for JsonSome case" in {
      val json =
        """
          |{
          |	"data": {
          |		"type": "tests",
          |		"attributes": {
          |			"t": 1,
          |     "test": true,
          |     "number": 1
          |		},
          |		"id": "t",
          |		"links": {
          |			"self": "/tests/t"
          |		}
          |	}
          |}
        """.stripMargin.parseJson.asJsObject
      val t = readOne[Test](json)
      t must be equalTo tSome

      val json2 =
        """
          |{
          |	"data": {
          |		"type": "tests",
          |		"attributes": {
          |			"t": 1,
          |     "test": true,
          |     "number": 1
          |		}
          |	}
          |}
        """.stripMargin.parseJson.asJsObject
      val t2 = readOne[NoIdTest](json2)
      t2 must be equalTo noIdSome
    }

    "produce correct JsonApiReader for JsonNull case" in {
      val json =
        """
          |{
          |	"data": {
          |		"type": "tests",
          |		"attributes": {
          |			"t": null,
          |     "test": true,
          |     "number": 1
          |		},
          |		"id": "t",
          |		"links": {
          |			"self": "/tests/t"
          |		}
          |	}
          |}
        """.stripMargin.parseJson.asJsObject
      val t = readOne[Test](json)
      t must be equalTo tNull

      val json2 =
        """
          |{
          |	"data": {
          |		"type": "tests",
          |		"attributes": {
          |			"t": null,
          |     "test": true,
          |     "number": 1
          |		}
          |	}
          |}
        """.stripMargin.parseJson.asJsObject
      val t2 = readOne[NoIdTest](json2)
      t2 must be equalTo noIdNull
    }

    "produce correct JsonApiReader for JsonAbsent case" in {
      val json =
        """
          |{
          |	"data": {
          |		"type": "tests",
          |		"attributes": {
          |     "test": true,
          |     "number": 1
          |		},
          |		"id": "t",
          |		"links": {
          |			"self": "/tests/t"
          |		}
          |	}
          |}
        """.stripMargin.parseJson.asJsObject
      val t = readOne[Test](json)
      t must be equalTo tNone

      val json2 =
        """
          |{
          |	"data": {
          |		"type": "tests",
          |		"attributes": {
          |     "test": true,
          |     "number": 1
          |		}
          |	}
          |}
        """.stripMargin.parseJson.asJsObject
      val t2 = readOne[NoIdTest](json2)
      t2 must be equalTo noIdNone
    }
  }

  "pattern matching" should {
    "work for JsonEmpty" in {
      val absent: JsonOption[Int] = JsonAbsent

      val isJsonEmpty = absent match {
        case _: JsonEmpty => true
        case _            => false
      }

      isJsonEmpty must beTrue
    }
  }

  "json option to one relation read and write" in {
    implicit val apiRoot = ApiRoot.empty

    @jsonApiResource final case class Author(id: String, name: String)
    @jsonApiResource final case class JsonMaybe(id: String, author: JsonOption[ToOne[Author]])

    @jsonApiResource final case class Nested(id: String, author: JsonOption[ToOne[Author]])
    @jsonApiResource final case class NestedJsonMaybe(id: String, author: JsonOption[ToOne[Nested]])

    val properJsonOptional =
      """
        |{
        |  "id": "test",
        |  "type": "json-maybes",
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
        |  "type": "json-maybes",
        |  "relationships": {
        |    "author": {
        |    }
        |  }
        |}
      """.stripMargin.parseJson

    val dataNullJsonOptional =
      """
        |{
        |  "id": "test",
        |  "type": "json-maybes",
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
        |  "type": "json-maybes",
        |  "relationships": {
        |    "author": null
        |  }
        |}
      """.stripMargin.parseJson

    val dataAbsentJsonOptional =
      """
        |{
        |  "id": "test",
        |  "type": "json-maybes",
        |  "relationships": {
        |  }
        |}
      """.stripMargin.parseJson

    val properJsonOptionalObj     = JsonMaybe("test", JsonSome(ToOne.reference("test")))
    val dataNullJsonOptionalObj   = JsonMaybe("test", JsonNull)
    val dataAbsentJsonOptionalObj = JsonMaybe("test", JsonAbsent)

    // reads
    implicitly[JsonApiFormat[JsonMaybe]].read(properJsonOptional, Set.empty) must be equalTo properJsonOptionalObj
    implicitly[JsonApiFormat[JsonMaybe]]
      .read(missingDataJsonOptional, Set.empty) must throwA[DeserializationException](
      message = "expected 'data', 'links' or 'meta' in 'author' in relationships json")
    implicitly[JsonApiFormat[JsonMaybe]].read(dataNullJsonOptional, Set.empty) must be equalTo dataNullJsonOptionalObj
    implicitly[JsonApiFormat[JsonMaybe]]
      .read(authorNullJsonOptional, Set.empty) must be equalTo dataNullJsonOptionalObj
    implicitly[JsonApiFormat[JsonMaybe]]
      .read(dataAbsentJsonOptional, Set.empty) must be equalTo dataAbsentJsonOptionalObj

    // writes
    readOne[JsonMaybe](rawOne(properJsonOptionalObj), Set("author")) must be equalTo properJsonOptionalObj
    readOne[JsonMaybe](rawOne(dataNullJsonOptionalObj), Set("author")) must be equalTo dataNullJsonOptionalObj
    readOne[JsonMaybe](rawOne(dataAbsentJsonOptionalObj), Set("author")) must be equalTo dataAbsentJsonOptionalObj
  }

  "json option poly to one relation read and write" in {
    implicit val apiRoot = ApiRoot.empty

    @jsonApiResource final case class Person(id: String, name: String)
    @jsonApiResource final case class Company(id: String, name: String)

    type Author = Person :+: Company :+: CNil
    implicit object AuthorPolyIdentifiable extends PolyIdentifiable[Author] {
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

    @jsonApiResource final case class JsonMaybe(id: String, author: JsonOption[PolyToOne[Author]])

    val polyToOneObj = JsonMaybe("test", JsonSome(PolyToOne.reference[Author, Person]("jani")))

    // read / writes
    readOne[JsonMaybe](rawOne(polyToOneObj), Set("author")) must be equalTo polyToOneObj
  }
}
