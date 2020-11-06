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

import io.lemonlabs.uri.Url
import io.lemonlabs.uri.typesafe.dsl._
import org.specs2.mutable._
import _root_.spray.json.DefaultJsonProtocol._
import _root_.spray.json._

class MetaSpec extends Specification {
  implicit val apiRoot: com.qvantel.jsonapi.ApiRoot = ApiRoot(None)

  @jsonApiResource final case class Test(id: String, meta: Map[String, Meta])

  val versioned: MetaProfile = new MetaProfile {
    override def alias     = "versioned"
    override def link: Url = "/versioned.html"
  }

  "meta parsing and writing" >> {
    "works for default write and read back loop" >> {

      implicit val profiles: Set[MetaProfile] = Set(versioned)

      val meta: Meta = UntypedMeta(Map("version" -> "1").toJson)

      val t1 = Test("test", Map("versioned" -> meta))
      val t2 = Test("test 2", Map.empty)

      val t1Json = rawOne(t1)
      val t2Json = rawOne(t2)

      val t1Parsed = readOne[Test](t1Json.asJsObject())
      val t2Parsed = readOne[Test](t2Json.asJsObject())

      t1Parsed must be equalTo t1
      t2Parsed must be equalTo t2
    }

    "throw exception if meta is not an object" >> {
      val json =
        """
          |{
          |  "data": {
          |    "attributes": {
          |
          |    },
          |    "links": {
          |      "self": "/tests/test"
          |    },
          |    "id": "test",
          |    "meta": "this will break",
          |    "type": "tests"
          |  },
          |  "aliases": {
          |    "versioned": "/versioned.html"
          |  },
          |  "links": {
          |    "profile": ["/versioned.html"]
          |  }
          |}
        """.stripMargin.parseJson.asJsObject

      readOne[Test](json) must throwA[DeserializationException]("expected meta to be a json object but got")
    }
  }
}
