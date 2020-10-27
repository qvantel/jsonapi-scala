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

import io.lemonlabs.uri.typesafe.dsl._
import _root_.spray.json._
import _root_.spray.json.DefaultJsonProtocol._

class ApiRootSpec extends org.specs2.mutable.Specification {

  "ApiRoot" should {
    "take effect when proof for it exists" in {
      implicit val apiRoot: com.qvantel.jsonapi.ApiRoot = ApiRoot(Some("/api"))
      @jsonApiResource case class Test(id: String, rel: ToOne[Child])
      @jsonApiResource case class Child(id: String)

      val json =
        """
          |{
          |  "relationships":{
          |    "rel":{
          |      "data":{
          |        "type":"children","id":"2"
          |      },
          |      "links":{
          |        "related":"/api/tests/1/rel"
          |      }
          |    }
          |  },
          |  "links":{"self":"/api/tests/1"},
          |  "id":"1",
          |  "type":"tests"
          |}
        """.stripMargin.parseJson

      implicitly[JsonApiWriter[Test]].write(Test("1", ToOne.loaded(Child("2")))) must beEqualTo(json)
    }
  }
}
