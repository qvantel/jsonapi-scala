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
import _root_.spray.json._
import _root_.spray.json.DefaultJsonProtocol._
import _root_.spray.testkit.Specs2RouteTest
import _root_.spray.routing.HttpService
import _root_.spray.http.{ContentType, MediaTypes}

class RelatedResponseSpec extends Specification with Specs2RouteTest with HttpService {
  def actorRefFactory                               = system
  implicit val apiRoot: com.qvantel.jsonapi.ApiRoot = ApiRoot(None)
  @jsonApiResource final case class Test(id: String)

  val test: Option[Test]      = Some(Test("teståöä•Ωé®")) // test UTF-8
  val emptyTest: Option[Test] = None
  val tests: List[Test]       = List(Test("test 1"), Test("test 2"))
  val emptyTests: List[Test]  = List.empty

  val route = get {
    complete {
      RelatedResponse(test)
    }
  }

  "correctly write to one none case" in {
    RelatedResponse(emptyTest).toResponse must be equalTo JsObject(
      "data" -> JsNull
    )
  }

  "correctly write to one some case" in {
    val answer = rawOne(test.get)

    RelatedResponse(test).toResponse must be equalTo answer
    RelatedResponse(test.get).toResponse must be equalTo answer
  }

  "correctly write to many empty case" in {
    RelatedResponse(emptyTests).toResponse must be equalTo JsObject(
      "data" -> JsArray.empty
    )
  }

  "correctly write to many non-empt case" in {
    val answer = rawCollection(tests)

    RelatedResponse(tests).toResponse must be equalTo answer
    RelatedResponse(tests.toSeq).toResponse must be equalTo answer
    RelatedResponse(tests.toIterable).toResponse must be equalTo answer
    RelatedResponse(tests.toSet).toResponse must be equalTo answer
  }

  "make sure that correct content type is given" in {
    Get("/") ~> route ~> check {
      contentType must be equalTo ContentType(MediaTypes.`application/vnd.api+json`, None)
    }
  }
}
