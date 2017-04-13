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
package com.qvantel.jsonapi.spray

import org.specs2.mutable.Specification
import _root_.spray.http.StatusCodes._
import _root_.spray.http.{ContentType, MediaTypes}
import _root_.spray.json.DefaultJsonProtocol._
import _root_.spray.json.lenses.JsonLenses._
import _root_.spray.json.{JsArray, JsonParser}
import _root_.spray.routing.Directives
import _root_.spray.testkit.Specs2RouteTest

class SprayExceptionHandlerSpec extends Specification with Directives with Specs2RouteTest {
  class TestSprayExceptionHandler extends SprayExceptionHandler

  val testSprayExceptionHandler = new TestSprayExceptionHandler
  private[this] val wrap        = handleExceptions(testSprayExceptionHandler.defaultSprayExceptionHandler)
  val JSON                      = ContentType(MediaTypes.`application/vnd.api+json`)

  "The spray ExceptionHandler" should {
    "Respond with InternalServerError and specified error message" in {
      Get() ~> wrap {
        failWith(new Exception("Specified error message"))
      } ~> check {
        status must_== InternalServerError
        contentType must_== JSON

        val json  = JsonParser(body.asString)
        val error = json.extract[JsArray]('errors).elements.headOption
        error.map(_.extract[String]('detail)) must beSome("Specified error message")
      }
    }

    "Respond with InternalServerError and default error message" in {
      Get() ~> wrap {
        failWith(new Exception)
      } ~> check {
        status must_== InternalServerError
        contentType must_== JSON

        val json  = JsonParser(body.asString)
        val error = json.extract[JsArray]('errors).elements.headOption
        error.map(_.extract[String]('detail)) must beSome(InternalServerError.defaultMessage)
      }
    }
  }
}
