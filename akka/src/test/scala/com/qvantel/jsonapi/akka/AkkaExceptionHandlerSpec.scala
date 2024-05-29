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
package com.qvantel.jsonapi.akka

import _root_.spray.json.DefaultJsonProtocol._
import _root_.spray.json.lenses.JsonLenses._
import _root_.spray.json.{JsArray, JsonParser}

import akka.event.Logging
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.model.headers._
import akka.http.scaladsl.server.AuthenticationFailedRejection.{CredentialsMissing, CredentialsRejected}
import akka.http.scaladsl.server._
import akka.http.scaladsl.testkit.Specs2RouteTest

import org.specs2.mutable.Specification

class AkkaExceptionHandlerSpec extends Specification with Directives with Specs2RouteTest {
  class TestAkkaExceptionHandler extends AkkaExceptionHandlerTrait

  val testAkkaExceptionHandler = new TestAkkaExceptionHandler
  implicit val log             = Logging(system, "Log")
  private[this] val wrap       = Directives.handleExceptions(testAkkaExceptionHandler.defaultAkkaExceptionHandler)
  val JSON                     = ContentType(MediaTypes.`application/vnd.api+json`)
  val route =
    handleRejections(testAkkaExceptionHandler.defaultAkkaRejectionHandler) {
      path("authenticationMissing") {
        reject(AuthenticationFailedRejection(CredentialsMissing, HttpChallenge("Auth", Some(""))))
      } ~
        path("authenticationRejected") {
          reject(AuthenticationFailedRejection(CredentialsRejected, HttpChallenge("Auth", Some(""))))
        } ~
        path("authorization") {
          reject(AuthorizationFailedRejection)
        } ~
        path("malformedFormField") {
          reject(MalformedFormFieldRejection("nameX", "messageX"))
        } ~
        path("malformedHeader") {
          reject(MalformedHeaderRejection("nameX", "messageX"))
        } ~
        path("malformedQueryParam") {
          reject(MalformedQueryParamRejection("nameX", "messageX"))
        } ~
        path("malformedRequestContent") {
          reject(MalformedRequestContentRejection("messageX", new Exception("")))
        } ~
        path("method") {
          reject(MethodRejection(HttpMethods.GET))
        } ~
        path("scheme") {
          reject(SchemeRejection("schemeX"))
        } ~
        path("missingCookie") {
          reject(MissingCookieRejection("cookieX"))
        } ~
        path("missingFormField") {
          reject(MissingFormFieldRejection("formFieldX"))
        } ~
        path("missingHeader") {
          reject(MissingHeaderRejection("headerX"))
        } ~
        path("missingQueryParam") {
          reject(MissingQueryParamRejection("parameterX"))
        } ~
        path("requestEntityExpected") {
          reject(RequestEntityExpectedRejection)
        } ~
        path("tooManyRanges") {
          reject(TooManyRangesRejection(1))
        } ~
        path("unsatisfiableRange") {
          reject(UnsatisfiableRangeRejection(Range(ByteRange(1000, 2000)).ranges, 1))
        } ~
        path("unacceptedResponseContentType") {
          reject(UnacceptedResponseContentTypeRejection(Set(ContentType(MediaTypes.`application/vnd.api+json`))))
        } ~
        path("unacceptedResponseEncoding") {
          reject(UnacceptedResponseEncodingRejection(HttpEncoding("encodingX")))
        } ~
        path("unsupportedRequestContentType") {
          reject(UnsupportedRequestContentTypeRejection(Set(ContentType(MediaTypes.`application/vnd.api+json`)), None))
        } ~
        path("unsupportedRequestEncoding") {
          reject(UnsupportedRequestEncodingRejection(HttpEncoding("encodingX")))
        } ~
        path("validation") {
          reject(ValidationRejection("messageX"))
        }
    }

  "The Akka ExceptionHandler" should {

    "Respond with InternalServerError and specified error message" in {
      Get() ~> wrap {
        failWith(new Exception("Specified error message"))
      } ~> check {
        status must_== InternalServerError
        contentType must_== JSON

        val json  = JsonParser(responseAs[String])
        val error = json.extract[JsArray]("errors").elements.headOption
        error.map(_.extract[String]("title")) must beSome("Internal Server Error")
        error.map(_.extract[String]("detail")) must beSome("Specified error message")
      }
    }

    "Respond with InternalServerError and default error message" in {
      Get() ~> wrap {
        failWith(new Exception)
      } ~> check {
        status must_== InternalServerError
        contentType must_== JSON

        val json  = JsonParser(responseAs[String])
        val error = json.extract[JsArray]("errors").elements.headOption
        error.map(_.extract[String]("title")) must beSome("Internal Server Error")
        error.map(_.extract[String]("detail")) must beSome(InternalServerError.defaultMessage)
      }
    }

    "Respond with IllegalRequestException and specific error message" in {
      Get() ~> wrap {
        failWith(IllegalRequestException(BadRequest, "infoX"))
      } ~> check {
        status must_== BadRequest
        contentType must_== JSON

        val json  = JsonParser(responseAs[String])
        val error = json.extract[JsArray]("errors").elements.headOption
        error.map(_.extract[String]("title")) must beSome("Illegal Request")
        error.map(_.extract[String]("detail")) must beSome("The request contains bad syntax or cannot be fulfilled.")
      }
    }
  }

  "The Akka RejectionHandler" should {

    "authentication should return 401 with credentialsmissing and a proper jsonapi.org error object" in {
      Get("/authenticationMissing") ~> route ~> check {
        status must_== Unauthorized
        contentType must_== JSON

        val json  = JsonParser(responseAs[String])
        val error = json.extract[JsArray]("errors").elements.headOption
        error.map(_.extract[String]("detail")) must beSome(
          "The resource requires authentication, which was not supplied with the request")
        error.map(_.extract[String]("title")) must beSome("Authentication Failed")
      }
    }

    "authentication should return 401 with credentialsrejected and a proper jsonapi.org error object" in {
      Get("/authenticationRejected") ~> route ~> check {
        status must_== Unauthorized
        contentType must_== JSON

        val json  = JsonParser(responseAs[String])
        val error = json.extract[JsArray]("errors").elements.headOption
        error.map(_.extract[String]("detail")) must beSome("The supplied authentication is invalid")
        error.map(_.extract[String]("title")) must beSome("Authentication Failed")
      }
    }

    "authorization should return 403 and a proper jsonapi.org error object" in {
      Get("/authorization") ~> route ~> check {
        status must_== Forbidden
        contentType must_== JSON

        val json  = JsonParser(responseAs[String])
        val error = json.extract[JsArray]("errors").elements.headOption
        error.map(_.extract[String]("detail")) must beSome(
          "The supplied authentication is not authorized to access this resource")
        error.map(_.extract[String]("title")) must beSome("Authorization Failed")
      }
    }

    "malformedFormField should return 400 and a proper jsonapi.org error object" in {
      Get("/malformedFormField") ~> route ~> check {
        status must_== BadRequest
        contentType must_== JSON

        val json  = JsonParser(responseAs[String])
        val error = json.extract[JsArray]("errors").elements.headOption
        error.map(_.extract[String]("detail")) must beSome("The form field 'nameX' was malformed:\nmessageX")
        error.map(_.extract[String]("title")) must beSome("Malformed Form Field")
      }
    }

    "malformedHeader should return 400 with proper jsonapi.org error object" in {
      Get("/malformedHeader") ~> route ~> check {
        status must_== BadRequest
        contentType must_== JSON

        val json  = JsonParser(responseAs[String])
        val error = json.extract[JsArray]("errors").elements.headOption
        error.map(_.extract[String]("detail")) must beSome("The value of HTTP header 'nameX' was malformed:\nmessageX")
        error.map(_.extract[String]("title")) must beSome("Malformed Header")
      }
    }

    "malformedQueryParam should return 400 with proper jsonapi.org error object" in {
      Get("/malformedQueryParam") ~> route ~> check {
        status must_== BadRequest
        contentType must_== JSON

        val json  = JsonParser(responseAs[String])
        val error = json.extract[JsArray]("errors").elements.headOption
        error.map(_.extract[String]("detail")) must beSome("The query parameter 'nameX' was malformed:\nmessageX")
        error.map(_.extract[String]("title")) must beSome("Malformed Query Param")
      }
    }

    "malformedRequestContent should return 400 with proper jsonapi.org error object" in {
      Get("/malformedRequestContent") ~> route ~> check {
        status must_== BadRequest
        contentType must_== JSON

        val json  = JsonParser(responseAs[String])
        val error = json.extract[JsArray]("errors").elements.headOption
        error.map(_.extract[String]("detail")) must beSome("The request content was malformed:\nmessageX")
        error.map(_.extract[String]("title")) must beSome("Malformed Request Content")
      }
    }

    "method should return 405 with proper jsonapi.org error object" in {
      Get("/method") ~> route ~> check {
        status must_== MethodNotAllowed
        contentType must_== JSON

        val json  = JsonParser(responseAs[String])
        val error = json.extract[JsArray]("errors").elements.headOption
        error.map(_.extract[String]("detail")) must beSome(
          "HTTP method not allowed, supported methods: HttpMethod(GET)")
        error.map(_.extract[String]("title")) must beSome("HTTP method not allowed")
      }
    }

    "scheme should return 400 with proper jsonapi.org error object" in {
      Get("/scheme") ~> route ~> check {
        status must_== BadRequest
        contentType must_== JSON

        val json  = JsonParser(responseAs[String])
        val error = json.extract[JsArray]("errors").elements.headOption
        error.map(_.extract[String]("detail")) must beSome("Uri scheme not allowed, supported schemes: schemeX")
        error.map(_.extract[String]("title")) must beSome("Uri scheme not allowed")
      }
    }

    "missingCookie should return 400 with proper jsonapi.org error object" in {
      Get("/missingCookie") ~> route ~> check {
        status must_== BadRequest
        contentType must_== JSON

        val json  = JsonParser(responseAs[String])
        val error = json.extract[JsArray]("errors").elements.headOption
        error.map(_.extract[String]("detail")) must beSome("Request is missing required cookie 'cookieX'")
        error.map(_.extract[String]("title")) must beSome("Missing Cookie")
      }
    }

    "missingFormField should return 400 with proper jsonapi.org error object" in {
      Get("/missingFormField") ~> route ~> check {
        status must_== BadRequest
        contentType must_== JSON

        val json  = JsonParser(responseAs[String])
        val error = json.extract[JsArray]("errors").elements.headOption
        error.map(_.extract[String]("detail")) must beSome("Request is missing required form field 'formFieldX'")
        error.map(_.extract[String]("title")) must beSome("Missing Form Field")
      }
    }

    "missingHeader should return 400 with proper jsonapi.org error object" in {
      Get("/missingHeader") ~> route ~> check {
        status must_== BadRequest
        contentType must_== JSON

        val json  = JsonParser(responseAs[String])
        val error = json.extract[JsArray]("errors").elements.headOption
        error.map(_.extract[String]("detail")) must beSome("Request is missing required HTTP header 'headerX'")
        error.map(_.extract[String]("title")) must beSome("Missing Header")
      }
    }

    "missingQueryParam should return 400 with proper jsonapi.org error object" in {
      Get("/missingQueryParam") ~> route ~> check {
        status must_== BadRequest
        contentType must_== JSON

        val json  = JsonParser(responseAs[String])
        val error = json.extract[JsArray]("errors").elements.headOption
        error.map(_.extract[String]("detail")) must beSome("Request is missing required query parameter 'parameterX'")
        error.map(_.extract[String]("title")) must beSome("Missing Query Param")
      }
    }

    "requestEntityExpected should return 400 with proper jsonapi.org error object" in {
      Get("/requestEntityExpected") ~> route ~> check {
        status must_== BadRequest
        contentType must_== JSON

        val json  = JsonParser(responseAs[String])
        val error = json.extract[JsArray]("errors").elements.headOption
        error.map(_.extract[String]("detail")) must beSome("Request entity expected but not supplied")
        error.map(_.extract[String]("title")) must beSome("Request Entity Expected")
      }
    }

    "tooManyRanges should return 416 with proper jsonapi.org error object" in {
      Get("/tooManyRanges") ~> route ~> check {
        status must_== RangeNotSatisfiable
        contentType must_== JSON

        val json  = JsonParser(responseAs[String])
        val error = json.extract[JsArray]("errors").elements.headOption
        error.map(_.extract[String]("detail")) must beSome("Request contains too many ranges")
        error.map(_.extract[String]("title")) must beSome("Too Many Ranges")
      }
    }

    "unsatisfiableRange should return 400 with proper jsonapi.org error object" in {
      Get("/unsatisfiableRange") ~> route ~> check {
        status must_== RangeNotSatisfiable
        contentType must_== JSON

        val json  = JsonParser(responseAs[String])
        val error = json.extract[JsArray]("errors").elements.headOption
        error.map(_.extract[String]("detail")) must beSome(
          "None of the following requested Ranges were satisfiable:\n1000-2000")
        error.map(_.extract[String]("title")) must beSome("Unsatisfiable Range")
      }
    }

    "unacceptedResponseContentType should return 406 with proper jsonapi.org error object" in {
      Get("/unacceptedResponseContentType") ~> route ~> check {
        status must_== NotAcceptable
        contentType must_== JSON

        val json  = JsonParser(responseAs[String])
        val error = json.extract[JsArray]("errors").elements.headOption
        error.map(_.extract[String]("detail")) must beSome(
          "Resource representation is only available with these Content-Types:\nContentType(application/vnd.api+json)")
        error.map(_.extract[String]("title")) must beSome("Unaccepted Response Content Type")
      }
    }

    "unacceptedResponseEncoding should return 406 with proper jsonapi.org error object" in {
      Get("/unacceptedResponseEncoding") ~> route ~> check {
        status must_== NotAcceptable
        contentType must_== JSON

        val json  = JsonParser(responseAs[String])
        val error = json.extract[JsArray]("errors").elements.headOption
        error.map(_.extract[String]("detail")) must beSome(
          "Resource representation is only available with these Content-Encodings:\nencodingX")
        error.map(_.extract[String]("title")) must beSome("Unaccepted Response Encoding")
      }
    }

    "unsupportedRequestContentType should return 415 with proper jsonapi.org error object" in {
      Get("/unsupportedRequestContentType") ~> route ~> check {
        status must_== UnsupportedMediaType
        contentType must_== JSON

        val json  = JsonParser(responseAs[String])
        val error = json.extract[JsArray]("errors").elements.headOption
        error.map(_.extract[String]("detail")) must beSome(
          "There was a problem with the requests Content-Type:\napplication/vnd.api+json")
        error.map(_.extract[String]("title")) must beSome("Unsupported Request Content-Type")
      }
    }

    "unsupportedRequestEncoding should return 400 with proper jsonapi.org error object" in {
      Get("/unsupportedRequestEncoding") ~> route ~> check {
        status must_== BadRequest
        contentType must_== JSON

        val json  = JsonParser(responseAs[String])
        val error = json.extract[JsArray]("errors").elements.headOption
        error.map(_.extract[String]("detail")) must beSome(
          "The request Content-Encoding must be the following:\nencodingX")
        error.map(_.extract[String]("title")) must beSome("Unsupported Request Encoding")
      }
    }

    "validation should return 400 with proper jsonapi.org error object" in {
      Get("/validation") ~> route ~> check {
        status must_== BadRequest
        contentType must_== JSON

        val json  = JsonParser(responseAs[String])
        val error = json.extract[JsArray]("errors").elements.headOption
        error.map(_.extract[String]("detail")) must beSome("messageX")
        error.map(_.extract[String]("title")) must beSome("Validation Rejection")
      }
    }
  }
}
