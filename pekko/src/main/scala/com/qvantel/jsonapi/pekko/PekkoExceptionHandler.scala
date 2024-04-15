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

import _root_.spray.json.DefaultJsonProtocol._
import _root_.spray.json._
import com.qvantel.jsonapi.model.ErrorObject
import org.apache.pekko.event.LoggingAdapter
import org.apache.pekko.http.scaladsl.model.StatusCodes._
import org.apache.pekko.http.scaladsl.model._
import org.apache.pekko.http.scaladsl.server.AuthenticationFailedRejection._
import org.apache.pekko.http.scaladsl.server.Directives._
import org.apache.pekko.http.scaladsl.server._
import org.apache.pekko.http.scaladsl.settings.RoutingSettings

import scala.util.control.NonFatal

trait PekkoExceptionHandlerTrait {

  import PekkoExceptionHandlerObject._

  val defaultPekkoRejectionHandler: RejectionHandler = RejectionHandler
    .newBuilder()
    .handle {
      case AuthenticationFailedRejection(cause, _) =>
        val rejectionMessage = cause match {
          case CredentialsMissing  => "The resource requires authentication, which was not supplied with the request"
          case CredentialsRejected => "The supplied authentication is invalid"
        }
        completeJsonApiError(Unauthorized, "Authentication Failed", rejectionMessage)

      case AuthorizationFailedRejection =>
        completeJsonApiError(Forbidden,
                             "Authorization Failed",
                             "The supplied authentication is not authorized to access this resource")

      case MalformedFormFieldRejection(name, msg, _) =>
        completeJsonApiError(BadRequest, "Malformed Form Field", "The form field '" + name + "' was malformed:\n" + msg)

      case MalformedHeaderRejection(headerName, msg, _) =>
        completeJsonApiError(BadRequest,
                             "Malformed Header",
                             s"The value of HTTP header '$headerName' was malformed:\n" + msg)

      case MalformedQueryParamRejection(name, msg, _) =>
        completeJsonApiError(BadRequest,
                             "Malformed Query Param",
                             "The query parameter '" + name + "' was malformed:\n" + msg)

      case MalformedRequestContentRejection(msg, _) =>
        completeJsonApiError(BadRequest, "Malformed Request Content", "The request content was malformed:\n" + msg)

      case MethodRejection(supported) =>
        completeJsonApiError(MethodNotAllowed,
                             "HTTP method not allowed",
                             "HTTP method not allowed, supported methods: " + supported.toString)

      case SchemeRejection(supported) =>
        completeJsonApiError(BadRequest,
                             "Uri scheme not allowed",
                             "Uri scheme not allowed, supported schemes: " + supported)

      case MissingCookieRejection(cookieName) =>
        completeJsonApiError(BadRequest, "Missing Cookie", s"Request is missing required cookie '$cookieName'")

      case MissingFormFieldRejection(fieldName) =>
        completeJsonApiError(BadRequest, "Missing Form Field", s"Request is missing required form field '$fieldName'")

      case MissingHeaderRejection(headerName) =>
        completeJsonApiError(BadRequest, "Missing Header", s"Request is missing required HTTP header '$headerName'")

      case MissingQueryParamRejection(paramName) =>
        completeJsonApiError(BadRequest,
                             "Missing Query Param",
                             s"Request is missing required query parameter '$paramName'")

      case RequestEntityExpectedRejection =>
        completeJsonApiError(BadRequest, "Request Entity Expected", "Request entity expected but not supplied")

      case TooManyRangesRejection(_) =>
        completeJsonApiError(RangeNotSatisfiable, "Too Many Ranges", "Request contains too many ranges")

      case UnsatisfiableRangeRejection(unsatisfiableRanges, _) =>
        completeJsonApiError(
          RangeNotSatisfiable,
          "Unsatisfiable Range",
          unsatisfiableRanges.mkString("None of the following requested Ranges were satisfiable:\n", "\n", "")
        )

      case UnacceptedResponseContentTypeRejection(supported) =>
        completeJsonApiError(
          NotAcceptable,
          "Unaccepted Response Content Type",
          "Resource representation is only available with these Content-Types:\n" + supported.mkString("\n")
        )

      case UnacceptedResponseEncodingRejection(supported) =>
        completeJsonApiError(
          NotAcceptable,
          "Unaccepted Response Encoding",
          "Resource representation is only available with these Content-Encodings:\n" + supported.mkString("\n")
        )

      case UnsupportedRequestContentTypeRejection(supported) =>
        completeJsonApiError(UnsupportedMediaType,
                             "Unsupported Request Content-Type",
                             "There was a problem with the requests Content-Type:\n" + supported.mkString(" or "))

      case UnsupportedRequestEncodingRejection(supported) =>
        completeJsonApiError(BadRequest,
                             "Unsupported Request Encoding",
                             "The request Content-Encoding must be the following:\n" + supported.value)

      case ValidationRejection(msg, _) =>
        completeJsonApiError(BadRequest, "Validation Rejection", msg)
    }
    .handleNotFound {
      completeJsonApiError(NotFound, NotFound.reason, NotFound.defaultMessage)
    }
    .result()

  def defaultPekkoExceptionHandler(implicit settings: RoutingSettings, log: LoggingAdapter): ExceptionHandler =
    ExceptionHandler {
      case e: IllegalRequestException =>
        extractRequestContext { ctx =>
          log.warning("Illegal request {}\n\t{}\n\tCompleting with '{}' response", ctx.request, e.getMessage, e.status)
          complete(jsonApiErrorResponse(e.status, "Illegal Request", e.info.format(settings.verboseErrorMessages)))
        }
      case NonFatal(e) =>
        extractRequestContext { ctx =>
          log.error(e, "Error during processing of request {}", ctx.request)
          complete(
            jsonApiErrorResponse(InternalServerError,
                                 InternalServerError.reason,
                                 if (e.getMessage != null) e.getMessage else InternalServerError.defaultMessage))
        }
    }
}

object PekkoExceptionHandlerObject extends Rejection {

  def jsonApiError(code: StatusCode, title: String, detail: String): JsValue =
    JsObject("errors" -> List(
      ErrorObject(status = Some(code.intValue.toString), title = Some(title), detail = Some(detail))).toJson)

  def jsonApiErrorResponse(code: StatusCode, title: String, detail: String): HttpResponse =
    HttpResponse(
      status = code,
      entity =
        HttpEntity(ContentType(MediaTypes.`application/vnd.api+json`), jsonApiError(code, title, detail).prettyPrint))

  def completeJsonApiError(code: StatusCode, title: String, detail: String): Route =
    complete(jsonApiErrorResponse(code, title, detail))
}
