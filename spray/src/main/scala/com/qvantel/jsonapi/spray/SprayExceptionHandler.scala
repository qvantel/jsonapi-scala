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
 * Neither the name of the <organization> nor the
      names of its contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package com.qvantel.jsonapi.spray

import scala.util.control.NonFatal
import _root_.spray.http.StatusCodes._
import _root_.spray.http._
import _root_.spray.httpx.SprayJsonSupport._
import _root_.spray.json.DefaultJsonProtocol._
import _root_.spray.json._
import _root_.spray.routing.AuthenticationFailedRejection._
import _root_.spray.routing.Directives._
import _root_.spray.routing._
import _root_.spray.util.LoggingContext

import com.qvantel.jsonapi.model.ErrorObject
import com.qvantel.jsonapi.model.ErrorObject._

trait SprayExceptionHandler {
  def jsonApiError(code: StatusCode, title: String, detail: String) =
    JsObject(
      "errors" -> List(
        ErrorObject(id = None,
                    links = Map.empty,
                    status = Some(code.intValue.toString),
                    code = None,
                    title = Some(title),
                    detail = Some(detail),
                    source = None,
                    meta = Map.empty)).toJson)

  def completeJsonApiError(code: StatusCode, title: String, detail: String) =
    complete(
      HttpResponse(status = code,
                   entity =
                     HttpEntity(MediaTypes.`application/vnd.api+json`, jsonApiError(code, title, detail).prettyPrint)))

  val defaultSprayRejectionHandler: PartialFunction[List[Rejection], Route] = {
    case Nil ⇒ completeJsonApiError(NotFound, "Resource not found", "The requested resource could not be found.")

    case AuthenticationFailedRejection(cause, challengeHeaders) :: _ ⇒
      val rejectionMessage = cause match {
        case CredentialsMissing  ⇒ "The resource requires authentication, which was not supplied with the request"
        case CredentialsRejected ⇒ "The supplied authentication is invalid"
      }
      { ctx ⇒
        ctx.complete((Unauthorized, jsonApiError(Unauthorized, "Authentication Failed", rejectionMessage)))
      }

    case AuthorizationFailedRejection :: _ ⇒
      completeJsonApiError(Forbidden,
                           "Authorization Failed",
                           "The supplied authentication is not authorized to access this resource")

    case CorruptRequestEncodingRejection(msg) :: _ ⇒
      completeJsonApiError(BadRequest, "Corrupt Request Encoding", "The requests encoding is corrupt:\n" + msg)

    case MalformedFormFieldRejection(name, msg, _) :: _ ⇒
      completeJsonApiError(BadRequest, "Malformed Form Field", "The form field '" + name + "' was malformed:\n" + msg)

    case MalformedHeaderRejection(headerName, msg, _) :: _ ⇒
      completeJsonApiError(BadRequest,
                           "Malformed Header",
                           s"The value of HTTP header '$headerName' was malformed:\n" + msg)

    case MalformedQueryParamRejection(name, msg, _) :: _ ⇒
      completeJsonApiError(BadRequest,
                           "Malformed Query Param",
                           "The query parameter '" + name + "' was malformed:\n" + msg)

    case MalformedRequestContentRejection(msg, _) :: _ ⇒
      completeJsonApiError(BadRequest, "Malformed Request Content", "The request content was malformed:\n" + msg)

    case rejections @ (MethodRejection(_) :: _) ⇒
      val methods = rejections.collect { case MethodRejection(method) ⇒ method }
      complete(
        (MethodNotAllowed,
         jsonApiError(MethodNotAllowed,
                      "HTTP method not allowed",
                      "HTTP method not allowed, supported methods: " + methods.mkString(", "))))

    case rejections @ (SchemeRejection(_) :: _) ⇒
      val schemes = rejections.collect { case SchemeRejection(scheme) ⇒ scheme }
      completeJsonApiError(BadRequest,
                           "Uri scheme not allowed",
                           "Uri scheme not allowed, supported schemes: " + schemes.mkString(", "))

    case MissingCookieRejection(cookieName) :: _ ⇒
      completeJsonApiError(BadRequest, "Missing Cookie", s"Request is missing required cookie '$cookieName'")

    case MissingFormFieldRejection(fieldName) :: _ ⇒
      completeJsonApiError(BadRequest, "Missing Form Field", s"Request is missing required form field '$fieldName'")

    case MissingHeaderRejection(headerName) :: _ ⇒
      completeJsonApiError(BadRequest, "Missing Header", s"Request is missing required HTTP header '$headerName'")

    case MissingQueryParamRejection(paramName) :: _ ⇒
      completeJsonApiError(NotFound,
                           "Missing Query Param",
                           s"Request is missing required query parameter '$paramName'")

    case RequestEntityExpectedRejection :: _ ⇒
      completeJsonApiError(BadRequest, "Request Entity Expected", "Request entity expected but not supplied")

    case TooManyRangesRejection(_) :: _ ⇒
      completeJsonApiError(RequestedRangeNotSatisfiable, "Too Many Ranges", "Request contains too many ranges.")

    case UnsatisfiableRangeRejection(unsatisfiableRanges, actualEntityLength) :: _ ⇒
      complete(
        (RequestedRangeNotSatisfiable,
         jsonApiError(
           RequestedRangeNotSatisfiable,
           "Unsatisfiable Range",
           unsatisfiableRanges.mkString("None of the following requested Ranges were satisfiable:\n", "\n", ""))))

    case rejections @ (UnacceptedResponseContentTypeRejection(_) :: _) ⇒
      val supported = rejections.flatMap {
        case UnacceptedResponseContentTypeRejection(supported) ⇒ supported
        case _                                                 ⇒ Nil
      }
      completeJsonApiError(
        NotAcceptable,
        "Unaccepted Response Content Type",
        "Resource representation is only available with these Content-Types:\n" + supported.map(_.value).mkString("\n")
      )

    case rejections @ (UnacceptedResponseEncodingRejection(_) :: _) ⇒
      val supported = rejections.collect { case UnacceptedResponseEncodingRejection(supported) ⇒ supported }
      completeJsonApiError(
        NotAcceptable,
        "Unaccepted Response Encoding",
        "Resource representation is only available with these Content-Encodings:\n" + supported
          .map(_.value)
          .mkString("\n")
      )

    case rejections @ (UnsupportedRequestContentTypeRejection(_) :: _) ⇒
      val supported = rejections.collect { case UnsupportedRequestContentTypeRejection(supported) ⇒ supported }
      completeJsonApiError(UnsupportedMediaType,
                           "Unsupported Request Content-Type",
                           "There was a problem with the requests Content-Type:\n" + supported.mkString(" or "))

    case rejections @ (UnsupportedRequestEncodingRejection(_) :: _) ⇒
      val supported = rejections.collect { case UnsupportedRequestEncodingRejection(supported) ⇒ supported }
      completeJsonApiError(
        BadRequest,
        "Unsupported Request Encoding",
        "The requests Content-Encoding must be one the following:\n" + supported.map(_.value).mkString("\n"))

    case ValidationRejection(msg, _) :: _ ⇒
      completeJsonApiError(BadRequest, "Validation Rejection", msg)
  }

  def defaultSprayExceptionHandler(implicit settings: RoutingSettings,
                                   log: LoggingContext): PartialFunction[Throwable, Route] = {
    case e: IllegalRequestException ⇒
      ctx ⇒
        {
          log.warning("Illegal request {}\n\t{}\n\tCompleting with '{}' response", ctx.request, e.getMessage, e.status)
          ctx.complete(
            (e.status, jsonApiError(e.status, "Illegal Request", e.info.format(settings.verboseErrorMessages))))
        }
    case e: RequestProcessingException ⇒
      ctx ⇒
        {
          log.warning("Request {} could not be handled normally\n\t{}\n\tCompleting with '{}' response",
                      ctx.request,
                      e.getMessage,
                      e.status)
          ctx.complete(
            (e.status,
             jsonApiError(e.status,
                          "Request could not be handled normally",
                          e.info.format(settings.verboseErrorMessages))))
        }
    case NonFatal(e) ⇒
      ctx ⇒
        {
          log.error(e, "Error during processing of request {}", ctx.request)
          ctx.complete(
            (InternalServerError,
             jsonApiError(InternalServerError,
                          InternalServerError.reason,
                          if (e.getMessage != null) e.getMessage else InternalServerError.defaultMessage)))
        }
  }
}
