package com.qvantel.jsonapi.client.http4s

import scala.language.higherKinds

import cats._
import cats.effect._
import cats.implicits._
import cats.data._
import org.http4s._
import org.http4s.headers.`Content-Type`
import spray.json._

import com.qvantel.jsonapi.{JsonApiReader, JsonApiWriter, readCollection, readOne, rawOne, rawCollection}

final case class Include(include: Set[String])

trait JsonApiInstances extends JsonApiInstancesLowPrio {
  implicit def jsonapiJsObjectDecoder[F[_]: Effect](implicit include: Include): EntityDecoder[F, JsObject] =
    EntityDecoder.decodeBy(mediaType) { msg: Message[F] =>
      EitherT {
        msg.as[String].map(s => s.parseJson.asJsObject.asRight[DecodeFailure])
      }
    }

  implicit def jsonapiDecoder[F[_]: Effect, A: JsonApiReader](implicit include: Include): EntityDecoder[F, A] =
    EntityDecoder.decodeBy(mediaType) { msg: Message[F] =>
      EitherT {
        msg.as[JsObject].map(json => readOne[A](json, include.include).asRight[DecodeFailure])
      }
    }

  implicit def jsonapiListDecoder[F[_]: Effect, A: JsonApiReader](
      implicit include: Include): EntityDecoder[F, List[A]] = EntityDecoder.decodeBy(mediaType) { msg: Message[F] =>
    EitherT {
      msg.as[JsObject].map(json => readCollection[A](json, include.include).toList.asRight[DecodeFailure])
    }
  }
}

trait JsonApiInstancesLowPrio {
  val mediaType: MediaType = MediaType.`application/vnd.api+json`

  implicit def jsonapiJsObjectDecoder[F[_]: Effect]: EntityDecoder[F, JsObject] =
    EntityDecoder.decodeBy(mediaType) { msg: Message[F] =>
      EitherT {
        msg.as[String].map(s => s.parseJson.asJsObject.asRight[DecodeFailure])
      }
    }

  implicit def jsonapiDecoder[F[_]: Effect, A: JsonApiReader]: EntityDecoder[F, A] =
    EntityDecoder.decodeBy(mediaType) { msg: Message[F] =>
      EitherT {
        msg.as[JsObject].map(json => readOne[A](json).asRight[DecodeFailure])
      }
    }

  implicit def jsonapiListDecoder[F[_]: Effect, A: JsonApiReader]: EntityDecoder[F, List[A]] =
    EntityDecoder.decodeBy(mediaType) { msg: Message[F] =>
      EitherT {
        msg.as[JsObject].map(json => readCollection[A](json).toList.asRight[DecodeFailure])
      }
    }

  implicit def jsonapiEncoder[F[_], A: JsonApiWriter](implicit F: Applicative[F]): EntityEncoder[F, A] =
    EntityEncoder
      .stringEncoder(F, Charset.`UTF-8`)
      .contramap[A](entity => rawOne(entity).compactPrint)
      .withContentType(`Content-Type`(mediaType))

  implicit def jsonapiListEncoder[F[_], A: JsonApiWriter](implicit F: Applicative[F]): EntityEncoder[F, List[A]] =
    EntityEncoder
      .stringEncoder(F, Charset.`UTF-8`)
      .contramap[List[A]](entities => rawCollection(entities).compactPrint)
      .withContentType(`Content-Type`(mediaType))
}

object JsonApiInstances extends JsonApiInstances
