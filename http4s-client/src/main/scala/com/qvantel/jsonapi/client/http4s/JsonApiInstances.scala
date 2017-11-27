package com.qvantel.jsonapi.client.http4s

import scala.language.higherKinds

import cats.Applicative
import cats.data.EitherT
import cats.effect._
import cats.implicits._
import org.http4s.{
  Charset,
  DecodeFailure,
  DecodeResult,
  EntityDecoder,
  EntityEncoder,
  MalformedMessageBodyFailure,
  MediaType,
  Message
}
import org.http4s.headers.`Content-Type`
import spray.json._
import spray.json.JsonParser.ParsingException

import com.qvantel.jsonapi.{JsonApiReader, JsonApiWriter, readCollection, readOne, rawOne, rawCollection}

final case class Include(include: Set[String])

trait JsonApiInstances {
  val mediaType: MediaType = MediaType.fromKey(("application", "vnd.api+json"))

  implicit def jsonDecoder[F[_]: Effect, A: JsonApiReader](implicit include: Include): EntityDecoder[F, A] =
    EntityDecoder.decodeBy[F, A](mediaType) { msg =>
      msg.attemptAs[String].flatMap { x =>
        val t: Either[DecodeFailure, A] = try {
          Right(readOne[A](x.parseJson.asJsObject, include.include))
        } catch {
          case e: ParsingException => Left(MalformedMessageBodyFailure(s"Invalid JSON: ${e.summary}", Some(e)))
        }

        EitherT.fromEither(t)
      }
    }

  implicit def listDecoder[F[_]: Effect, A: JsonApiReader](implicit include: Include): EntityDecoder[F, List[A]] =
    EntityDecoder.decodeBy[F, List[A]](mediaType) { msg =>
      msg.attemptAs[String].flatMap { x =>
        val t: Either[DecodeFailure, List[A]] = try {
          Right(readCollection[A](x.parseJson.asJsObject, include.include).toList)
        } catch {
          case e: ParsingException => Left(MalformedMessageBodyFailure(s"Invalid JSON: ${e.summary}", Some(e)))
        }

        EitherT.fromEither(t)
      }
    }

  implicit def optionDecoder[F[_]: Effect, A: JsonApiReader](implicit include: Include): EntityDecoder[F, Option[A]] =
    EntityDecoder.decodeBy[F, Option[A]](mediaType) { msg =>
      msg.contentLength match {
        case Some(0l) | None => DecodeResult.success(None)
        case _               => EntityDecoder[F, A].map(x => Option(x)).decode(msg, false)
      }
    }

  implicit def jsonEncoder[F[_], A <: JsValue](implicit F: Applicative[F]): EntityEncoder[F, A] =
    EntityEncoder
      .stringEncoder(F, Charset.`UTF-8`)
      .contramap[A](json => json.compactPrint)
      .withContentType(`Content-Type`(mediaType))

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
