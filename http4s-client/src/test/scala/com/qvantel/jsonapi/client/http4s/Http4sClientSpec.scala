package com.qvantel.jsonapi.client.http4s

import scala.language.experimental.macros

import cats.Applicative
import cats.instances.list._
import cats.data.OptionT
import cats.effect.IO
import org.specs2.matcher.MatcherMacros
import org.specs2.mutable.Specification
import com.netaporter.uri.dsl._
import org.http4s.{HttpService, QueryParamDecoder}
import org.http4s.client.Client
import org.http4s.dsl.io._

import com.qvantel.jsonapi._
import com.qvantel.jsonapi.client.http4s.Http4sClient._
import com.qvantel.jsonapi.client.http4s.JsonApiInstances._

class Http4sClientSpec extends Specification with MatcherMacros {

  val billingAccounts = Map(
    "ba1"             -> BillingAccount("ba1", "ba1", "foobar", None, ToOne.reference("ca1")),
    "ba2"             -> BillingAccount("ba2", "ba1", "foobar", None, ToOne.reference("ca1")),
    "foo & bar / baz" -> BillingAccount("foo & bar / baz", "ba1", "foobar", None, ToOne.reference("ca1"))
  )

  val customerAccounts = Map(
    "ca1" -> CustomerAccount("ca1", ToMany.reference(Set("ba1", "ba2")))
  )

  implicit val stringSetQueryParamDecoder: QueryParamDecoder[Set[String]] =
    QueryParamDecoder.stringQueryParamDecoder.map { x =>
      x.split(",").toSet
    }

  object IncludeQueryParamMatcher extends QueryParamDecoderMatcher[Set[String]]("include")

  object FilterQueryParamMatcher extends QueryParamDecoderMatcher[String]("filter")

  val service: HttpService[IO] = HttpService[IO] {
    case GET -> Root / "billing-accounts" :? FilterQueryParamMatcher(filter) =>
      if (filter == """(OR (EQ id "ba1") (EQ id "foo & bar / baz") )""") {
        Ok(List(billingAccounts("ba1"), billingAccounts("foo & bar / baz")))
      } else {
        Ok(List.empty[CustomerAccount])
      }
    case GET -> Root / "billing-accounts" / id :? IncludeQueryParamMatcher(include) =>
      val ba = billingAccounts(id)
      val loadedBa = if (include.contains("customer-account")) {
        ba.copy(customerAccount = ToOne.loaded(customerAccounts(ba.customerAccount.id)))
      } else {
        ba
      }

      Ok(loadedBa)
    case GET -> Root / "billing-accounts" / id =>
      billingAccounts.get(id) match {
        case Some(x) => Ok(x)
        case None    => NotFound()
      }
    case GET -> Root / "customer-accounts" / id =>
      customerAccounts.get(id) match {
        case Some(x) => Ok(x)
        case None    => NotFound()
      }
    case GET -> Root / "billing-accounts" =>
      Ok(billingAccounts.values.toList)
  }

  implicit val endpoint: ApiEndpoint = ApiEndpoint.Static("http://localhost:8080")
  implicit val client: Client[IO]    = Client.fromHttpService(service)

  "one" >> {
    val req = JsonApiClient[BillingAccount].one("ba1").unsafeRunSync()

    req must beSome(matchA[BillingAccount].id("ba1"))
  }

  "one should return None when backend returns 404" >> {
    val req = JsonApiClient[BillingAccount].one("foobar").unsafeRunSync()

    req must beNone
  }

  "make sure weird characters in id work correctly" >> {
    val req = JsonApiClient[BillingAccount].one("""foo & bar / baz""").unsafeRunSync()

    req must beSome(matchA[BillingAccount].id("foo & bar / baz"))
  }

  "one with include" >> {
    val req = OptionT(JsonApiClient[BillingAccount].one("ba1", Set("customer-account")))
    val res = req.subflatMap(_.customerAccount.get).value.unsafeRunSync()

    res must beSome(matchA[CustomerAccount].id("ca1"))
  }

  "many" >> {
    val req = JsonApiClient[BillingAccount]
      .many(Set("ba1", "ba2"))
      .unsafeRunSync()

    req must contain(
      matchA[BillingAccount].id("ba1"),
      matchA[BillingAccount].id("ba2")
    )
  }

  "many with include" >> {
    val req    = JsonApiClient[BillingAccount].many(Set("ba1", "ba2"), Set("customer-account"))
    val mapped = req.flatMap(x => Applicative[IO].traverse(x)(_.customerAccount.load))

    val res = mapped.unsafeRunSync()

    res must contain(
      matchA[CustomerAccount].id("ca1")
    )
  }

  "load ToOne" >> {
    val ba = OptionT(JsonApiClient[BillingAccount].one("ba1"))

    val ca = ba.semiflatMap(_.customerAccount.load)

    val res = ca.value.unsafeRunSync()

    res must beSome(matchA[CustomerAccount].id("ca1"))
  }

  "load ToMany" >> {
    val ca = OptionT(JsonApiClient[CustomerAccount].one("ca1"))

    val ba = ca.semiflatMap(_.billingAccounts.load)

    val res = ba.value.unsafeRunSync()

    res must beSome(contain(matchA[BillingAccount].id("ba1")))
  }

  "filter" >> {
    val req = JsonApiClient[BillingAccount]
      .filter("""(OR (EQ id "ba1") (EQ id "foo & bar / baz") )""")
      .unsafeRunSync()

    req must contain(
      matchA[BillingAccount].id("ba1"),
      matchA[BillingAccount].id("foo & bar / baz")
    )
  }

  "pathMany" >> {
    val req = JsonApiClient[BillingAccount].pathMany("billing-accounts").unsafeRunSync()

    req must contain(
      matchA[BillingAccount].id("ba1"),
      matchA[BillingAccount].id("ba2"),
      matchA[BillingAccount].id("foo & bar / baz")
    )
  }

  "pathMany 404" >> {
    JsonApiClient[BillingAccount].pathMany("billing-accountss").unsafeRunSync() must throwA
  }
}
