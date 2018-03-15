package com.qvantel.jsonapi.client.http4s

import scala.language.experimental.macros

import cats.instances.list._
import cats.instances.option._
import cats.syntax.traverse._
import cats.data.OptionT
import cats.effect.IO
import org.specs2.matcher.MatcherMacros
import org.specs2.mutable.Specification
import com.netaporter.uri.dsl._
import org.http4s.{HttpService, QueryParamDecoder, QueryParameterValue}
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

  object IncludeQueryParamMatcher extends QueryParamDecoderMatcher[Set[String]]("include") {
    val name = "include"
    override def unapplySeq(params: Map[String, Seq[String]]): Option[Seq[Set[String]]] =
      params
        .get(name)
        .flatMap(values =>
          values.toList.traverse(s =>
            QueryParamDecoder[Set[String]].decode(QueryParameterValue(s)).toOption.orElse(Some(Set.empty[String]))))

    override def unapply(params: Map[String, Seq[String]]): Option[Set[String]] =
      params
        .get(name)
        .flatMap(_.headOption)
        .flatMap(s => QueryParamDecoder[Set[String]].decode(QueryParameterValue(s)).toOption)
        .orElse(Some(Set.empty[String]))
  }

  object FilterQueryParamMatcher extends QueryParamDecoderMatcher[String]("filter")

  implicit val stringSetQueryParamDecoder: QueryParamDecoder[Set[String]] = {
    QueryParamDecoder.stringQueryParamDecoder.map { x =>
      x.split(",").toSet
    }
  }

  val service: HttpService[IO] = HttpService[IO] {
    case GET -> Root / "billing-accounts" :? FilterQueryParamMatcher(filter) =>
      if (filter == """(OR (EQ id "ba1") (EQ id "foo & bar / baz") )""") {
        Ok(List(billingAccounts("ba1"), billingAccounts("foo & bar / baz")))
      } else {
        Ok(List.empty[CustomerAccount])
      }
    case GET -> Root / "billing-accounts" / id :? IncludeQueryParamMatcher(include) =>
      billingAccounts.get(id) match {
        case Some(ba) =>
          val loadedBa = if (include.contains("customer-account")) {
            ba.copy(customerAccount = ToOne.loaded(customerAccounts(ba.customerAccount.id)))
          } else {
            ba
          }

          Ok(loadedBa)
        case None =>
          NotFound()
      }
    case GET -> Root / "customer-accounts" / id =>
      customerAccounts.get(id) match {
        case Some(x) => Ok(x)
        case None    => NotFound()
      }
    case GET -> Root / "billing-accounts" =>
      Ok(billingAccounts.values.toList)

    case req @ POST -> Root / "billing-accounts" / id :? IncludeQueryParamMatcher(include) =>
      implicit val _include: Include = Include(include)
      for {
        ba   <- req.as[BillingAccount]
        resp <- Ok(customerAccounts("ca1").copy(id = ba.id))
      } yield resp

    case req @ PUT -> Root / "billing-accounts" / id :? IncludeQueryParamMatcher(include) =>
      implicit val _include: Include = Include(include)
      for {
        ba   <- req.as[BillingAccount]
        resp <- Ok(customerAccounts("ca1").copy(id = ba.id))
      } yield resp

    case req @ PATCH -> Root / "billing-accounts" / id :? IncludeQueryParamMatcher(include) =>
      implicit val _include: Include = Include(include)
      for {
        ba   <- req.as[BillingAccount]
        resp <- Ok(customerAccounts("ca1").copy(id = ba.id))
      } yield resp

    case DELETE -> Root / "billing-accounts" / id =>
      Ok(customerAccounts("ca1").copy(id = "delete"))
  }

  implicit val endpoint: ApiEndpoint = ApiEndpoint.Static("http://localhost:8080", Map())
  implicit val client: Client[IO]    = Client.fromHttpService(service)

  val jac = JsonApiClient.instance

  "one" >> {
    val req = jac.one[BillingAccount]("ba1").unsafeRunSync()

    req must beSome(matchA[BillingAccount].id("ba1"))
  }

  "one should return None when backend returns 404" >> {
    val req = jac.one[BillingAccount]("foobar").unsafeRunSync()

    req must beNone
  }

  "make sure weird characters in id work correctly" >> {
    val req = jac.one[BillingAccount]("""foo & bar / baz""").unsafeRunSync()

    req must beSome(matchA[BillingAccount].id("foo & bar / baz"))
  }

  "one with include" >> {
    val req = OptionT(jac.one[BillingAccount]("ba1", Set("customer-account")))
    val res = req.subflatMap(_.customerAccount.get).value.unsafeRunSync()

    res must beSome(matchA[CustomerAccount].id("ca1"))
  }

  "many" >> {
    val req = jac.many[BillingAccount](Set("ba1", "ba2")).unsafeRunSync()

    req must contain(
      matchA[BillingAccount].id("ba1"),
      matchA[BillingAccount].id("ba2")
    )
  }

  "many with include" >> {
    val req    = jac.many[BillingAccount](Set("ba1", "ba2"), Set("customer-account"))
    val mapped = req.flatMap(_.traverse(_.customerAccount.load))

    val res = mapped.unsafeRunSync()

    res must contain(
      matchA[CustomerAccount].id("ca1")
    )
  }

  "load ToOne" >> {
    val ba = OptionT(jac.one[BillingAccount]("ba1"))

    val ca = ba.semiflatMap(_.customerAccount.load)

    val res = ca.value.unsafeRunSync()

    res must beSome(matchA[CustomerAccount].id("ca1"))
  }

  "load ToMany" >> {
    val ca = OptionT(jac.one[CustomerAccount]("ca1"))

    val ba = ca.semiflatMap(_.billingAccounts.load)

    val res = ba.value.unsafeRunSync()

    res must beSome(contain(matchA[BillingAccount].id("ba1")))
  }

  "filter" >> {
    val req = jac.filter[BillingAccount]("""(OR (EQ id "ba1") (EQ id "foo & bar / baz") )""").unsafeRunSync()

    req must contain(
      matchA[BillingAccount].id("ba1"),
      matchA[BillingAccount].id("foo & bar / baz")
    )
  }

  "pathMany" >> {
    val req = jac.pathMany[BillingAccount]("billing-accounts").unsafeRunSync()

    req must contain(
      matchA[BillingAccount].id("ba1"),
      matchA[BillingAccount].id("ba2"),
      matchA[BillingAccount].id("foo & bar / baz")
    )
  }

  "pathMany 404" >> {
    jac.pathMany[BillingAccount]("billing-accountss").unsafeRunSync() must throwA
  }

  "post" >> {
    val ca = jac.post[BillingAccount, CustomerAccount](billingAccounts("ba1").copy(id = "post")).unsafeRunSync()

    ca.id must_== "post"
  }

  "put" >> {
    val ca = jac.put[BillingAccount, CustomerAccount](billingAccounts("ba1").copy(id = "put")).unsafeRunSync()

    ca.id must_== "put"
  }

  "patch" >> {
    val ca =
      jac.patch[BillingAccount, CustomerAccount](billingAccounts("ba1").copy(id = "patch")).unsafeRunSync()

    ca.id must_== "patch"
  }

  "delete" >> {
    val ca = jac.delete[BillingAccount, CustomerAccount](billingAccounts("ba1")).unsafeRunSync()

    ca.id must_== "delete"
  }
}
