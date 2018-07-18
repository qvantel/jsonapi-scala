package com.qvantel.jsonapi.client.akka

import scala.language.experimental.macros

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import cats.instances.list._
import cats.syntax.traverse._
import cats.data.OptionT
import cats.instances.list._
import com.netaporter.uri.dsl._
import org.specs2.concurrent.ExecutionEnv
import org.specs2.matcher.MatcherMacros
import org.specs2.mutable.Specification
import org.specs2.specification.AfterAll

import com.qvantel.jsonapi._
import AkkaClient._
import com.netaporter.uri.dsl._

class AkkaClientSpec(implicit ee: ExecutionEnv) extends Specification with MatcherMacros with AfterAll {
  // this is an integration test.
  // to run these tests uncomment this and start a jsonapi.org compatible server in the url specified for the endpoint
  skipAll

  implicit val system: ActorSystem   = ActorSystem()
  implicit val m: ActorMaterializer  = ActorMaterializer()
  implicit val endpoint: ApiEndpoint = ApiEndpoint.Static("http://localhost:8080/api", Map())

  val jac = JsonApiClient.instance

  "one" >> {
    val req = jac.one[BillingAccount]("lindberg-ab-billingaccount1").unsafeRunSync()

    req must beSome(matchA[BillingAccount].id("lindberg-ab-billingaccount1"))
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
    val req = OptionT(jac.one[BillingAccount]("lindberg-ab-billingaccount1", Set("customer-account")))
    val res = req.subflatMap(_.customerAccount.get).value.unsafeRunSync()

    res must beSome(matchA[CustomerAccount].id("lindberg-ab-customeraccount1"))
  }

  "many" >> {
    val req = jac.many[BillingAccount](Set("lindberg-ab-billingaccount1", "qvantel-billingaccount1")).unsafeRunSync()

    req must contain(
      matchA[BillingAccount].id("lindberg-ab-billingaccount1"),
      matchA[BillingAccount].id("qvantel-billingaccount1")
    )
  }

  "many with include" >> {
    val req =
      jac.many[BillingAccount](Set("lindberg-ab-billingaccount1", "qvantel-billingaccount1"), Set("customer-account"))
    val mapped = req.flatMap(_.traverse(_.customerAccount.load))

    val res = mapped.unsafeRunSync()

    res must contain(
      matchA[CustomerAccount].id("lindberg-ab-customeraccount1"),
      matchA[CustomerAccount].id("qvantel-customeraccount1")
    )
  }

  "load ToOne" >> {
    val ba = OptionT(jac.one[BillingAccount]("lindberg-ab-billingaccount1"))

    val ca = ba.semiflatMap(_.customerAccount.load)

    val res = ca.value.unsafeRunSync()

    res must beSome(matchA[CustomerAccount].id("lindberg-ab-customeraccount1"))
  }

  "load ToMany" >> {
    val ca = OptionT(jac.one[CustomerAccount]("lindberg-ab-customeraccount1"))

    val ba = ca.semiflatMap(_.billingAccounts.load)

    val res = ba.value.unsafeRunSync()

    res must beSome(contain(matchA[BillingAccount].id("lindberg-ab-billingaccount1")))
  }

  "filter" >> {
    val req = jac
      .filter[BillingAccount]("""(OR (EQ id "lindberg-ab-billingaccount1") (EQ id "foo & bar / baz") )""")
      .unsafeRunSync()

    req must contain(
      matchA[BillingAccount].id("lindberg-ab-billingaccount1"),
      matchA[BillingAccount].id("foo & bar / baz")
    )
  }

  "pathMany" >> {
    val req = jac.pathMany[BillingAccount]("/api/billing-accounts").unsafeRunSync()

    req must contain(
      matchA[BillingAccount].id("lindberg-ab-billingaccount1"),
      matchA[BillingAccount].id("timo-billingaccount")
    )
  }

  "pathMany 404" >> {
    jac.pathMany[BillingAccount]("/api/billing-accountss").unsafeRunSync() must throwA
  }

  def afterAll = {
    m.shutdown()
    Await.ready(system.terminate(), Duration.Inf)
  }
}
