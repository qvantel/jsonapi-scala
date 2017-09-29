package com.qvantel.jsonapi.client.akka

import scala.language.experimental.macros

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import cats.data.OptionT
import org.specs2.concurrent.ExecutionEnv
import org.specs2.matcher.MatcherMacros
import org.specs2.mutable.Specification
import org.specs2.specification.AfterAll

import com.qvantel.jsonapi._
import AkkaClient._
import com.netaporter.uri.dsl._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global

class AkkaClientSpec(implicit ee: ExecutionEnv) extends Specification with MatcherMacros with AfterAll {
  // TODO: make tests run/work without external service
  // to run these tests uncomment this and start a jsonapi.org compatible server in the url specified for the endpoint
  skipAll

  implicit val system: ActorSystem   = ActorSystem()
  implicit val m: ActorMaterializer  = ActorMaterializer()
  implicit val endpoint: ApiEndpoint = ApiEndpoint.Static("http://localhost:8080/api")

  "AkkaClient" >> {
    "one" >> {
      val req = JsonApiClient[BillingAccount].one("lindberg-ab-billingaccount1").runAsync

      req must beSome(matchA[BillingAccount].id("lindberg-ab-billingaccount1")).await
    }

    "one with include" >> {
      val req = OptionT(JsonApiClient[BillingAccount].one("lindberg-ab-billingaccount1", Set("customer-account")))

      req.subflatMap(_.customerAccount.get).value.runAsync must beSome(
        matchA[CustomerAccount].id("lindberg-ab-customeraccount1")).await
    }

    "many" >> {
      val req = JsonApiClient[BillingAccount].many(Set("lindberg-ab-billingaccount1", "qvantel-billingaccount1"))

      val right = req.map(_.flatMap(_.right.toOption))

      right.runAsync must contain(
        matchA[BillingAccount].id("lindberg-ab-billingaccount1"),
        matchA[BillingAccount].id("qvantel-billingaccount1")
      ).await
    }

    "many with include" >> {
      val req = JsonApiClient[BillingAccount].many(Set("lindberg-ab-billingaccount1", "qvantel-billingaccount1"),
                                                   Set("customer-account"))

      val right = req.map(_.flatMap(_.right.toOption))

      val mapped = right.flatMap(x => Task.gatherUnordered(x.map(_.customerAccount.load)))

      mapped.runAsync must contain(
        matchA[CustomerAccount].id("lindberg-ab-customeraccount1"),
        matchA[CustomerAccount].id("qvantel-customeraccount1")
      ).await
    }

    "load ToOne" >> {
      val ba = OptionT(JsonApiClient[BillingAccount].one("lindberg-ab-billingaccount1"))

      val ca = ba.semiflatMap(_.customerAccount.load)

      ca.value.runAsync must beSome(matchA[CustomerAccount].id("lindberg-ab-customeraccount1")).await
    }

    "load ToMany" >> {
      val ca = OptionT(JsonApiClient[CustomerAccount].one("lindberg-ab-customeraccount1"))

      val ba = ca.semiflatMap(_.billingAccounts.load)

      ba.value.runAsync must beSome(contain(matchA[BillingAccount].id("lindberg-ab-billingaccount1"))).await
    }

    "filter" >> {
      val req = JsonApiClient[BillingAccount].filter("""(EQ id "lindberg-ab-billingaccount1")""").runAsync

      req must contain(matchA[BillingAccount].id("lindberg-ab-billingaccount1")).await
    }
  }

  def afterAll = {
    m.shutdown()
    Await.ready(system.terminate(), Duration.Inf)
  }
}
