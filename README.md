# http://jsonapi.org/ implementation in scala

[![Build Status](https://travis-ci.org/qvantel/jsonapi-scala.svg?branch=master)](https://travis-ci.org/qvantel/jsonapi-scala)
[![codecov](https://codecov.io/gh/qvantel/jsonapi-scala/branch/master/graph/badge.svg)](https://codecov.io/gh/qvantel/jsonapi-scala)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.qvantel/jsonapi-scala-core_2.11/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.qvantel/jsonapi-scala-core_2.11)

## Features
* Automatic generation of jsonapi json writers with relationship handling for case classes

## Requirements
* Tested to work on Scala 2.11.12 or 2.12.10
* Macro paradise plugin. Add `addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)` into your build.sbt somewhere

### Example
```scala
import _root_.spray.json.DefaultJsonProtocol._
import com.qvantel.jsonapi._

implicit val apiRoot: ApiRoot = ApiRoot(None)

@jsonApiResource final case class Employee(id: String, name: String)
@jsonApiResource final case class Company(id: String, name: String, employees: ToMany[Employee])

val acme = Company("1", "acme", ToMany.loaded(Seq(Employee("1", "number one 1"))))

val json = rawOne(acme)
val parsed = readOne[Company](json, Set("employees"))

acme == parsed // true
```

or see https://github.com/Doikor/jsonapi-scala-example

### Known issues
  * loop handing on reader side (relationship path has to be given manually)


## JsonApiClient

There is a very generic JsonApiClient interface for implementing a simple client 
interface for handling the http query writing side of this

The subproject "akka-client" has an implementation of this using akka-http

The subproject "http4s-client" has an implementation of this using http4s

### Usage

```scala
import cats.data.OptionT
import com.qvantel.jsonapi.JsonApiClient

val jac = JsonApiClient.instance // won't work if you don't have an actual implementations stuff in scope. See setup.

val one: IO[Option[BillingAccount]] = jac.one[BillingAccount]("ba1") 
val many: IO[List[BillingAccount]] = jac.many[BillingAccount](Set("ba1", "ba2"))

// can also load includes at the same time
val withIncludes = jac.one[BillingAccount]("ba1", Set("customer-account"))

// includes can also be loaded on their own with a method
val ba: OptionT[IO, BillingAccount]  = OptionT(jac.one[BillingAccount]("ba"))
val ca: OptionT[IO, CustomerAccount] = ba.semiflatMap(_.customerAccount.load)

// filtering support
val filtered = jac.filter[BillingAccount]("some nice filter string here")
```

### Setup

#### akka-http client
```scala
// needs ActorSystem and Materializer for akka-http
// the ApiEndPoint is used to as the "root" where to launch queries
import com.netaporter.uri.dsl._
import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import com.qvantel.jsonapi.ApiEndpoint
import com.qvantel.jsonapi.JsonApiClient
import com.qvantel.jsonapi.client.akka.AkkaClient._

implicit val system: ActorSystem  = ActorSystem()
implicit val materializer: ActorMaterializer = ActorMaterializer()
implicit val endpoint: ApiEndpoint = ApiEndpoint.Static("http://localhost:8080/api")

val jac = JsonApiClient.instance
```

#### http4s client
Setup for http4s client
```scala
import com.netaporter.uri.dsl._
import org.http4s.client.Client
import org.http4s.client.blaze.Http1Client
import cats.effect.IO
import com.qvantel.jsonapi.ApiEndpoint
import com.qvantel.jsonapi.JsonApiClient

import com.qvantel.jsonapi.client.http4s.Http4sClient._
import com.qvantel.jsonapi.client.http4s.JsonApiInstances._


implicit val endpoint: ApiEndpoint = ApiEndpoint.Static("http://localhost:8080/api")

implicit val client: Client[IO] = Http1Client[IO]().unsafeRunSync()

val jac = JsonApiClient.instance
```