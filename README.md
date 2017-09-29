# http://jsonapi.org/ implementation in scala

[![Build Status](https://travis-ci.org/qvantel/jsonapi-scala.svg?branch=master)](https://travis-ci.org/qvantel/jsonapi-scala)
[![Coverage Status](https://coveralls.io/repos/github/qvantel/jsonapi-scala/badge.svg?branch=master)](https://coveralls.io/github/qvantel/jsonapi-scala?branch=master)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.qvantel/jsonapi-scala-core_2.11/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.qvantel/jsonapi-scala-core_2.11)

## Features
* Automatic generation of jsonapi json writers with relationship handling for case classes


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

### Known issues
  * loop handing on reader side (relationship path has to be given manually)


## JsonApiClient

There is a very generic JsonApiClient interface for implementing a simple client 
interface for handling the http query writing side of this

The subproject "akka-client" has an implementation of this using akka-http

Simple example:
````scala
// needs ActorSystem and Materializer for akka-http
// the ApiEndPoint is used to as the "root" where to launch queries
import com.qvantel.jsonapi.client.akka.AkkaClient._

implicit val system: ActorSystem  = ActorSystem()
implicit val materializer: ActorMaterializer = ActorMaterializer()
implicit val endpoint: ApiEndpoint = ApiEndpoint.Static("http://localhost:8080/api")

val one: Task[Option[BillingAccount] = JsonApiClient[BillingAccount].one("ba1") 
val many: Task[List[BillingAccount] = JsonApiClient[BillingAccount].many(Set("ba1", "ba2"))

// can also load includes at the same time
val withIncludes = JsonApiClient[BillingAccount].one("ba1", Set("customer-account"))

// includes can also be loaded on their own with a method
val ba: Task[Option[BillingAccount]] = OptionT(JsonApiClient[BillingAccount].one("ba"))
val ca: Task[Option[CustomerAccount]] = ba.semiflatMap(_.customerAccount.load)

// filtering support
val filtered = JsonApiClient[BillingAccount].filter("some nice filter string here")
````
