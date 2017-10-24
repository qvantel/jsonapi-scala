# http://jsonapi.org/ implementation in scala

[![Build Status](https://travis-ci.org/qvantel/jsonapi-scala.svg?branch=master)](https://travis-ci.org/qvantel/jsonapi-scala)
[![Coverage Status](https://coveralls.io/repos/github/qvantel/jsonapi-scala/badge.svg?branch=master)](https://coveralls.io/github/qvantel/jsonapi-scala?branch=master)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.qvantel/jsonapi-scala-core_2.11/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.qvantel/jsonapi-scala-core_2.11)

## Features
* Automatic generation of jsonapi json writers with relationship handling for case classes


### Example
```
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
