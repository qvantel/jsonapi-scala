# http://jsonapi.org/ implementation in scala

[![Build Status](https://travis-ci.org/qvantel/jsonapi-scala.svg?branch=master)](https://travis-ci.org/qvantel/jsonapi-scala)
[![Coverage Status](https://coveralls.io/repos/github/qvantel/jsonapi-scala/badge.svg?branch=master)](https://coveralls.io/github/qvantel/jsonapi-scala?branch=master)

## Features
* Automatic generation of jsonapi json writers with relationship handling for case classes


### Example
```
@jsonApiResource final case class Company(id: String, name: String, employees: ToMany[Employee])
@jsonApiResource final case class Employee(id: String, name: String)

val acme = Company("1", "acme", ToMany.loaded(Employee("1", "number one 1")))

val json = JsonApiSupport.rawOne(acme)
val parsed = JsonApiSupport.readOne(json)

json == parsed // true

```

### Known issues
  * loop handing on reader side (relationship path has to be given manually)
