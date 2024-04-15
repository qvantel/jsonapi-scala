/*
Copyright (c) 2017, Qvantel
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
 * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
 * Neither the name of the Qvantel nor the
      names of its contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL Qvantel BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package com.qvantel.jsonapi

import pekko.JsonApiSupport._
import _root_.org.apache.pekko.http.scaladsl.testkit.Specs2RouteTest
import _root_.spray.json.DefaultJsonProtocol._
import _root_.spray.json._
import io.lemonlabs.uri.typesafe.dsl._
import org.apache.pekko.actor.ActorSystem
import org.specs2.mutable._

final class JsonApiSortingPekkoSpec extends Specification with Specs2RouteTest {
  def actorRefFactory: ActorSystem = system

  implicit val apiRoot: com.qvantel.jsonapi.ApiRoot = ApiRoot(Some("/api"))

  @jsonApiResource final case class Res(id: String, rel: ToMany[Res])

  val one = Res(
    "1",
    ToMany.loaded(Seq(Res("3", ToMany.reference("/api/res/3/rel")), Res("2", ToMany.reference("/api/res/2/rel")))))
  val many = Seq(
    Res("1", ToMany.reference("/api/res/1/rel")),
    Res("3", ToMany.reference("/api/res/3/rel")),
    Res("2", ToMany.reference("/api/res/2/rel"))
  )

  val manualOne =
    """
      |{
      |  "data": {
      |    "relationships": {
      |      "rel": {
      |        "data": [{
      |          "type": "res",
      |          "id": "3"
      |        }, {
      |          "type": "res",
      |          "id": "2"
      |        }],
      |        "links": {
      |          "related": "/api/res/1/rel"
      |        }
      |      }
      |    },
      |    "links": {
      |      "self": "/api/res/1"
      |    },
      |    "id": "1",
      |    "type": "res"
      |  },
      |  "included": [{
      |    "relationships": {
      |      "rel": {
      |        "links": {
      |          "related": "/api/res/3/rel"
      |        }
      |      }
      |    },
      |    "links": {
      |      "self": "/api/res/3"
      |    },
      |    "id": "3",
      |    "type": "res"
      |  }, {
      |    "relationships": {
      |      "rel": {
      |        "links": {
      |          "related": "/api/res/2/rel"
      |        }
      |      }
      |    },
      |    "links": {
      |      "self": "/api/res/2"
      |    },
      |    "id": "2",
      |    "type": "res"
      |  }]
      |}
    """.stripMargin.parseJson.asJsObject

  val manualMany =
    """
      |{
      |  "data": [{
      |    "relationships": {
      |      "rel": {
      |        "links": {
      |          "related": "/api/res/1/rel"
      |        }
      |      }
      |    },
      |    "links": {
      |      "self": "/api/res/1"
      |    },
      |    "id": "1",
      |    "type": "res"
      |  }, {
      |    "relationships": {
      |      "rel": {
      |        "links": {
      |          "related": "/api/res/3/rel"
      |        }
      |      }
      |    },
      |    "links": {
      |      "self": "/api/res/3"
      |    },
      |    "id": "3",
      |    "type": "res"
      |  }, {
      |    "relationships": {
      |      "rel": {
      |        "links": {
      |          "related": "/api/res/2/rel"
      |        }
      |      }
      |    },
      |    "links": {
      |      "self": "/api/res/2"
      |    },
      |    "id": "2",
      |    "type": "res"
      |  }]
      |}
    """.stripMargin.parseJson.asJsObject

  "Unsorted" >> {
    val rawOne  = marshal(one).getData().utf8String.parseJson.asJsObject
    val rawMany = marshal(many).getData().utf8String.parseJson.asJsObject

    rawOne must_== manualOne
    rawMany must_== manualMany

    one must_== readOne[Res](rawOne, Set("rel"))
    many must_== readCollection[Res](rawMany, Set("rel")).toList
  }

  "AscendingId" >> {
    implicit val sorting: JsonApiSorting = JsonApiSorting.AscendingId

    val rawOne  = marshal(one).getData().utf8String.parseJson.asJsObject
    val rawMany = marshal(many).getData().utf8String.parseJson.asJsObject

    val sortedOne =
      """
        |{
        |  "data": {
        |    "relationships": {
        |      "rel": {
        |        "data": [{
        |          "type": "res",
        |          "id": "3"
        |        }, {
        |          "type": "res",
        |          "id": "2"
        |        }],
        |        "links": {
        |          "related": "/api/res/1/rel"
        |        }
        |      }
        |    },
        |    "links": {
        |      "self": "/api/res/1"
        |    },
        |    "id": "1",
        |    "type": "res"
        |  },
        |  "included": [{
        |    "relationships": {
        |      "rel": {
        |        "links": {
        |          "related": "/api/res/2/rel"
        |        }
        |      }
        |    },
        |    "links": {
        |      "self": "/api/res/2"
        |    },
        |    "id": "2",
        |    "type": "res"
        |  }, {
        |    "relationships": {
        |      "rel": {
        |        "links": {
        |          "related": "/api/res/3/rel"
        |        }
        |      }
        |    },
        |    "links": {
        |      "self": "/api/res/3"
        |    },
        |    "id": "3",
        |    "type": "res"
        |  }]
        |}
      """.stripMargin.parseJson.asJsObject

    val sortedMany =
      """
        |{
        |  "data": [{
        |    "relationships": {
        |      "rel": {
        |        "links": {
        |          "related": "/api/res/1/rel"
        |        }
        |      }
        |    },
        |    "links": {
        |      "self": "/api/res/1"
        |    },
        |    "id": "1",
        |    "type": "res"
        |  }, {
        |    "relationships": {
        |      "rel": {
        |        "links": {
        |          "related": "/api/res/2/rel"
        |        }
        |      }
        |    },
        |    "links": {
        |      "self": "/api/res/2"
        |    },
        |    "id": "2",
        |    "type": "res"
        |  }, {
        |    "relationships": {
        |      "rel": {
        |        "links": {
        |          "related": "/api/res/3/rel"
        |        }
        |      }
        |    },
        |    "links": {
        |      "self": "/api/res/3"
        |    },
        |    "id": "3",
        |    "type": "res"
        |  }]
        |}
      """.stripMargin.parseJson.asJsObject

    rawOne must_== sortedOne
    rawMany must_== sortedMany

    // sanity check
    one must_== readOne[Res](rawOne, Set("rel"))
    many.sortBy(_.id) must_== readCollection[Res](rawMany, Set("rel")).toList
  }

  "DescendingId" >> {
    implicit val sorting: JsonApiSorting = JsonApiSorting.DescendingId
    val rawOne                           = marshal(one).getData().utf8String.parseJson.asJsObject
    val rawMany                          = marshal(many).getData().utf8String.parseJson.asJsObject

    val sortedOne =
      """
        |{
        |  "data": {
        |    "relationships": {
        |      "rel": {
        |        "data": [{
        |          "type": "res",
        |          "id": "3"
        |        }, {
        |          "type": "res",
        |          "id": "2"
        |        }],
        |        "links": {
        |          "related": "/api/res/1/rel"
        |        }
        |      }
        |    },
        |    "links": {
        |      "self": "/api/res/1"
        |    },
        |    "id": "1",
        |    "type": "res"
        |  },
        |  "included": [{
        |    "relationships": {
        |      "rel": {
        |        "links": {
        |          "related": "/api/res/3/rel"
        |        }
        |      }
        |    },
        |    "links": {
        |      "self": "/api/res/3"
        |    },
        |    "id": "3",
        |    "type": "res"
        |  }, {
        |    "relationships": {
        |      "rel": {
        |        "links": {
        |          "related": "/api/res/2/rel"
        |        }
        |      }
        |    },
        |    "links": {
        |      "self": "/api/res/2"
        |    },
        |    "id": "2",
        |    "type": "res"
        |  }]
        |}
      """.stripMargin.parseJson.asJsObject

    val sortedMany =
      """
        |{
        |  "data": [{
        |    "relationships": {
        |      "rel": {
        |        "links": {
        |          "related": "/api/res/3/rel"
        |        }
        |      }
        |    },
        |    "links": {
        |      "self": "/api/res/3"
        |    },
        |    "id": "3",
        |    "type": "res"
        |  }, {
        |    "relationships": {
        |      "rel": {
        |        "links": {
        |          "related": "/api/res/2/rel"
        |        }
        |      }
        |    },
        |    "links": {
        |      "self": "/api/res/2"
        |    },
        |    "id": "2",
        |    "type": "res"
        |  }, {
        |    "relationships": {
        |      "rel": {
        |        "links": {
        |          "related": "/api/res/1/rel"
        |        }
        |      }
        |    },
        |    "links": {
        |      "self": "/api/res/1"
        |    },
        |    "id": "1",
        |    "type": "res"
        |  }]
        |}
      """.stripMargin.parseJson.asJsObject

    rawOne must_== sortedOne
    rawMany must_== sortedMany

    // sanity check
    one must_== readOne[Res](rawOne, Set("rel"))
    many.sortBy(_.id).reverse must_== readCollection[Res](rawMany, Set("rel")).toList
  }

  "ByOrdering" >> {
    implicit val sorting: JsonApiSorting =
      JsonApiSorting.ByOrdering(Ordering.by[JsObject, Option[String]](_.fields.get("id").map(_.convertTo[String])))
    val rawOne  = marshal(one).getData().utf8String.parseJson.asJsObject
    val rawMany = marshal(many).getData().utf8String.parseJson.asJsObject

    val sortedOne =
      """
        |{
        |  "data": {
        |    "relationships": {
        |      "rel": {
        |        "data": [{
        |          "type": "res",
        |          "id": "3"
        |        }, {
        |          "type": "res",
        |          "id": "2"
        |        }],
        |        "links": {
        |          "related": "/api/res/1/rel"
        |        }
        |      }
        |    },
        |    "links": {
        |      "self": "/api/res/1"
        |    },
        |    "id": "1",
        |    "type": "res"
        |  },
        |  "included": [{
        |    "relationships": {
        |      "rel": {
        |        "links": {
        |          "related": "/api/res/2/rel"
        |        }
        |      }
        |    },
        |    "links": {
        |      "self": "/api/res/2"
        |    },
        |    "id": "2",
        |    "type": "res"
        |  }, {
        |    "relationships": {
        |      "rel": {
        |        "links": {
        |          "related": "/api/res/3/rel"
        |        }
        |      }
        |    },
        |    "links": {
        |      "self": "/api/res/3"
        |    },
        |    "id": "3",
        |    "type": "res"
        |  }]
        |}
      """.stripMargin.parseJson.asJsObject

    val sortedMany =
      """
        |{
        |  "data": [{
        |    "relationships": {
        |      "rel": {
        |        "links": {
        |          "related": "/api/res/1/rel"
        |        }
        |      }
        |    },
        |    "links": {
        |      "self": "/api/res/1"
        |    },
        |    "id": "1",
        |    "type": "res"
        |  }, {
        |    "relationships": {
        |      "rel": {
        |        "links": {
        |          "related": "/api/res/2/rel"
        |        }
        |      }
        |    },
        |    "links": {
        |      "self": "/api/res/2"
        |    },
        |    "id": "2",
        |    "type": "res"
        |  }, {
        |    "relationships": {
        |      "rel": {
        |        "links": {
        |          "related": "/api/res/3/rel"
        |        }
        |      }
        |    },
        |    "links": {
        |      "self": "/api/res/3"
        |    },
        |    "id": "3",
        |    "type": "res"
        |  }]
        |}
      """.stripMargin.parseJson.asJsObject

    rawOne must_== sortedOne
    rawMany must_== sortedMany

    // sanity check
    one must_== readOne[Res](rawOne, Set("rel"))
    many.sortBy(_.id) must_== readCollection[Res](rawMany, Set("rel")).toList
  }

  "RelatedResponse" >> {
    implicit val sorting: JsonApiSorting = JsonApiSorting.AscendingId

    val rawOne  = marshal(RelatedResponse(one)).getData().utf8String.parseJson.asJsObject
    val rawMany = marshal(RelatedResponse(many)).getData().utf8String.parseJson.asJsObject
    val sortedOne =
      """
        |{
        |  "data": {
        |    "relationships": {
        |      "rel": {
        |        "data": [{
        |          "type": "res",
        |          "id": "3"
        |        }, {
        |          "type": "res",
        |          "id": "2"
        |        }],
        |        "links": {
        |          "related": "/api/res/1/rel"
        |        }
        |      }
        |    },
        |    "links": {
        |      "self": "/api/res/1"
        |    },
        |    "id": "1",
        |    "type": "res"
        |  },
        |  "included": [{
        |    "relationships": {
        |      "rel": {
        |        "links": {
        |          "related": "/api/res/2/rel"
        |        }
        |      }
        |    },
        |    "links": {
        |      "self": "/api/res/2"
        |    },
        |    "id": "2",
        |    "type": "res"
        |  }, {
        |    "relationships": {
        |      "rel": {
        |        "links": {
        |          "related": "/api/res/3/rel"
        |        }
        |      }
        |    },
        |    "links": {
        |      "self": "/api/res/3"
        |    },
        |    "id": "3",
        |    "type": "res"
        |  }]
        |}
      """.stripMargin.parseJson.asJsObject

    val sortedMany =
      """
        |{
        |  "data": [{
        |    "relationships": {
        |      "rel": {
        |        "links": {
        |          "related": "/api/res/1/rel"
        |        }
        |      }
        |    },
        |    "links": {
        |      "self": "/api/res/1"
        |    },
        |    "id": "1",
        |    "type": "res"
        |  }, {
        |    "relationships": {
        |      "rel": {
        |        "links": {
        |          "related": "/api/res/2/rel"
        |        }
        |      }
        |    },
        |    "links": {
        |      "self": "/api/res/2"
        |    },
        |    "id": "2",
        |    "type": "res"
        |  }, {
        |    "relationships": {
        |      "rel": {
        |        "links": {
        |          "related": "/api/res/3/rel"
        |        }
        |      }
        |    },
        |    "links": {
        |      "self": "/api/res/3"
        |    },
        |    "id": "3",
        |    "type": "res"
        |  }]
        |}
      """.stripMargin.parseJson.asJsObject

    rawOne must_== sortedOne
    rawMany must_== sortedMany

    // sanity check
    one must_== readOne[Res](rawOne, Set("rel"))
    many.sortBy(_.id) must_== readCollection[Res](rawMany, Set("rel")).toList
  }
}
