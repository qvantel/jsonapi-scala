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
package com.qvantel.jsonapi.model

import org.specs2.mutable.Specification
import _root_.spray.json.DefaultJsonProtocol._
import _root_.spray.json.JsonParser

import com.qvantel.jsonapi.model.Link.Url
import com.netaporter.uri.dsl._

final class TopLevelSpec extends Specification {
  "JsonFormat" should {
    "successfully deserialize collection JSON-API example" in {
      val exampleStr =
        """{
          |  "links": {
          |    "self": "http://example.com/articles",
          |    "next": "http://example.com/articles?page[offset]=2",
          |    "last": "http://example.com/articles?page[offset]=10"
          |  },
          |  "data": [{
          |    "type": "articles",
          |    "id": "1",
          |    "attributes": {
          |      "title": "JSON API paints my bikeshed!"
          |    },
          |    "relationships": {
          |      "author": {
          |        "links": {
          |          "self": "http://example.com/articles/1/relationships/author",
          |          "related": "http://example.com/articles/1/author",
          |          "next": null
          |        },
          |        "data": { "type": "people", "id": "9" }
          |      },
          |      "comments": {
          |        "links": {
          |          "self": "http://example.com/articles/1/relationships/comments",
          |          "related": "http://example.com/articles/1/comments"
          |        },
          |        "data": [
          |          { "type": "comments", "id": "5" },
          |          { "type": "comments", "id": "12" }
          |        ]
          |      }
          |    },
          |    "links": {
          |      "self": "http://example.com/articles/1",
          |      "next": null
          |    }
          |  },
          |  {
          |    "type": "articles",
          |    "id": "2",
          |    "attributes": {
          |      "title": "JSON API paints my bikeshed! 2"
          |    },
          |    "relationships": {
          |      "author": {
          |        "links": {
          |          "self": "http://example.com/articles/1/relationships/author",
          |          "related": "http://example.com/articles/1/author"
          |        },
          |        "data": { "type": "people", "id": "9" }
          |      },
          |      "comments": {
          |        "links": {
          |          "self": "http://example.com/articles/1/relationships/comments",
          |          "related": "http://example.com/articles/1/comments"
          |        },
          |        "data": [
          |          { "type": "comments", "id": "5" },
          |          { "type": "comments", "id": "12" }
          |        ]
          |      }
          |    },
          |    "links": {
          |      "self": "http://example.com/articles/1"
          |    }
          |  }],
          |  "included": [{
          |    "type": "people",
          |    "id": "9",
          |    "attributes": {
          |      "first-name": "Dan",
          |      "last-name": "Gebhardt",
          |      "twitter": "dgeb"
          |    },
          |    "links": {
          |      "self": "http://example.com/people/9"
          |    }
          |  }, {
          |    "type": "comments",
          |    "id": "5",
          |    "attributes": {
          |      "body": "First!"
          |    },
          |    "relationships": {
          |      "author": {
          |        "data": { "type": "people", "id": "2" }
          |      }
          |    },
          |    "links": {
          |      "self": "http://example.com/comments/5"
          |    }
          |  }, {
          |    "type": "comments",
          |    "id": "12",
          |    "attributes": {
          |      "body": "I like XML better"
          |    },
          |    "relationships": {
          |      "author": {
          |        "data": { "type": "people", "id": "9" }
          |      }
          |    },
          |    "links": {
          |      "self": "http://example.com/comments/12"
          |    }
          |  }]
          |}""".stripMargin
      val exampleJson = JsonParser(exampleStr)
      val topLevel    = exampleJson.convertTo[TopLevel]

      topLevel must beAnInstanceOf[TopLevel.Collection]
      val collection = topLevel.asInstanceOf[TopLevel.Collection]

      collection.data must have size 2
      collection.included must have size 3

      val one = collection.data.values.find(_.id.contains("1")).get
      val two = collection.data.values.find(_.id.contains("2")).get

      one.`type` must be equalTo "articles"
      one.id must beSome("1")
      one.attributes.getAs[String]('title) must beSome("JSON API paints my bikeshed!")
      one.relationships must have size 2
      one.relationships must haveKey("author")
      one.relationships must haveKey("comments")

      two.`type` must be equalTo "articles"
      two.id must beSome("2")
      two.attributes.getAs[String]('title) must beSome("JSON API paints my bikeshed! 2")
      two.relationships must have size 2
      two.relationships must haveKey("author")
      two.relationships must haveKey("comments")
    }
    "successfully deserialize single JSON-API example" in {
      val exampleStr =
        """{
          |  "data": {
          |    "type": "articles",
          |    "id": "1",
          |    "attributes": {
          |      "title": "JSON API paints my bikeshed!"
          |    },
          |    "relationships": {
          |      "author": {
          |        "links": {
          |          "self": "http://example.com/articles/1/relationships/author",
          |          "related": "http://example.com/articles/1/author"
          |        }
          |      },
          |      "comments": {
          |        "links": {
          |          "self": "http://example.com/articles/1/relationships/comments",
          |          "related": "http://example.com/articles/1/comments"
          |        },
          |        "data": [
          |          { "type": "comments", "id": "5" },
          |          { "type": "comments", "id": "12" }
          |        ]
          |      }
          |    },
          |    "links": {
          |      "self": "http://example.com/articles/1"
          |    }
          |  },
          |  "included": [{
          |    "type": "comments",
          |    "id": "5",
          |    "attributes": {
          |      "body": "First!"
          |    },
          |    "relationships": {
          |      "author": {
          |        "data": { "type": "people", "id": "2" }
          |      }
          |    },
          |    "links": {
          |      "self": "http://example.com/comments/5"
          |    }
          |  }, {
          |    "type": "comments",
          |    "id": "12",
          |    "attributes": {
          |      "body": "I like XML better"
          |    },
          |    "relationships": {
          |      "author": {
          |        "data": { "type": "people", "id": "9" }
          |      }
          |    },
          |    "links": {
          |      "self": "http://example.com/comments/12"
          |    }
          |  }]
          |}""".stripMargin
      val exampleJson = JsonParser(exampleStr)
      val topLevel    = exampleJson.convertTo[TopLevel]

      topLevel must beAnInstanceOf[TopLevel.Single]

      val single = topLevel.asInstanceOf[TopLevel.Single]

      single.included must have size 2

      single.data must not beEmpty
      val data = single.data.get._2

      data.`type` must be equalTo "articles"
      data.id must beSome("1")
      data.attributes.getAs[String]('title) must beSome("JSON API paints my bikeshed!")
      data.relationships must have size 2
      data.relationships must haveKey("author")
      data.relationships must haveKey("comments")
    }
    "successfully deserialize single JSON-API example where relationships are not loaded" in {
      val exampleStr =
        """{
          |  "data": {
          |    "type": "articles",
          |    "id": "1",
          |    "attributes": {
          |      "title": "JSON API paints my bikeshed!"
          |    },
          |    "relationships": {
          |      "author": {
          |        "links": {
          |          "self": "http://example.com/articles/1/relationships/author",
          |          "related": "http://example.com/articles/1/author"
          |        },
          |        "data": { "type": "people", "id": "9" }
          |      },
          |      "comments": {
          |        "links": {
          |          "self": "http://example.com/articles/1/relationships/comments",
          |          "related": "http://example.com/articles/1/comments"
          |        },
          |        "data": [
          |          { "type": "comments", "id": "5" },
          |          { "type": "comments", "id": "12" }
          |        ]
          |      }
          |    },
          |    "links": {
          |      "self": "http://example.com/articles/1",
          |      "next": null
          |    }
          |  }
          |}""".stripMargin
      val exampleJson = JsonParser(exampleStr)
      val topLevel    = exampleJson.convertTo[TopLevel]

      topLevel must beAnInstanceOf[TopLevel.Single]

      val single = topLevel.asInstanceOf[TopLevel.Single]

      single.included must beEmpty

      single.data must not beEmpty
      val data = single.data.get._2

      data.`type` must be equalTo "articles"
      data.id must beSome("1")
      data.attributes.getAs[String]('title) must beSome("JSON API paints my bikeshed!")
      data.relationships must have size 2
      data.relationships must haveKey("author")
      data.relationships must haveKey("comments")
    }
    "successfully deserialize a basic error object" in {
      val exampleStr =
        """{
          |  "errors": [{
          |      "status": "422",
          |      "source": { "pointer": "/data/attributes/first-name" },
          |      "title":  "Invalid Attribute",
          |      "detail": "First name must contain at least three characters."
          |  }]
          |}""".stripMargin
      val exampleJson = JsonParser(exampleStr)
      val topLevel    = exampleJson.convertTo[TopLevel]

      topLevel must beAnInstanceOf[TopLevel.Errors]

      val errors = topLevel.asInstanceOf[TopLevel.Errors]

      errors.errors must have size 1

      val error = errors.errors.head

      error.status must beSome("422")
      error.source must not beEmpty

      error.title must beSome("Invalid Attribute")
      error.detail must beSome("First name must contain at least three characters.")
    }
    "successfully deserialize an empty to-one relationship of a to-one related link" in {
      val exampleStr =
        """{
          |  "data": null,
          |  "links": {
          |      "self": "http://example.com/articles/1"
          |  }
          |}""".stripMargin
      val exampleJson = JsonParser(exampleStr)
      val topLevel    = exampleJson.convertTo[TopLevel]

      topLevel must beAnInstanceOf[TopLevel.Single]

      val single = topLevel.asInstanceOf[TopLevel.Single]
      single.included must beEmpty
      single.data must beEmpty
      single.links must haveKey("self")
    }

    "successfully compare TopLevel documents with different order of data and includes" in {
      val exampleStr1 =
        """{
          |  "links": {
          |    "self": "http://example.com/articles",
          |    "next": "http://example.com/articles?page[offset]=2",
          |    "last": "http://example.com/articles?page[offset]=10"
          |  },
          |  "data": [
          |  {
          |    "type": "articles",
          |    "id": "1",
          |    "attributes": {
          |      "title": "JSON API paints my bikeshed!"
          |    },
          |    "relationships": {
          |      "author": {
          |        "links": {
          |          "self": "http://example.com/articles/1/relationships/author",
          |          "related": "http://example.com/articles/1/author",
          |          "next": null
          |        },
          |        "data": { "type": "people", "id": "9" }
          |      },
          |      "comments": {
          |        "links": {
          |          "self": "http://example.com/articles/1/relationships/comments",
          |          "related": "http://example.com/articles/1/comments"
          |        },
          |        "data": [
          |          { "type": "comments", "id": "12" },
          |          { "type": "comments", "id": "5" }
          |        ]
          |      }
          |    },
          |    "links": {
          |      "self": "http://example.com/articles/1",
          |      "next": null
          |    }
          |  },
          |  {
          |    "type": "articles",
          |    "id": "2",
          |    "attributes": {
          |      "title": "JSON API paints my bikeshed! 2"
          |    },
          |    "relationships": {
          |      "author": {
          |        "links": {
          |          "self": "http://example.com/articles/1/relationships/author",
          |          "related": "http://example.com/articles/1/author"
          |        },
          |        "data": { "type": "people", "id": "9" }
          |      },
          |      "comments": {
          |        "links": {
          |          "self": "http://example.com/articles/1/relationships/comments",
          |          "related": "http://example.com/articles/1/comments"
          |        },
          |        "data": [
          |          { "type": "comments", "id": "12" },
          |          { "type": "comments", "id": "5" }
          |        ]
          |      }
          |    },
          |    "links": {
          |      "self": "http://example.com/articles/1",
          |      "next": null
          |    }
          |  },
          |  {
          |    "type": "articles",
          |    "id": "2",
          |    "attributes": {
          |      "title": "JSON API paints my bikeshed! 2"
          |    },
          |    "relationships": {
          |      "author": {
          |        "links": {
          |          "self": "http://example.com/articles/1/relationships/author",
          |          "related": "http://example.com/articles/1/author"
          |        },
          |        "data": { "type": "people", "id": "9" }
          |      },
          |      "comments": {
          |        "links": {
          |          "self": "http://example.com/articles/1/relationships/comments",
          |          "related": "http://example.com/articles/1/comments"
          |        },
          |        "data": [
          |          { "type": "comments", "id": "5" },
          |          { "type": "comments", "id": "12" }
          |        ]
          |      }
          |    },
          |    "links": {
          |      "self": "http://example.com/articles/1"
          |    }
          |  }],
          |  "included": [{
          |    "type": "people",
          |    "id": "9",
          |    "attributes": {
          |      "first-name": "Dan",
          |      "last-name": "Gebhardt",
          |      "twitter": "dgeb"
          |    },
          |    "links": {
          |      "self": "http://example.com/people/9"
          |    }
          |  }, {
          |    "type": "comments",
          |    "id": "5",
          |    "attributes": {
          |      "body": "First!"
          |    },
          |    "relationships": {
          |      "author": {
          |        "data": { "type": "people", "id": "2" }
          |      }
          |    },
          |    "links": {
          |      "self": "http://example.com/comments/5"
          |    }
          |  }, {
          |    "type": "comments",
          |    "id": "12",
          |    "attributes": {
          |      "body": "I like XML better"
          |    },
          |    "relationships": {
          |      "author": {
          |        "data": { "type": "people", "id": "9" }
          |      }
          |    },
          |    "links": {
          |      "self": "http://example.com/comments/12"
          |    }
          |  }]
          |}""".stripMargin
      val exampleStr2 =
        """{
          |  "links": {
          |    "self": "http://example.com/articles",
          |    "next": "http://example.com/articles?page[offset]=2",
          |    "last": "http://example.com/articles?page[offset]=10"
          |  },
          |  "data": [{
          |    "type": "articles",
          |    "id": "1",
          |    "attributes": {
          |      "title": "JSON API paints my bikeshed!"
          |    },
          |    "relationships": {
          |      "author": {
          |        "links": {
          |          "self": "http://example.com/articles/1/relationships/author",
          |          "related": "http://example.com/articles/1/author",
          |          "next": null
          |        },
          |        "data": { "type": "people", "id": "9" }
          |      },
          |      "comments": {
          |        "links": {
          |          "self": "http://example.com/articles/1/relationships/comments",
          |          "related": "http://example.com/articles/1/comments"
          |        },
          |        "data": [
          |          { "type": "comments", "id": "5" },
          |          { "type": "comments", "id": "12" }
          |        ]
          |      }
          |    },
          |    "links": {
          |      "self": "http://example.com/articles/1",
          |      "next": null
          |    }
          |  },
          |  {
          |    "type": "articles",
          |    "id": "2",
          |    "attributes": {
          |      "title": "JSON API paints my bikeshed! 2"
          |    },
          |    "relationships": {
          |      "author": {
          |        "links": {
          |          "self": "http://example.com/articles/1/relationships/author",
          |          "related": "http://example.com/articles/1/author"
          |        },
          |        "data": { "type": "people", "id": "9" }
          |      },
          |      "comments": {
          |        "links": {
          |          "self": "http://example.com/articles/1/relationships/comments",
          |          "related": "http://example.com/articles/1/comments"
          |        },
          |        "data": [
          |          { "type": "comments", "id": "5" },
          |          { "type": "comments", "id": "12" }
          |        ]
          |      }
          |    },
          |    "links": {
          |      "self": "http://example.com/articles/1"
          |    }
          |  }],
          |  "included": [{
          |    "type": "people",
          |    "id": "9",
          |    "attributes": {
          |      "first-name": "Dan",
          |      "last-name": "Gebhardt",
          |      "twitter": "dgeb"
          |    },
          |    "links": {
          |      "self": "http://example.com/people/9"
          |    }
          |  }, {
          |    "type": "comments",
          |    "id": "12",
          |    "attributes": {
          |      "body": "I like XML better"
          |    },
          |    "relationships": {
          |      "author": {
          |        "data": { "type": "people", "id": "9" }
          |      }
          |    },
          |    "links": {
          |      "self": "http://example.com/comments/12"
          |    }
          |  }, {
          |    "type": "comments",
          |    "id": "5",
          |    "attributes": {
          |      "body": "First!"
          |    },
          |    "relationships": {
          |      "author": {
          |        "data": { "type": "people", "id": "2" }
          |      }
          |    },
          |    "links": {
          |      "self": "http://example.com/comments/5"
          |    }
          |  }]
          |}""".stripMargin
      val topLevel1 = JsonParser(exampleStr1).convertTo[TopLevel]
      val topLevel2 = JsonParser(exampleStr2).convertTo[TopLevel]

      topLevel1 must be equalTo topLevel2
    }
    "successfully deserialize single JSON-API example with correctly encoded links" in {
      val exampleStr =
        """{
          |  "data": {
          |    "type": "articles",
          |    "id": "1",
          |    "attributes": {
          |      "title": "JSON API paints my bikeshed!"
          |    },
          |    "relationships": {
          |      "author": {
          |        "links": {
          |          "self": "http://example.com/articles/1/relationships/author",
          |          "related": "http://example.com/articles/1/author"
          |        }
          |      },
          |      "comments": {
          |        "links": {
          |          "self": "http://example.com/articles/1/relationships/comments",
          |          "related": "http://example.com/articles/1/comments"
          |        },
          |        "data": [
          |          { "type": "comments", "id": "5" },
          |          { "type": "comments", "id": "12" }
          |        ]
          |      }
          |    },
          |    "links": {
          |      "self": "http://example.com/articles/1|5:12|comments"
          |    }
          |  },
          |  "included": [{
          |    "type": "comments",
          |    "id": "5",
          |    "attributes": {
          |      "body": "First!"
          |    },
          |    "relationships": {
          |      "author": {
          |        "data": { "type": "people", "id": "2" }
          |      }
          |    },
          |    "links": {
          |      "self": "http://example.com/comments/5"
          |    }
          |  }, {
          |    "type": "comments",
          |    "id": "12",
          |    "attributes": {
          |      "body": "I like XML better"
          |    },
          |    "relationships": {
          |      "author": {
          |        "data": { "type": "people", "id": "9" }
          |      }
          |    },
          |    "links": {
          |      "self": "http://example.com/comments/12"
          |    }
          |  }]
          |}""".stripMargin
      val exampleJson = JsonParser(exampleStr)
      val topLevel    = exampleJson.convertTo[TopLevel]

      topLevel must beAnInstanceOf[TopLevel.Single]

      val single = topLevel.asInstanceOf[TopLevel.Single]

      single.included must have size 2

      single.data must not beEmpty
      val data = single.data.get._2

      data.`type` must be equalTo "articles"
      data.id must beSome("1")
      data.attributes.getAs[String]('title) must beSome("JSON API paints my bikeshed!")
      data.relationships must have size 2
      data.relationships must haveKey("author")
      data.relationships must haveKey("comments")
      data.links must haveKey("self")

      val selfLink = data.links.getOrElse("self", "error")
      selfLink must_== Url("http://example.com/articles/1%7C5:12%7Ccomments")

    }
  }
}
