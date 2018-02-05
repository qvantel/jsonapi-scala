package com.qvantel.jsonapi

import org.specs2.mutable.Specification
import com.netaporter.uri.dsl._
import _root_.spray.json.DefaultJsonProtocol._

class PathToSpec extends Specification {
  implicit val apiRoot = ApiRoot(Some("foo" / "bar"))

  @jsonApiResource final case class Test(id: String)

  "apiRoot should be printed to PathTo output" >> {
    val t = Test("1")

    PathTo[Test].entity(t) must be equalTo "/foo/bar/tests/1"
    PathToId[Test].self("test") must be equalTo "/foo/bar/tests/test"

    rawOne(t)
      .fields("data")
      .asJsObject
      .fields("links")
      .asJsObject
      .fields("self")
      .convertTo[String] must be equalTo "/foo/bar/tests/1"
  }

  "root should print out apiRoot" >> {
    PathTo[Test].root must be equalTo "/foo/bar/tests"
  }
}
