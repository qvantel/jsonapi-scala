package com.qvantel.jsonapi.model

import org.specs2.mutable.Specification

import spray.json._

class LinkSpec extends Specification {
  "handle / in id correctly" >> {
    val url = "/api/foo%2Fbar/rel"
    val url1 =
      s"""
         |"$url"
      """.stripMargin.parseJson

    val url2 =
      s"""
         |{
         |  "href": "$url"
         |}
      """.stripMargin.parseJson

    url1 must be equalTo url1.convertTo[Link].toJson
    url2 must be equalTo url2.convertTo[Link].toJson

    url1.convertTo[Link].href.toString(Link.uriConfig) must be equalTo url
    url2.convertTo[Link].href.toString(Link.uriConfig) must be equalTo url
  }

  "handle : in id correctly" >> {
    val url = "/api/foo%3Abar/rel"
    val url1 =
      s"""
         |"$url"
      """.stripMargin.parseJson

    val url2 =
      s"""
         |{
         |  "href": "$url"
         |}
      """.stripMargin.parseJson

    url1 must be equalTo url1.convertTo[Link].toJson
    url2 must be equalTo url2.convertTo[Link].toJson

    url1.convertTo[Link].href.toString(Link.uriConfig) must be equalTo url
    url2.convertTo[Link].href.toString(Link.uriConfig) must be equalTo url
  }
}
