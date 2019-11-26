package com.qvantel.jsonapi

import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import spray.json.{JsArray, JsNull, JsObject}
import _root_.spray.json._
import _root_.spray.json.DefaultJsonProtocol._

class RelatedResponseSpec extends Specification with ScalaCheck {
  implicit val apiRoot: com.qvantel.jsonapi.ApiRoot = ApiRoot(None)

  @jsonApiResource final case class Test(id: String)
  @jsonApiResource final case class Test2(id: String)

  val test: Option[Test]      = Some(Test("teståöä•Ωé®")) // test UTF-8
  val emptyTest: Option[Test] = None
  val tests: List[Test]       = List(Test("test 1"), Test("test 2"))
  val emptyTests: List[Test]  = List.empty

  def transformToTest2(in: Test): Test2 = Test2(in.id + "-2")

  "correctly write to one none case" in {
    RelatedResponse(emptyTest).toResponse must be equalTo JsObject(
      "data" -> JsNull
    )

    RelatedResponse(emptyTest).map(transformToTest2).toResponse must be equalTo JsObject(
      "data" -> JsNull
    )
  }

  "correctly write to one some case" in {
    val answer = rawOne(test.get)

    RelatedResponse(test).toResponse must be equalTo answer
    RelatedResponse(test.get).toResponse must be equalTo answer

    val transformedAnswer = rawOne(transformToTest2(test.get))
    RelatedResponse(test).map(transformToTest2).toResponse must be equalTo transformedAnswer
    RelatedResponse(test.get).map(transformToTest2).toResponse must be equalTo transformedAnswer
  }

  "correctly write to many empty case" in {
    RelatedResponse(emptyTests).toResponse must be equalTo JsObject(
      "data" -> JsArray.empty
    )

    RelatedResponse(emptyTests).map(transformToTest2).toResponse must be equalTo JsObject(
      "data" -> JsArray.empty
    )
  }

  "correctly write to many non-empty case" in {
    val answer = rawCollection(tests)

    RelatedResponse(tests).toResponse must be equalTo answer
    RelatedResponse(tests.toSeq).toResponse must be equalTo answer
    RelatedResponse(tests.toIterable).toResponse must be equalTo answer
    RelatedResponse(tests.toSet).toResponse must be equalTo answer

    val transformedAnswer = rawCollection(tests.map(transformToTest2))

    RelatedResponse(tests).map(transformToTest2).toResponse must be equalTo transformedAnswer
    RelatedResponse(tests.toSeq).map(transformToTest2).toResponse must be equalTo transformedAnswer
    RelatedResponse(tests.toIterable).map(transformToTest2).toResponse must be equalTo transformedAnswer
    RelatedResponse(tests.toSet).map(transformToTest2).toResponse must be equalTo transformedAnswer
  }
}
