package com.qvantel.jsonapi

import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import spray.json.{JsArray, JsNull, JsObject}
import _root_.spray.json.DefaultJsonProtocol._
import fs2.Stream

class RelatedResponseSpec extends Specification with ScalaCheck {
  implicit val apiRoot: com.qvantel.jsonapi.ApiRoot = ApiRoot(None)

  @jsonApiResource final case class Test(id: String, name: String, age: Int)
  @jsonApiResource final case class Test2(id: String, name: String, age: Int)

  val test: Option[Test]      = Some(Test("teståöä•Ωé®", "someName", 20)) // test UTF-8
  val emptyTest: Option[Test] = None
  val tests: List[Test]       = List(Test("test 1", "someName1", 20), Test("test 2", "someName2", 21))
  val emptyTests: List[Test]  = List.empty

  def transformToTest2(in: Test): Test2 = Test2(in.id + "-2", in.name, in.age)

  "correctly write to one none case" in {
    RelatedResponse(emptyTest).toResponse must be equalTo JsObject(
      "data" -> JsNull
    )

    RelatedResponse(emptyTest).map(transformToTest2).toResponse must be equalTo JsObject(
      "data" -> JsNull
    )

    RelatedResponse(emptyTest).filter(_ => true).toResponse must be equalTo JsObject(
      "data" -> JsNull
    )

    RelatedResponse(test).filter(_ => false).toResponse must be equalTo JsObject(
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

    RelatedResponse(test).filter(_.age == 20).toResponse must be equalTo answer
  }

  "correctly write to many empty case" in {
    RelatedResponse(emptyTests).toResponse must be equalTo JsObject(
      "data" -> JsArray.empty
    )

    RelatedResponse(emptyTests).map(transformToTest2).toResponse must be equalTo JsObject(
      "data" -> JsArray.empty
    )

    RelatedResponse(emptyTests).filter(_ => true).toResponse must be equalTo JsObject(
      "data" -> JsArray.empty
    )

    RelatedResponse(tests).filter(_ => false).toResponse must be equalTo JsObject(
      "data" -> JsArray.empty
    )

    RelatedResponse(Stream.emits(emptyTests)).filter(_ => true).toResponse must be equalTo JsObject(
      "data" -> JsArray.empty
    )

    RelatedResponse(Stream.emits(tests)).filter(_ => false).toResponse must be equalTo JsObject(
      "data" -> JsArray.empty
    )
  }

  "correctly write to many non-empty case" in {
    val answer = rawCollection(tests)

    RelatedResponse(tests).toResponse must be equalTo answer
    RelatedResponse(tests.toSeq).toResponse must be equalTo answer
    RelatedResponse(tests.toIterable).toResponse must be equalTo answer
    RelatedResponse(tests.toSet).toResponse must be equalTo answer
    RelatedResponse(Stream.emits(tests)).toResponse must be equalTo answer

    val transformedAnswer = rawCollection(tests.map(transformToTest2))

    RelatedResponse(tests).map(transformToTest2).toResponse must be equalTo transformedAnswer
    RelatedResponse(tests.toSeq).map(transformToTest2).toResponse must be equalTo transformedAnswer
    RelatedResponse(tests.toIterable).map(transformToTest2).toResponse must be equalTo transformedAnswer
    RelatedResponse(tests.toSet).map(transformToTest2).toResponse must be equalTo transformedAnswer
    RelatedResponse(Stream.emits(tests)).map(transformToTest2).toResponse must be equalTo transformedAnswer

    val filteredAnswer = rawCollection(tests.filter(_.age > 20))

    RelatedResponse(tests).filter(_.age > 20).toResponse must be equalTo filteredAnswer
    RelatedResponse(Stream.emits(tests)).filter(_.age > 20).toResponse must be equalTo filteredAnswer
  }

  "correctly write sparse fieldsets" in {
    implicit val sparseFields: Map[String, List[String]] = Map("tests" -> List("age"), "test2s" -> List("age"))
    val answer                                           = rawOne(test.get)

    RelatedResponse(test).toResponse must be equalTo answer
    RelatedResponse(test.get).toResponse must be equalTo answer

    val transformedAnswer = rawOne(transformToTest2(test.get))

    RelatedResponse(test).map(transformToTest2).toResponse must be equalTo transformedAnswer
    RelatedResponse(test.get).map(transformToTest2).toResponse must be equalTo transformedAnswer
  }
}
