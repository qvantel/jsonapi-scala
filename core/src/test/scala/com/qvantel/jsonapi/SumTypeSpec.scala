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
 * Neither the name of the <organization> nor the
      names of its contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package com.qvantel.jsonapi

/*import org.specs2.mutable.Specification
import _root_.spray.http.Uri.Path
import _root_.spray.json.DefaultJsonProtocol._
import _root_.spray.json._


final class SumTypeSpec extends Specification {
  implicit val apiRoot = ApiRoot(None)

  private[this] sealed trait Shape {
    def id: String
  }

  private[this] final case class Circle(id: String, radius: Double) extends Shape
  private[this] final case class Rectangle(id: String, width: Double, height: Double) extends Shape
  private[this] final case class Composite(id: String, zIndex: Int, first: ToOne[Shape], second: Option[ToOne[Shape]]) extends Shape
  private[this] final case class UnboundedComposite(id: String, zIndex: Int, shapes: ToMany[Shape]) extends Shape

  private[this] object Shape {
    implicit val shapeIdentifiable: Identifiable[Shape] = Identifiable.by(_.id)
    implicit val shapeResourceType: ResourceType[Shape] = ResourceType("shapes")
    implicit val shapePathTo: PathTo[Shape] = new PathTo[Shape] {
      override def self(id: String): Path = Path("/shapes") / id
    }
    implicit val shapeJsonApiformat: JsonApiFormat[Shape] = jsonApiFormat[Shape]

    implicit lazy val shapeIncludes: Includes[Shape] = com.qvantel.jsonapi.includes[Shape]

    def circle(id: String, radius: Double): Shape = Circle(id, radius)
    def rectangle(id: String, width: Double, height: Double): Shape = Rectangle(id, width, height)
    def composite(id: String, zIndex: Int, first: ToOne[Shape], second: Option[ToOne[Shape]]): Shape = Composite(id, zIndex, first, second)
    def unboundedComposite(id: String, zIndex: Int, shapes: ToMany[Shape]): Shape = UnboundedComposite(id, zIndex, shapes)
  }

  private[this] final case class Drawing(id: String, name: String, shape: Option[ToOne[Shape]])
  private[this] final case class NonEmptyDrawing(id: String, name: String, shape: ToOne[Shape])
  private[this] final case class Scene(id: String, name: String, shapes: ToMany[Shape])

  private[this] object Scene {
    implicit val sceneIdentifiable: Identifiable[Scene] = Identifiable.by(_.id)
    implicit val sceneResourceType: ResourceType[Scene] = ResourceType("scenes")
    implicit val scenePathTo: PathTo[Scene] = new PathTo[Scene] {
      override def self(id: String): Path = Path("/scenes") / id
    }
    implicit val sceneJsonApiformat: JsonApiFormat[Scene] = jsonApiFormat[Scene]

    implicit lazy val sceneIncludes: Includes[Scene] = com.qvantel.jsonapi.includes[Scene]
  }

  private[this] object Examples {
    val circle = Shape.circle("c", 3.0)
    val rectangle = Shape.rectangle("r", 3.14, 14.3)
    val composite1 = Shape.composite("o1", 6, ToOne.loaded(circle), Option(ToOne.loaded(rectangle)))
    val composite2 = Shape.composite("o2", 6, ToOne.reference(circle.id), Option(ToOne.reference(rectangle.id)))
    val composite3 = Shape.composite("o3", 6, ToOne.loaded(circle), Option.empty)
    val composite4 = Shape.composite("o4", 6, ToOne.reference(circle.id), Option.empty)
    val composite5 = Shape.composite("o5", 6, ToOne.loaded(composite2), Option(ToOne.loaded(composite3)))
    val composite6 = Shape.composite("o6", 6, ToOne.loaded(composite5), Option(ToOne.loaded(composite4)))
    val unboundedComposite1 = Shape.unboundedComposite("u1", 7, ToMany.reference)
    val unboundedComposite2 = Shape.unboundedComposite("u2", 7, ToMany.loaded(Seq(circle, rectangle)))
    val unboundedComposite3 = Shape.unboundedComposite("u3", 7, ToMany.loaded(Seq(composite1, composite6, unboundedComposite2)))
  }

  def primary[A: JsonApiFormat](a: A): JsValue = implicitly[JsonApiFormat[A]].write(a)
  def includes[A: JsonApiFormat](a: A): Set[JsObject] = implicitly[JsonApiFormat[A]].included(a)

  def jsonApiObject(id:         String,
                    tpe:        String,
                    meta:       Map[String, JsValue] = Map.empty,
                    attributes: Map[String, JsValue] = Map.empty,
                    links:      Map[String, String] = Map.empty) = {
    JsObject(
      "id"         -> id.toJson,
      "type"       -> tpe.toJson,
      "meta"       -> JsObject(meta),
      "attributes" -> JsObject(attributes),
      "links"      -> JsObject(links.mapValues(JsString(_))))
  }

  def rt[A: ResourceType]: String = implicitly[ResourceType[A]].resourceType

  "jsonApiFormat" should {
    "should be able to serialize a simple instance of a sum type (circle)" in {
      val shape = Examples.circle.asInstanceOf[Circle]
      val expected = jsonApiObject(shape.id, rt[Shape],
        meta = Map("shapes-type" -> "Circle".toJson),
        attributes = Map("radius" -> shape.radius.toJson),
        links      = Map("self" -> s"/shapes/${shape.id}"))
      primary(Examples.circle) mustEqual expected
    }

    "should be able to serialize a simple instance of a sum type (rectangle)" in {
      val shape = Examples.rectangle.asInstanceOf[Rectangle]
      val expected = jsonApiObject(shape.id, rt[Shape],
        meta = Map("shapes-type" -> "Rectangle".toJson),
        attributes = Map("width" -> shape.width.toJson, "height" -> shape.height.toJson),
        links      = Map("self" -> s"/shapes/${shape.id}"))
      primary(Examples.rectangle) mustEqual expected
    }

    "correctly generate includes for SymTypes" in {
      Scene.sceneIncludes.includeAllowed("shapes") must beTrue
      Shape.shapeIncludes.includesAllowed("test") must beFalse
    }
  }
} */
