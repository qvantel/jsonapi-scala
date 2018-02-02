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

import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import _root_.spray.json.DefaultJsonProtocol._
import _root_.spray.json.{JsObject, JsonParser}
import com.netaporter.uri.Uri
import com.netaporter.uri.dsl._

import com.qvantel.jsonapi.Helpers._

final class MacrosSpec extends Specification with ScalaCheck {
  implicit val apiRoot: com.qvantel.jsonapi.ApiRoot = ApiRoot(None)

  "jsonapi.org macros" should {
    "correctly generate single level ToOne includes" in {
      final case class Root(id: String, child: ToOne[Child])
      final case class Child(id: String)

      implicit lazy val childIncludes: com.qvantel.jsonapi.Includes[Child] = includes[Child]

      includes[Root].includeAllowed("child") must beTrue

      prop { part: String =>
        childIncludes.includeAllowed(part) must beFalse
      }.setGen(makeGenInclude("child"))
    }

    "correctly generate single level Option[ToOne] includes" in {
      final case class Root(id: String, maybeChild: Option[ToOne[Child]])
      final case class Child(id: String)

      implicit lazy val childIncludes: com.qvantel.jsonapi.Includes[Child] = includes[Child]

      includes[Root].includeAllowed("test") must beFalse
      includes[Root].includeAllowed("maybe-child") must beTrue

      prop { part: String =>
        childIncludes.includeAllowed(part) must beFalse
      }.setGen(makeGenInclude("test", "maybe-child"))
    }

    "correctly generate single level ToMany includes" in {
      final case class Root(id: String, children: ToMany[Child])
      final case class Child(id: String)

      implicit lazy val childIncludes: com.qvantel.jsonapi.Includes[Child] = includes[Child]

      includes[Root].includeAllowed("test") must beFalse
      includes[Root].includeAllowed("children") must beTrue

      prop { part: String =>
        childIncludes.includeAllowed(part) must beFalse
      }.setGen(makeGenInclude("test", "children"))
    }

    "correctly handle multi level includes" in {
      final case class Root(id: String, child: ToOne[Child1], leaf: ToOne[Child2])
      final case class Child1(id: String, child: ToOne[Child2])
      final case class Child2(id: String)

      implicit lazy val child2Includes: com.qvantel.jsonapi.Includes[Child2] = includes[Child2]
      implicit lazy val child1Includes: com.qvantel.jsonapi.Includes[Child1] = includes[Child1]
      implicit lazy val rootIncludes: com.qvantel.jsonapi.Includes[Root]     = includes[Root]

      rootIncludes.includeAllowed("test") must beFalse
      rootIncludes.includeAllowed("child") must beTrue
      rootIncludes.includeAllowed("leaf") must beTrue
      rootIncludes.includeAllowed("child.child") must beTrue

      prop { part: String =>
        rootIncludes.includeAllowed(part) must beFalse
      }.setGen(makeGenInclude("test", "child", "leaf", "child"))
    }

    "correctly handle looped includes" in {
      final case class Fun(id: String, loop: ToOne[Loop])
      final case class Loop(id: String, fun: ToOne[Fun])

      // lazy required when manually calling includes macro normally just use @jsonApiResource annotation
      implicit lazy val funIncludes: Includes[Fun]   = includes[Fun]
      implicit lazy val loopIncludes: Includes[Loop] = includes[Loop]

      funIncludes.includeAllowed("loop") must beTrue
      loopIncludes.includeAllowed("fun") must beTrue

      funIncludes.includeAllowed(
        (0 to 50)
          .map { x =>
            if (x % 2 == 0) "loop" else "fun"
          }
          .mkString(".")) must beTrue

      loopIncludes.includeAllowed(
        (0 to 50)
          .map { x =>
            if (x % 2 == 0) "fun" else "loop"
          }
          .mkString(".")) must beTrue

      prop { part: String =>
        funIncludes.includeAllowed(part) must beFalse
        loopIncludes.includeAllowed(part) must beFalse
      }.setGen(makeGenInclude("fun", "loop"))
    }

    "correctly handle self loop include" in {
      final case class Looped(id: String, loop: ToOne[Looped])

      implicit lazy val loopedIncludes: Includes[Looped] = includes[Looped]

      loopedIncludes.includeAllowed("loop") must beTrue

      loopedIncludes.includeAllowed((0 to 50).map(x => "loop").mkString("."))

      prop { part: String =>
        loopedIncludes.includeAllowed(part) must beFalse
      }.setGen(makeGenInclude("loop"))
    }

    "correctly produce json" in {
      import _root_.spray.json.lenses.JsonLenses._

      final case class Root(id: String,
                            nameMangling: String,
                            rField: Boolean,
                            aField: String,
                            bField: Int,
                            cField: BigDecimal)
          extends HasId

      implicit val resourceType: com.qvantel.jsonapi.ResourceType[Root] = ResourceType[Root]("root")

      implicit val pathTo: PathTo[Root] = new PathTo[Root] {
        override final def root: Uri = "/roots"
      }

      val format = jsonApiFormat[Root]

      val data = Root("1", "test data", false, "a field", 3, BigDecimal(3.2))

      val json = format.write(data)

      json.extract[String]('attributes / "name-mangling") must_== data.nameMangling
      json.extract[String]('id) must_== "1"
      json.extract[String]('type) must_== "root"
      // check that attributes are ordered
      json.extract[JsObject]('attributes) must_== JsonParser(
        """{"a-field":"a field","b-field":3,"c-field":3.2,"name-mangling":"test data","r-field":false}""")
    }

    "materialize multi level relationships" in {
      final case class Root(id: String, nameMangling: String, child: ToOne[Child])           extends HasId
      final case class Child(id: String, child: Option[ToOne[Leaf]], children: ToMany[Leaf]) extends HasId
      final case class Leaf(id: String, end: ToOne[End])                                     extends HasId
      final case class End(id: String)                                                       extends HasId

      object End {
        implicit lazy val endResourceType: com.qvantel.jsonapi.ResourceType[End] = ResourceType[End]("end")
        implicit lazy val endPathTo: PathTo[End] = new PathTo[End] {
          override final def root: Uri = "/end"
        }
        implicit lazy val endFormat: com.qvantel.jsonapi.JsonApiFormat[End] = jsonApiFormat[End]
        implicit lazy val endIncludes: Includes[End]                        = includes[End]
      }

      object Leaf {
        implicit lazy val leafResourceType: com.qvantel.jsonapi.ResourceType[Leaf] = ResourceType[Leaf]("leaves")
        implicit lazy val leafPathTo: PathTo[Leaf] = new PathTo[Leaf] {
          override final def root: Uri = "/leaves"
        }
        implicit lazy val leafFormat: com.qvantel.jsonapi.JsonApiFormat[Leaf] = jsonApiFormat[Leaf]
        implicit lazy val leafIncludes: Includes[Leaf]                        = includes[Leaf]
      }

      object Child {
        implicit lazy val childResourceType: com.qvantel.jsonapi.ResourceType[Child] = ResourceType[Child]("children")
        implicit lazy val childPathTo: PathTo[Child] = new PathTo[Child] {
          override final def root: Uri = "/children"
        }
        implicit lazy val childFormat: com.qvantel.jsonapi.JsonApiFormat[Child] = jsonApiFormat[Child]
        implicit lazy val childIncludes: Includes[Child]                        = includes[Child]
      }

      object Root {
        implicit lazy val rootResourceType: com.qvantel.jsonapi.ResourceType[Root] = ResourceType[Root]("roots")
        implicit lazy val rootPathTo: PathTo[Root] = new PathTo[Root] {
          override final def root: Uri = "/roots"
        }
        implicit lazy val rootFormat: com.qvantel.jsonapi.JsonApiFormat[Root] = jsonApiFormat[Root]
        implicit lazy val rootIncludes: Includes[Root]                        = includes[Root]
      }

      val end  = End("666")
      val leaf = Leaf("3", ToOne.loaded(end))
      val child = Child("2",
                        Some(ToOne.loaded(leaf)),
                        ToMany.loaded(Seq(Leaf("5", ToOne.loaded(end)), Leaf("30", ToOne.loaded(end)))))
      val root = Root("1", "test data", ToOne.loaded(child))

      val json = Root.rootFormat.write(root)

      import _root_.spray.json.lenses.JsonLenses._

      json.extract[String]('attributes / "name-mangling") must_== root.nameMangling
      json.extract[String]('id) must_== "1"
      json.extract[String]('type) must_== "roots"

      json.extract[String]('relationships / 'child / 'data / 'id) must_== child.id

      val includedJson = Root.rootFormat.included(root)

      includedJson.exists(_.extract[String]('id) == "5") must_== true
      includedJson.exists(_.extract[String]('id) == "30") must_== true
      includedJson.exists(_.extract[String]('id) == "3") must_== true
      includedJson.exists(_.extract[String]('id) == "2") must_== true
      includedJson.exists(_.extract[String]('id) == "666") must_== true
    }
  }
}
