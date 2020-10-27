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

import org.specs2.mutable.Specification
import shapeless._
import _root_.spray.json.DefaultJsonProtocol._
import io.lemonlabs.uri.Url
import io.lemonlabs.uri.typesafe.dsl._

final class CoproductSpec extends Specification {
  implicit val apiRoot: com.qvantel.jsonapi.ApiRoot = ApiRoot(None)

  private[this] final case class Limb(id: String, fingers: Int)
  private[this] final case class LightBulb(id: String, color: Long)
  private[this] final case class Eye(id: String, fov: Float, friend: Option[ToOne[Robot]])
  private[this] type Module = Limb :+: LightBulb :+: Eye :+: CNil
  private[this] final case class Robot(id: String, arm: ToOne[Limb], module: PolyToOne[Module])
  private[this] final case class CrippledRobot(id: String, module: Option[PolyToOne[Module]])
  private[this] final case class AdvancedRobot(id: String, modules: PolyToMany[Module])

  private[this] implicit object ModulePolyIdentifiable extends PolyIdentifiable[Module] {
    private[this] object polyIdentify extends Poly1 {
      implicit def caseLimb      = at[Limb](obj => obj.id)
      implicit def caseLightBulb = at[LightBulb](obj => obj.id)
      implicit def caseEye       = at[Eye](obj => obj.id)
    }

    private[this] object polyResourceType extends Poly1 {
      implicit def caseLimb      = at[Limb](_ => implicitly[ResourceType[Limb]].resourceType)
      implicit def caseLightBulb = at[LightBulb](_ => implicitly[ResourceType[LightBulb]].resourceType)
      implicit def caseEye       = at[Eye](_ => implicitly[ResourceType[Eye]].resourceType)
    }

    override def identify(a: Module): String     = a fold polyIdentify
    override def resourceType(a: Module): String = a fold polyResourceType
  }

  private[this] object Limb {
    implicit val limbResourceType: ResourceType[Limb] = ResourceType[Limb]("limbs")
    implicit val limbIdentifiable: Identifiable[Limb] = Identifiable.by(_.id)
    implicit val limbPathTo: PathTo[Limb] = new PathToId[Limb] {
      override def root: Url = "/limbs"
    }
    implicit val limbJsonApiFormat: JsonApiFormat[Limb] = jsonApiFormat[Limb]

    implicit lazy val limbIncludes: Includes[Limb] = includes[Limb]
  }

  private[this] object Eye {
    implicit val eyeResourceType: ResourceType[Eye] = ResourceType[Eye]("eyes")
    implicit val eyeIdentifiable: Identifiable[Eye] = Identifiable.by(_.id)
    implicit val eyePathTo: PathTo[Eye] = new PathToId[Eye] {
      override def root: Url = "/eyes"
    }
    implicit val eyeJsonApiFormat: JsonApiFormat[Eye] = jsonApiFormat[Eye]

    implicit lazy val eyeIncludes: Includes[Eye] = includes[Eye]
  }

  private[this] object LightBulb {
    implicit val lightBulbResourceType: ResourceType[LightBulb] = ResourceType[LightBulb]("light-bulbs")
    implicit val lightBulbIdentifiable: Identifiable[LightBulb] = Identifiable.by(_.id)
    implicit val lightBulbPathTo: PathTo[LightBulb] = new PathToId[LightBulb] {
      override def root: Url = "/light-bulbs"
    }
    implicit val lightBulbJsonApiFormat: JsonApiFormat[LightBulb] = jsonApiFormat[LightBulb]

    implicit lazy val lightBulbIncludes: Includes[LightBulb] = includes[LightBulb]
  }

  private[this] object Robot {
    implicit val robotResourceType: ResourceType[Robot] = ResourceType[Robot]("robots")
    implicit val robotIdentifiable: Identifiable[Robot] = Identifiable.by(_.id)
    implicit val robotPathTo: PathTo[Robot] = new PathToId[Robot] {
      override def root: Url = "/robots"
    }
    implicit val robotJsonApiFormat: JsonApiFormat[Robot] = jsonApiFormat[Robot]

    implicit lazy val robotIncludes: Includes[Robot] = includes[Robot]
  }

  private[this] object AdvancedRobot {
    implicit val advancedRobotResourceType: ResourceType[AdvancedRobot] =
      ResourceType[AdvancedRobot]("advanced-robots")
    implicit val advancedRobotIdentifiable: Identifiable[AdvancedRobot] = Identifiable.by(_.id)
    implicit val advancedRobotPathTo: PathTo[AdvancedRobot] = new PathToId[AdvancedRobot] {
      override def root: Url = "/advanced-robots"
    }
    implicit val advancedRobotJsonApiFormat: JsonApiFormat[AdvancedRobot] = jsonApiFormat[AdvancedRobot]

    implicit lazy val advancedRobotIncludes: Includes[AdvancedRobot] = includes[AdvancedRobot]
  }

  private[this] object Examples {
    val limb           = Limb("l", 3)
    val limb2          = Limb("y", 9)
    val lightBulb      = LightBulb("b", 0xff1100L)
    val eye            = Eye("e", 60.0f, None)
    val robot1         = Robot("r1", ToOne.loaded(limb2), PolyToOne.reference[Module, Limb](limb.id))
    val robot2         = Robot("r2", ToOne.loaded(limb2), PolyToOne.loaded(lightBulb))
    val robot3         = Robot("r3", ToOne.loaded(limb2), PolyToOne.loaded(eye))
    val advancedRobot1 = AdvancedRobot("a1", PolyToMany.reference)
    val advancedRobot2 =
      AdvancedRobot("a2", PolyToMany.loaded(Seq(Coproduct[Module](eye), Coproduct[Module](lightBulb))))
  }

  "jsonApiFormat" should {
    "at least compile" in {
      val pjr1 = implicitly[JsonApiFormat[Robot]].write(Examples.robot1)
      val pjr2 = implicitly[JsonApiFormat[Robot]].write(Examples.robot2)
      val pjr3 = implicitly[JsonApiFormat[Robot]].write(Examples.robot3)
      val ijr1 = implicitly[JsonApiFormat[Robot]].included(Examples.robot1)
      val ijr2 = implicitly[JsonApiFormat[Robot]].included(Examples.robot2)
      val ijr3 = implicitly[JsonApiFormat[Robot]].included(Examples.robot3)
      val pja1 = implicitly[JsonApiFormat[AdvancedRobot]].write(Examples.advancedRobot1)
      val pja2 = implicitly[JsonApiFormat[AdvancedRobot]].write(Examples.advancedRobot2)
      val ija1 = implicitly[JsonApiFormat[AdvancedRobot]].included(Examples.advancedRobot1)
      val ija2 = implicitly[JsonApiFormat[AdvancedRobot]].included(Examples.advancedRobot2)
      ok
    }

    "at least compile with a sparse fieldset defined" in {
      val sparseFields: Map[String, List[String]] = Map("robots" -> List("arm"), "advanced-robots" -> List("modules"))

      val pjr1 = implicitly[JsonApiFormat[Robot]].write(Examples.robot1, sparseFields)
      val pjr2 = implicitly[JsonApiFormat[Robot]].write(Examples.robot2, sparseFields)
      val pjr3 = implicitly[JsonApiFormat[Robot]].write(Examples.robot3, sparseFields)
      val ijr1 = implicitly[JsonApiFormat[Robot]].included(Examples.robot1, sparseFields)
      val ijr2 = implicitly[JsonApiFormat[Robot]].included(Examples.robot2, sparseFields)
      val ijr3 = implicitly[JsonApiFormat[Robot]].included(Examples.robot3, sparseFields)
      val pja1 = implicitly[JsonApiFormat[AdvancedRobot]].write(Examples.advancedRobot1, sparseFields)
      val pja2 = implicitly[JsonApiFormat[AdvancedRobot]].write(Examples.advancedRobot2, sparseFields)
      val ija1 = implicitly[JsonApiFormat[AdvancedRobot]].included(Examples.advancedRobot1, sparseFields)
      val ija2 = implicitly[JsonApiFormat[AdvancedRobot]].included(Examples.advancedRobot2, sparseFields)
      ok
    }

    "includes work" in {
      Eye.eyeIncludes.includesAllowed("friend", "friend.arm", "friend.module") must beTrue
      Robot.robotIncludes.includesAllowed("arm", "module") must beTrue
      AdvancedRobot.advancedRobotIncludes.includesAllowed("modules") must beTrue
    }
  }
}
