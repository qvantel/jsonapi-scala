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

name := "jsonapi-scala"

description := "jsonapi.org scala implementation"

version in ThisBuild := "3.7.2"

startYear in ThisBuild := Some(2015)

organization in ThisBuild := "com.qvantel"

organizationHomepage in ThisBuild := Some(new java.net.URL("https://www.qvantel.com/"))

pomIncludeRepository in ThisBuild := { _ =>
  false
}

licenses in ThisBuild := Seq("BSD-3-Clause" -> url("https://opensource.org/licenses/BSD-3-Clause"))

homepage in ThisBuild := Some(url("https://github.com/qvantel/jsonapi-scala"))

scmInfo in ThisBuild := Some(
  ScmInfo(
    url("https://github.com/qvantel/jsonapi-scala"),
    "scm:git@github.com:qvantel/jsonapi-scala.git"
  )
)

developers in ThisBuild := List(
  Developer(
    id = "Doikor",
    name = "Aki Huttunen",
    email = "doikor@gmail.com",
    url = url("http://doikor.fi")
  )
)

publishMavenStyle := true

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

publishArtifact in Test := false

scalaVersion in ThisBuild := "2.11.8"

ivyScala := ivyScala.value map { _.copy(overrideScalaVersion = true) }

testOptions in Test in ThisBuild ++= Seq(
  Tests.Argument("-oD")
)

javacOptions in ThisBuild ++= Seq("-deprecation", "-g", "-source", "8", "-target", "8", "-Xlint")

incOptions in ThisBuild := (incOptions in ThisBuild).value.withNameHashing(true)

scalacOptions in ThisBuild ++= Seq(
  "-deprecation",
  "-feature",
  "-target:jvm-1.8",
  "-unchecked",
  // advanced
  "-Xcheckinit",
  "-Xfuture",
  "-Xlint:_",
  "-Xlog-reflective-calls",
  "-Xlog-free-terms",
  "-Xlog-free-types",
  "-Xmax-classfile-name",
  "130",
  "-Xverify",
  // private
  "-Ybackend:GenBCode",
  "-Ybreak-cycles",
  "-Yclosure-elim",
  "-Yconst-opt",
  "-Ydead-code",
  "-Ydelambdafy:method",
  "-Yinline",
  "-Yinline-handlers",
  "-Yinline-warnings",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-inaccessible",
  "-Ywarn-infer-any",
  "-Ywarn-nullary-override",
  "-Ywarn-nullary-unit",
  "-Ywarn-numeric-widen",
  "-Ywarn-unused",
  "-Ywarn-unused-import",
  "-Ywarn-value-discard",
//  "-Ymacro-debug-lite",
  ""
)

wartremoverErrors in (Compile, compile) ++= (Warts.unsafe.toSet -- Set(Wart.Any,
                                                                       Wart.Null,
                                                                       Wart.NonUnitStatements,
                                                                       Wart.AsInstanceOf,
                                                                       Wart.DefaultArguments,
                                                                       Wart.Throw)).toSeq

libraryDependencies in ThisBuild ++= Seq(
  // testing related
  "org.specs2"        %% "specs2-core"       % "3.8.9"  % "test",
  "org.specs2"        %% "specs2-junit"      % "3.8.9"  % "test",
  "org.specs2"        %% "specs2-scalacheck" % "3.8.9"  % "test",
  "io.spray"          %% "spray-testkit"     % "1.3.4"  % "test",
  "com.typesafe.akka" %% "akka-testkit"      % "2.4.14" % "test"
)

lazy val core = (project in file("core"))
  .settings(
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
      "com.chuusai"           %% "shapeless"       % "2.3.2",
      "io.backchat.inflector" %% "scala-inflector" % "1.3.5",
      "com.typesafe.akka"     %% "akka-actor"      % "2.4.14" excludeAll (ExclusionRule(
        organization = "com.typesafe.akka",
        name = "akka-cluster_2.11"), ExclusionRule(organization = "com.typesafe.akka", name = "akka-remote_2.11")),
      "io.spray"         %% "spray-json"  % "1.3.2",
      "io.spray"         %% "spray-httpx" % "1.3.4",
      "net.virtual-void" %% "json-lenses" % "0.6.1" excludeAll ExclusionRule(organization = "org.parboiled",
                                                                             name = "parboiled-scala_2.11")
    )
  )

lazy val spray = (project in file("spray"))
  .dependsOn(core, model)
  .settings(
    libraryDependencies ++= Seq(
      "io.spray" %% "spray-httpx"  % "1.3.4",
      "io.spray" %% "spray-client" % "1.3.4" excludeAll ExclusionRule(organization = "com.typesafe.akka",
                                                                      name = "akka-actor_2.11"),
      "io.spray" %% "spray-routing-shapeless23" % "1.3.4" excludeAll ExclusionRule(organization = "com.chuusai",
                                                                                   name = "shapeless_2.11")
    )
  )

lazy val model = (project in file("model"))
  .dependsOn(core)

lazy val root = (project in file(".")).aggregate(core, spray, model)
