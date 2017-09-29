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

val scala212 = Seq(
  "-deprecation", // Emit warning and location for usages of deprecated APIs.
  "-encoding",
  "utf-8", // Specify character encoding used by source files.
  "-explaintypes", // Explain type errors in more detail.
  "-feature", // Emit warning and location for usages of features that should be imported explicitly.
  "-language:existentials", // Existential types (besides wildcard types) can be written and inferred
  "-language:experimental.macros", // Allow macro definition (besides implementation and application)
  "-language:higherKinds", // Allow higher-kinded types
  "-language:implicitConversions", // Allow definition of implicit functions called views
  "-unchecked", // Enable additional warnings where generated code depends on assumptions.
  "-Xcheckinit", // Wrap field accessors to throw an exception on uninitialized access.
  "-Xfuture", // Turn on future language features.
  "-Xlint:adapted-args", // Warn if an argument list is modified to match the receiver.
  "-Xlint:by-name-right-associative", // By-name parameter of right associative operator.
  "-Xlint:constant", // Evaluation of a constant arithmetic expression results in an error.
  "-Xlint:delayedinit-select", // Selecting member of DelayedInit.
  "-Xlint:doc-detached", // A Scaladoc comment appears to be detached from its element.
  "-Xlint:inaccessible", // Warn about inaccessible types in method signatures.
  "-Xlint:infer-any", // Warn when a type argument is inferred to be `Any`.
  "-Xlint:missing-interpolator", // A string literal appears to be missing an interpolator id.
  "-Xlint:nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
  "-Xlint:nullary-unit", // Warn when nullary methods return Unit.
  "-Xlint:option-implicit", // Option.apply used implicit view.
  "-Xlint:package-object-classes", // Class or object defined in package object.
  "-Xlint:poly-implicit-overload", // Parameterized overloaded implicit methods are not visible as view bounds.
  "-Xlint:private-shadow", // A private field (or class parameter) shadows a superclass field.
  "-Xlint:stars-align", // Pattern sequence wildcard must align with sequence component.
  "-Xlint:type-parameter-shadow", // A local type parameter shadows a type already in scope.
  "-Xlint:unsound-match", // Pattern match may not be typesafe.
  "-Yno-adapted-args", // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
  "-Ypartial-unification", // Enable partial unification in type constructor inference
  "-Ywarn-dead-code", // Warn when dead code is identified.
  "-Ywarn-extra-implicit", // Warn when more than one implicit parameter section is defined.
  "-Ywarn-inaccessible", // Warn about inaccessible types in method signatures.
  "-Ywarn-infer-any", // Warn when a type argument is inferred to be `Any`.
  "-Ywarn-nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
  "-Ywarn-nullary-unit", // Warn when nullary methods return Unit.
  "-Ywarn-numeric-widen", // Warn when numerics are widened.
  "-Ywarn-unused:implicits", // Warn if an implicit parameter is unused.
  "-Ywarn-unused:imports", // Warn if an import selector is not referenced.
  "-Ywarn-unused:locals", // Warn if a local definition is unused.
  "-Ywarn-unused:params", // Warn if a value parameter is unused.
  "-Ywarn-unused:patvars", // Warn if a variable bound in a pattern is unused.
  "-Ywarn-unused:privates", // Warn if a private member is unused.
  "-Ywarn-value-discard" // Warn when non-Unit expression results are unused.
)

val scala211 = Seq(
  "-deprecation",
  "-feature",
  "-target:jvm-1.8",
  "-unchecked",
  "-encoding",
  "utf-8",
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

description in ThisBuild := "jsonapi.org scala implementation"

version in ThisBuild := "5.1.0"

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

publishMavenStyle in ThisBuild := true

publishTo in ThisBuild := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

publishArtifact in Test := false

testOptions in Test in ThisBuild ++= Seq(
  Tests.Argument("-oD")
)

javacOptions in ThisBuild ++= Seq("-deprecation", "-g", "-source", "8", "-target", "8", "-Xlint")

val testDeps = Seq(
  // testing related
  "org.specs2" %% "specs2-core"          % "3.9.1" % "test",
  "org.specs2" %% "specs2-junit"         % "3.9.1" % "test",
  "org.specs2" %% "specs2-scalacheck"    % "3.9.1" % "test",
  "org.specs2" %% "specs2-matcher-extra" % "3.9.1" % "test"
)

wartremoverErrors in (Compile, compile) ++= (Warts.unsafe.toSet -- Set(Wart.Any,
                                                                       Wart.Null,
                                                                       Wart.NonUnitStatements,
                                                                       Wart.AsInstanceOf,
                                                                       Wart.DefaultArguments,
                                                                       Wart.Throw)).toSeq

lazy val core = (project in file("core"))
  .settings(
    name := "jsonapi-scala-core",
    scalaVersion := "2.11.11",
    crossScalaVersions := Seq("2.12.3", "2.11.11"),
    scalacOptions ++= {
      if (scalaVersion.value startsWith "2.11.") {
        scala211
      } else {
        scala212
      }
    },
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
      "com.chuusai"      %% "shapeless"       % "2.3.2",
      "com.qvantel"      %% "scala-inflector" % "1.3.6",
      "io.spray"         %% "spray-json"      % "1.3.2",
      "io.lemonlabs"     %% "scala-uri"       % "0.5.0",
      "net.virtual-void" %% "json-lenses"     % "0.6.2" excludeAll ExclusionRule(organization = "org.parboiled",
                                                                             name = "parboiled-scala_2.11"),
      "io.monix" %% "monix" % "3.0.0-M1" // pulls in cats 1.0.0-MF and cats-effect 0.4
    ) ++ testDeps
  )

lazy val model = (project in file("model"))
  .dependsOn(core)
  .settings(
    name := "jsonapi-scala-model",
    scalaVersion := "2.11.11",
    crossScalaVersions := Seq("2.12.3", "2.11.11"),
    scalacOptions ++= {
      if (scalaVersion.value startsWith "2.11.") {
        scala211
      } else {
        scala212
      }
    },
    libraryDependencies ++= testDeps
  )

lazy val spray = (project in file("spray"))
  .dependsOn(core, model)
  .settings(
    name := "jsonapi-scala-spray",
    scalaVersion := "2.11.11",
    crossScalaVersions := Seq("2.11.11"),
    scalacOptions ++= scala211,
    libraryDependencies ++= Seq(
      compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
      "io.spray" %% "spray-httpx"  % "1.3.4",
      "io.spray" %% "spray-client" % "1.3.4" excludeAll ExclusionRule(organization = "com.typesafe.akka",
                                                                      name = "akka-actor_2.11"),
      "io.spray" %% "spray-routing-shapeless23" % "1.3.4" excludeAll ExclusionRule(organization = "com.chuusai",
                                                                                   name = "shapeless_2.11"),
      "com.typesafe.akka" %% "akka-actor" % "2.4.20" excludeAll (ExclusionRule(
        organization = "com.typesafe.akka",
        name = "akka-cluster_2.11"), ExclusionRule(organization = "com.typesafe.akka", name = "akka-remote_2.11")),
      "io.spray"          %% "spray-testkit"             % "1.3.4"  % "test",
      "com.typesafe.akka" %% "akka-testkit"              % "2.4.20" % "test",
      "io.spray"          %% "spray-routing-shapeless23" % "1.3.4"  % "test"
    ) ++ testDeps
  )

lazy val akkaClient = (project in file("akka-client"))
  .dependsOn(core)
  .settings(
    name := "jsonapi-scala-akka-client",
    scalaVersion := "2.11.11",
    crossScalaVersions := Seq("2.12.3", "2.11.11"),
    scalacOptions ++= {
      if (scalaVersion.value startsWith "2.11.") {
        scala211
      } else {
        scala212
      }
    },
    libraryDependencies ++= Seq(
      compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
      "com.typesafe.akka" %% "akka-stream"          % "2.5.4",
      "com.typesafe.akka" %% "akka-actor"           % "2.5.4",
      "com.typesafe.akka" %% "akka-http"            % "10.0.10",
      "com.typesafe.akka" %% "akka-http-spray-json" % "10.0.10"
    ) ++ testDeps
  )

lazy val root = (project in file("."))
  .aggregate(core, model, spray, akkaClient)
  .settings(
    publishArtifact := false,
    name := "jsonapi-scala",
    scalaVersion := "2.11.11"
  )
