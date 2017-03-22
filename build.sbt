import sbt._
import Keys._

import com.typesafe.sbt.SbtScalariform._
import scalariform.formatter.preferences._

import sbtunidoc.Plugin._

lazy val globalSettings = scalariform ++ Seq(
  organization := "toolxit",
  version := "0.1.0-SNAPSHOT",
  resolvers += "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
  scalaVersion := "2.12.1",
  scalacOptions ++= Seq("-deprecation", "-feature"),
  libraryDependencies += "io.iteratee" %% "iteratee-core" % "0.9.0")

lazy val toolxit = project.in(file("."))
  .settings(
    name := "toolxit")
  .settings(unidocSettings: _*)
  .settings(
    scalacOptions in (Compile, doc) ++= Seq("-doc-root-content", "rootdoc.txt", "-groups"))
  .settings(globalSettings: _*)
  .aggregate(core, eyes, mouth, stomach, xonsole)

lazy val scalariform = scalariformSettings ++ Seq(
  ScalariformKeys.preferences :=
    ScalariformKeys.preferences.value
      .setPreference(AlignSingleLineCaseStatements, true)
      .setPreference(DoubleIndentClassDeclaration, true)
      .setPreference(PreserveDanglingCloseParenthesis, true)
      .setPreference(MultilineScaladocCommentsStartOnFirstLine, true))

lazy val core = project.in(file("core"))
  .settings(globalSettings: _*)
  .settings(
    name := "core",
    libraryDependencies ++= Seq(
      "org.gnieh" %% "tekstlib" % "0.1.0-SNAPSHOT"))

lazy val eyes = project.in(file("eyes"))
  .settings(globalSettings: _*)
  .settings(
    name := "eyes")
  .dependsOn(core)

lazy val mouth = project.in(file("mouth"))
  .settings(globalSettings: _*)
  .settings(
    name := "mouth")
  .dependsOn(eyes)

lazy val stomach = project.in(file("stomach"))
  .settings(globalSettings: _*)
  .settings(
    name := "stomach")
  .dependsOn(mouth)

lazy val xonsole = project.in(file("xonsole"))
  .settings(globalSettings: _*)
  .settings(
    name := "xonsole",
    libraryDependencies += "org.jline" % "jline" % "3.2.0")
  .dependsOn(stomach)
