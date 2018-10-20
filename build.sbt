import sbt._
import Keys._

import scalariform.formatter.preferences._

lazy val globalSettings = scalariform ++ Seq(
  organization := "toolxit",
  version := "0.1.0-SNAPSHOT",
  resolvers += "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
  scalaVersion := "2.12.7",
  scalacOptions ++= Seq("-deprecation", "-feature"),
  libraryDependencies += "com.lihaoyi" %% "sourcecode" % "0.1.5",
  libraryDependencies += "com.beachape" %% "enumeratum" % "1.5.13")

lazy val toolxit = project.in(file("."))
  .enablePlugins(ScalaUnidocPlugin)
  .settings(
    name := "toolxit")
  .settings(
    scalacOptions in (Compile, doc) ++= Seq("-doc-root-content", "rootdoc.txt", "-groups"))
  .settings(globalSettings: _*)
  .aggregate(core, fonts, math, eyes, mouth, stomach, xonsole)

lazy val scalariform = Seq(
  scalariformAutoformat := true,
  scalariformPreferences :=
    scalariformPreferences.value
      .setPreference(AlignSingleLineCaseStatements, true)
      .setPreference(DoubleIndentConstructorArguments, true)
      .setPreference(DanglingCloseParenthesis, Preserve)
      .setPreference(MultilineScaladocCommentsStartOnFirstLine, true))

lazy val core = project.in(file("core"))
  .settings(globalSettings: _*)
  .settings(
    name := "core")

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

lazy val fonts = project.in(file("fonts"))
  .settings(globalSettings: _*)
  .settings(
    name := "fonts",
    libraryDependencies += "org.scodec" %% "scodec-core" % "1.10.3")
  .dependsOn(core)

lazy val math = project.in(file("math"))
  .settings(globalSettings: _*)
  .settings(
    name := "math")
  .dependsOn(fonts)

lazy val xonsole = project.in(file("xonsole"))
  .settings(globalSettings: _*)
  .settings(
    name := "xonsole",
    libraryDependencies += "org.jline" % "jline" % "3.9.0")
  .dependsOn(stomach, fonts)
