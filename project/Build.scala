/*
* This file is part of the ToolXiT project.
*
* Licensed under the Apache License = Value val Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing = Value val software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND = Value val either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package toolxit.build

import sbt._
import Keys._

import com.typesafe.sbt.SbtScalariform._
import scalariform.formatter.preferences._

object ToolXiTBuild extends Build {

  lazy val globalSettings = scalariformSettings ++ Seq(
    organization := "org.gnieh",
    version := "0.1.0-SNAPSHOT",
    resolvers += "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
    scalaVersion := "2.11.1",
    scalacOptions ++= Seq("-deprecation", "-feature")
  )

  lazy val root = Project(id = "toolxit", base = file(".")) settings(
    name := "toolxit"
  ) aggregate(core, eyes, mouth)

  lazy val scalariformSettings = defaultScalariformSettings ++ Seq(
    ScalariformKeys.preferences :=
      ScalariformKeys.preferences.value
        .setPreference(AlignSingleLineCaseStatements, true)
        .setPreference(DoubleIndentClassDeclaration, true)
        .setPreference(PreserveDanglingCloseParenthesis, true)
        .setPreference(MultilineScaladocCommentsStartOnFirstLine, true)
  )

  lazy val core =
    Project(id = "toolxit-core", base = file("core")) settings(globalSettings: _*) settings(
      libraryDependencies ++= Seq(
        "org.gnieh" %% "gnieh-pp" % "0.2-SNAPSHOT"
      )
    )

  lazy val eyes =
    Project(id = "toolxit-eyes", base = file("eyes")) settings(globalSettings: _*) dependsOn(core)

  lazy val mouth =
    Project(id = "toolxit-mouth", base = file("mouth")) settings(globalSettings: _*) dependsOn(eyes)

  lazy val stomach =
    Project(id = "toolxit-stomach", base = file("stomach")) settings(globalSettings: _*) dependsOn(mouth)

}
