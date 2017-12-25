/**
  * Copyright (c) 2013 Saddle Development Team
  *
  * Licensed under the Apache License, Version 2.0 (the "License");
  * you may not use this file except in compliance with the License.
  * You may obtain a copy of the License at
  *
  *     http://www.apache.org/licenses/LICENSE-2.0
  *
  * Unless required by applicable law or agreed to in writing, software
  * distributed under the License is distributed on an "AS IS" BASIS,
  * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  * See the License for the specific language governing permissions and
  * limitations under the License.
 **/
import sbt.Keys._
import sbt._

object Shared {

  def testDeps(cfg: Configuration = Test) = Seq(
    "org.specs2" %% "specs2-core"       % "3.8.6" % cfg,
    "org.specs2" %% "specs2-scalacheck" % "3.8.6" % cfg,
    "org.specs2" %% "specs2-junit"      % "3.8.6" % cfg
  )

  val settings = Seq(
    organization := "org.saddle",
    publishMavenStyle := true,
    publishArtifact in Test := false,
    pomIncludeRepository := { _ =>
      false
    },
    pomExtra := <url>http://saddle.github.io/</url>
      <licenses>
        <license>
          <name>Apache 2.0</name>
          <url>http://www.apache.org/licenses/LICENSE-2.0.html</url>
          <distribution>repo</distribution>
        </license>
      </licenses>
      <scm>
        <url>git@github.com:saddle/saddle.git</url>
        <connection>scm:git:git@github.com:saddle/saddle.git</connection>
      </scm>
      <developers>
        <developer>
          <id>adamklein</id>
          <name>Adam Klein</name>
          <url>http://blog.adamdklein.com</url>
        </developer>
        <developer>
          <id>chrislewis</id>
          <name>Chris Lewis</name>
          <email>chris@thegodcode.net</email>
          <url>http://www.thegodcode.net/</url>
          <organizationUrl>https://www.novus.com/</organizationUrl>
          <timezone>-5</timezone>
        </developer>
        <developer>
          <id>hntd187</id>
          <name>Stephen Carman</name>
          <email>shcarman@gmail.com</email>
        </developer>
      </developers>,
    scalaVersion := "2.12.4",
    crossScalaVersions := Seq("2.11.12"),
    scalacOptions := Seq(
      "-deprecation",
      "-encoding",
      "utf-8",
      "-explaintypes",
      "-feature",
      "-language:existentials",
      "-language:experimental.macros",
      "-language:higherKinds",
      "-language:implicitConversions",
      "-language:reflectiveCalls",
      "-language:postfixOps",
      "-unchecked"
    ),
    shellPrompt := { (state: State) =>
      "[%s]$ ".format(Project.extract(state).currentProject.id)
    },
    resolvers ++= Seq(
      "Sonatype OSS Releases" at "http://oss.sonatype.org/content/repositories/releases/",
      "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
    ),
    publishTo := {
      val nexus = "https://oss.sonatype.org"
      if (version.value.trim.endsWith("SNAPSHOT")) {
        Some("snapshots" at s"$nexus/content/repositories/snapshots")
      } else {
        Some("releases" at s"$nexus/service/local/staging/deploy/maven2")
      }
    },
    credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")
  )
}
