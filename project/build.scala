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

import sbt._
import sbt.Keys._
import sbtassembly.Plugin._
import sbtassembly.Plugin.AssemblyKeys._
import sbtrelease.ReleasePlugin._

object SaddleBuild extends sbt.Build {

  lazy val root =
    project(id = "saddle",
            settings = Seq(
              /* 'console' in root acts as if in core. */
              console <<= (console in core in Compile) { identity },
              assembleArtifact in packageScala := false,
              publishArtifact := false,
              mergeStrategy in assembly := {
                case "META-INF/MANIFEST.MF" | "META-INF/LICENSE" | "META-INF/BCKEY.DSA" => MergeStrategy.discard
                case _ => MergeStrategy.first
              }
            ),
            base = file(".")) aggregate(core, hdf5, test_framework)

  lazy val core =
    project(id = "saddle-core",
            base = file("saddle-core"),
            settings = Seq(
              initialCommands := """
                |import org.joda.time.DateTime
                |import org.saddle._
                |import org.saddle.time._
                |import org.saddle.io._""".stripMargin('|'),
              unmanagedClasspath in(LocalProject("saddle-core"), Test) <++= (fullClasspath in(LocalProject("saddle-test-framework"), Test)),
              libraryDependencies <++= scalaVersion (v => Seq(
                "joda-time" % "joda-time" % "2.1",
                "org.joda" % "joda-convert" % "1.2",
                "org.scala-saddle" % "google-rfc-2445" % "20110304",
                "com.googlecode.efficient-java-matrix-library" % "ejml" % "0.19",
                "org.apache.commons" % "commons-math" % "2.2",
                "it.unimi.dsi" % "fastutil" % "6.5.4",
                "it.unimi.dsi" % "dsiutils" % "2.0.15"
              ) ++ Shared.testDeps(v)),
              testOptions in Test += Tests.Argument("console", "junitxml")
            ))

  lazy val hdf5 =
    project(id = "saddle-hdf5",
            base = file("saddle-hdf5"),
            settings = Seq(
              initialCommands := """
                |import org.joda.time.DateTime
                |import org.saddle._
                |import org.saddle.time._
                |import org.saddle.io._""".stripMargin('|'),
              libraryDependencies <++= scalaVersion (v => Seq(
                "org.scala-saddle" % "jhdf5" % "2.9"
              ) ++ Shared.testDeps(v)),
              testOptions in Test += Tests.Argument("console", "junitxml")
            )) dependsOn(core)

  lazy val test_framework =
    project(
      id = "saddle-test-framework",
      base = file("saddle-test-framework"),
      settings = Seq(
        libraryDependencies <++= scalaVersion(v => Shared.testDeps(v, "compile"))
      )
    ) dependsOn (core)

  def project(id: String, base: File, settings: Seq[Project.Setting[_]] = Nil) =
    Project(id = id,
            base = base,
            settings = assemblySettings ++ Project.defaultSettings ++ Shared.settings ++ releaseSettings ++ settings)
}

object Shared {
  def testDeps(version: String, conf: String = "test") = {
    val specs2 = if (version.startsWith("2.1"))
      "org.specs2" %% "specs2" % "2.3.9"
    else if (version.startsWith("2.9.3"))
      "org.specs2" % "specs2_2.9.2" % "1.12.4"
    else
      "org.specs2" %% "specs2" % "1.12.4"

    val scalacheck = if (version.startsWith("2.9"))
      "org.scalacheck" %% "scalacheck" % "1.10.1"
    else
      "org.scalacheck" %% "scalacheck" % "1.11.3"

    Seq(
      specs2 % conf,
      scalacheck % conf,
      "junit" % "junit" % "4.11" % conf
    )
  }

  val settings = Seq(
    organization := "org.scala-saddle",
    publishMavenStyle := true,
    publishArtifact in Test := false,
    pomIncludeRepository := { x => false },
    pomExtra := (
      <url>http://saddle.github.io/</url>
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
      </developers>
    ),
    scalaVersion := "2.10.3",
    version := "1.3.4-SNAPSHOT",
    crossScalaVersions := Seq("2.9.2", "2.9.3", "2.10.3", "2.11.0-RC1"),
    scalacOptions := Seq("-deprecation", "-unchecked"), // , "-Xexperimental"),
    shellPrompt := { (state: State) => "[%s]$ " format(Project.extract(state).currentProject.id) },
    resolvers ++= Seq(
      "Sonatype OSS Releases" at "http://oss.sonatype.org/content/repositories/releases/",
      "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
    ),
    publishTo <<= (version) { version: String =>
      val nexus = "https://oss.sonatype.org/"
      if (version.trim.endsWith("SNAPSHOT"))
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases" at nexus + "service/local/staging/deploy/maven2")
    },
    credentials += Credentials(Path.userHome / ".ivy2" / ".credentials"),
    compile <<= (compile in Compile) dependsOn (compile in Test)
  )
}
