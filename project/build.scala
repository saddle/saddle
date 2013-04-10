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
import Keys._
import sbtassembly.Plugin._
import sbtassembly.Plugin.AssemblyKeys._

object SaddleBuild extends sbt.Build {

  lazy val root =
    project(id = "saddle",
            base = file("."),
            settings = Seq(
              initialCommands := "import org.joda.time.DateTime; import org.saddle._; import org.saddle.time._; import org.saddle.io._",
              libraryDependencies <++= scalaVersion (v => Seq(
                "joda-time" % "joda-time" % "2.1",
                "org.joda" % "joda-convert" % "1.2",
                "com.googlecode.efficient-java-matrix-library" % "ejml" % "0.19",
                "org.apache.commons" % "commons-math" % "2.2",
                "it.unimi.dsi" % "fastutil" % "6.5.2",
                "it.unimi.dsi" % "dsiutils" % "2.0.15",
                "org.scala-saddle" % "jhdf5" % "2.9"
              ) ++ Shared.testDeps),
              jarName in assembly <<= version { v => "saddle-%s.jar" format (v) },
              assembleArtifact in packageScala := false,
              mergeStrategy in assembly := {
                case "META-INF/MANIFEST.MF" | "META-INF/LICENSE" | "META-INF/BCKEY.DSA" => MergeStrategy.discard
                case _ => MergeStrategy.first
              },
              testOptions in Test += Tests.Argument("console", "junitxml")
            ))

  def project(id: String, base: File, settings: Seq[Project.Setting[_]] = Nil) =
    Project(id = id,
            base = base,
            settings = assemblySettings ++ Project.defaultSettings ++ Shared.settings ++ settings)
}

object Shared {
  /** Resolve specs version for the current scala version (thanks @n8han). */
  val testDeps = Seq(
    "org.specs2" %% "specs2" % "1.12.3" % "test",
    "org.scalacheck" %% "scalacheck" % "1.10.0" % "test",
    "junit" % "junit" % "4.7" % "test"
  )

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
      </developers>
    ),
    version := "1.1.0-SNAPSHOT",
    scalaVersion := "2.9.2",
    crossScalaVersions := Seq("2.9.2", "2.10.0"),
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
    credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")
  )
}


