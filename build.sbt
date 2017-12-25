import Shared._

lazy val root = project
  .in(file("."))
  .settings(
    assembleArtifact in assemblyPackageScala := false,
    publishArtifact := false,
    assemblyMergeStrategy in assembly := {
      case "META-INF/MANIFEST.MF" | "META-INF/LICENSE" | "META-INF/BCKEY.DSA" => MergeStrategy.discard
      case _                                                                  => MergeStrategy.first
    }
  )
  .aggregate(core, test_framework)

lazy val core = project
  .in(file("saddle-core"))
  .settings(
    initialCommands := """
                         |import org.joda.time.DateTime
                         |import org.saddle._
                         |import org.saddle.time._
                         |import org.saddle.io._""".stripMargin('|'),
    libraryDependencies ++= Seq(
      "joda-time"                                    % "joda-time"       % "2.1",
      "org.joda"                                     % "joda-convert"    % "1.2",
      "org.scala-saddle"                             % "google-rfc-2445" % "20110304",
      "com.googlecode.efficient-java-matrix-library" % "ejml"            % "0.19",
      "org.apache.commons"                           % "commons-math"    % "2.2",
      "it.unimi.dsi"                                 % "fastutil"        % "6.5.4",
      "it.unimi.dsi"                                 % "dsiutils"        % "2.0.15"
    ) ++ Shared.testDeps(),
    testOptions in Test += Tests.Argument("console", "junitxml")
  ).settings(settings)

lazy val test_framework = project
  .in(file("saddle-test-framework"))
  .settings(libraryDependencies ++= testDeps(Compile))
    .settings(settings)
  .dependsOn(core)

lazy val reborn = project.in(file("reborn"))
  .settings(
    organization := "saddle",
    libraryDependencies ++= Seq(
      "org.scalanlp" %% "breeze" % "0.13.2",
      "org.scalanlp" %% "breeze-natives" % "0.13.2"
    )
  )

lazy val reborn_macros = project.in(file("macros"))
  .settings(
    organization := "saddle",
    libraryDependencies ++= Seq(
      "org.scalameta" %% "scalameta" % "2.1.2"
    )
  )