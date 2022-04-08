val ZIOVersion = "2.0.0-RC4"

ThisBuild / scalaVersion     := "3.1.2"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "io.iog"
ThisBuild / organizationName := "input-output"

lazy val root = (project in file("."))
  .settings(
    name := "minichain",
  )

libraryDependencies ++= Seq(
  "dev.zio"       %% "zio"          % ZIOVersion,
  "dev.zio"       %% "zio-test"     % ZIOVersion % Test,
  "dev.zio"       %% "zio-test-sbt" % ZIOVersion % Test,
  "org.scalactic" %% "scalactic"    % "3.2.11",
)

testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))
