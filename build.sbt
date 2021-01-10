import java.nio.file.Paths

import sbt.Keys._
import sbt.Resolver

val circeVersion = "0.12.3"

lazy val commonSettings = Seq(
  organization := "org.combinators",

  scalaVersion := "2.13.2",
  crossScalaVersions := Seq("2.11.12", scalaVersion.value),

  resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.typesafeRepo("releases"),
    Resolver.sonatypeRepo("snapshots"),
    Resolver.typesafeRepo("snapshots"),
    "apache-snapshot" at "https://repository.apache.org/snapshots/"
),

  scalacOptions ++= Seq(
    //"-unchecked",
    "-deprecation",
    "-feature",
    "-language:implicitConversions")
) ++ noPublishSettings

updateOptions := updateOptions.value.withLatestSnapshots(true)


lazy val root = (Project(id = "combinatory-trajectory-planning", base = file(".")))
  .settings(commonSettings: _*)
  .settings(
    moduleName := "cls-ctp",
    libraryDependencies ++= Seq(
      //"org.combinators" %% "cls-scala" % "2.1.0+7-9e42ea3e",
      "org.combinators" %% "cls-scala" % "2.2.0-TEMP",
      "org.scalactic" %% "scalactic" % "3.1.2" % "test",
      "org.scalatest" %% "scalatest" % "3.1.2" % "test",
      "org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0",
      "ch.qos.logback" % "logback-classic" % "1.2.3",
      "org.apache.poi" % "poi" % "3.9",
      "org.apache.poi" % "poi-ooxml" % "3.9",
      "com.lightbend.akka" %% "akka-stream-alpakka-mqtt" % "2.0.0-M1",
      "com.typesafe.akka" %% "akka-stream" % "2.5.31",
      "com.typesafe.scala-logging" %% "scala-logging" % "3.9.0",
      "org.scala-graph" %% "graph-core" % "1.13.2",
      "org.scala-graph" %% "graph-json" % "1.13.0",
      "org.eclipse.paho" % "org.eclipse.paho.client.mqttv3" % "1.2.0",
      "org.locationtech.jts" % "jts-core" % "1.17.1",
      "com.dreizak" % "miniball" % "1.0.3",
      "org.scalaz" %% "scalaz-core" % "7.2.27",
      "org.apache.commons" % "commons-math3" % "3.6.1",
      "org.apache.commons" % "commons-geometry-core" % "1.0-SNAPSHOT",
      "org.apache.commons" % "commons-geometry-euclidean" % "1.0-SNAPSHOT") ++

      //<module>commons-geometry-spherical</module>
      //<module>commons-geometry-hull</module>
      //<module>commons-geometry-enclosing</module>

      Seq(
        "io.circe" %% "circe-core",
        "io.circe" %% "circe-generic",
        "io.circe" %% "circe-parser"
      ).map(_ % circeVersion),
    unmanagedSources / excludeFilter := {
      val pf = (baseDirectory.value / "src" ) ** ("temp") ** ("*.scala" || "*.package")
      (s => pf.get().contains(s))
    },
    mainClass in (Compile, packageBin) := Some("org.combinators.ctp.repositories.benchmarks.RunBmClient")
  )
lazy val noPublishSettings = Seq(
  publish := Seq.empty,
  publishLocal := Seq.empty,
  publishArtifact := false
)
