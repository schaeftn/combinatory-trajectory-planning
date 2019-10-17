import sbt.Keys._
import sbt.Resolver

val circeVersion = "0.11.1"

lazy val commonSettings = Seq(
  organization := "org.combinators",

  scalaVersion := "2.12.6",
  crossScalaVersions := Seq("2.11.12", scalaVersion.value),

  resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.typesafeRepo("releases"),
    Resolver.sonatypeRepo("snapshots"),
    Resolver.typesafeRepo("snapshots")
  ),
  
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),

  scalacOptions ++= Seq(
    "-unchecked",
    "-deprecation",
    "-feature",
    "-language:implicitConversions"
  )
) ++ noPublishSettings


lazy val root = (Project(id = "combinatory-trajectory-planning", base = file(".")))
  .settings(commonSettings: _*)
  .settings(
    moduleName := "cls-graph-search",
    libraryDependencies ++= Seq(
      "org.combinators" %% "cls-scala" % "2.1.0+7-9e42ea3e",
      "org.scalactic" %% "scalactic" % "3.0.5" % "test",
      "org.scalatest" %% "scalatest" % "3.0.5" % "test",
      "ch.qos.logback" % "logback-classic" % "1.2.3",
      "org.apache.poi" % "poi" % "3.9",
      "org.apache.poi" % "poi-ooxml" % "3.9",
      "com.typesafe.scala-logging" %% "scala-logging" % "3.9.0",
      "org.scala-graph" %% "graph-core" % "1.12.5",
      "org.eclipse.paho" % "org.eclipse.paho.client.mqttv3" % "1.2.0",
      "com.dreizak" % "miniball" % "1.0.3",
      "org.scalaz" %% "scalaz-core" % "7.2.27",
      "org.apache.commons" % "commons-math3" % "3.6.1") ++
      Seq(
        "io.circe" %% "circe-core",
        "io.circe" %% "circe-generic",
        "io.circe" %% "circe-parser"
      ).map(_ % circeVersion)
  )

lazy val noPublishSettings = Seq(
  publish := Seq.empty,
  publishLocal := Seq.empty,
  publishArtifact := false
)
