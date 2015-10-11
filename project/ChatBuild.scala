import sbt._
import Keys._

import org.scalajs.sbtplugin.ScalaJSPlugin
import ScalaJSPlugin._
import autoImport._

import spray.revolver.RevolverPlugin._

object ChatBuild extends Build {
  lazy val root =
    Project("root", file("."))
      .aggregate(frontend, backend)

  // Scala-Js frontend
  lazy val frontend =
    Project("frontend", file("frontend"))
      .enablePlugins(ScalaJSPlugin)
      .settings(commonSettings: _*)
      .settings(
        emitSourceMaps := false,
        persistLauncher in Compile := true,
        persistLauncher in Test := false,
        jsDependencies ++= Seq(
          "org.webjars" % "react" % "0.12.2" / "react-with-addons.js" commonJSName "React" minified "react-with-addons.min.js"
        ),
        testFrameworks += new TestFramework("utest.runner.Framework"),
        libraryDependencies ++= Seq(
          "org.scala-js" %%% "scalajs-dom" % "0.8.0",
          "com.lihaoyi" %%% "upickle" % "0.2.8",
          "com.lihaoyi" %%% "utest" % "0.3.0" % "test",
          "com.github.chandu0101.scalajs-react-components" %%% "core" % "0.1.0",
          "com.github.japgolly.scalajs-react" %%% "core" % "0.9.2",
          "com.github.japgolly.scalajs-react" %%% "extra" % "0.9.2"
        )

      )
      .dependsOn(sharedJs)

  // Akka Http based backend
  lazy val backend =
    Project("backend", file("backend"))
      .settings(Revolver.settings: _*)
      .settings(commonSettings: _*)
      .settings(
        libraryDependencies ++= Seq(
          "com.typesafe.akka" %% "akka-http-experimental" % "1.0",
          "org.specs2" %% "specs2" % "2.3.12" % "test",
          "com.lihaoyi" %% "upickle" % "0.2.8"
        ),
        (resourceGenerators in Compile) <+=
          (fastOptJS in Compile in frontend, packageScalaJSLauncher in Compile in frontend)
            .map((f1, f2) => Seq(f1.data, f2.data)),
        watchSources <++= (watchSources in frontend)
      )
      .dependsOn(sharedJvm)

  lazy val shared = (crossProject.crossType(CrossType.Pure) in file ("shared")).
    settings(
      scalaVersion:=scalaV,
      libraryDependencies +="org.scala-js" %% "scalajs-stubs" % scalaJSVersion % "provided"
    )

  lazy val sharedJvm= shared.jvm
  lazy val sharedJs= shared.js

  lazy val scalaV = "2.11.6"

  def commonSettings = Seq(
    scalaVersion := scalaV
  ) ++ ScalariformSupport.formatSettings
}