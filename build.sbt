import Dependencies._


lazy val parserSettings = Seq(
      organization := "com.example",
      scalaVersion := "2.12.6",
      version      := "0.1.0-SNAPSHOT",
      scalacOptions += "-Ypartial-unification")

lazy val core = (project in file("core"))
  .settings(parserSettings: _*)
  .settings(
    name := "parser",
    libraryDependencies ++= Seq(
      cats,
      scalaCheck,
      fastparse,
      scalaTest % Test)
  )
  .disablePlugins(JmhPlugin)

lazy val bench = (project in file("bench"))
  .dependsOn(core)
  .settings(name := "parser-benchmark")
  .settings(parserSettings: _*)
  .enablePlugins(JmhPlugin)
