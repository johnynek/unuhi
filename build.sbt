import Dependencies._


lazy val parserSettings = Seq(
      organization := "org.bykn",
      scalaVersion := "2.12.6",
      version      := "0.1.0-SNAPSHOT",
      scalacOptions += "-Ypartial-unification")

lazy val core = (project in file("core"))
  .settings(parserSettings: _*)
  .settings(
    name := "unuhi-core",
    libraryDependencies ++= Seq(
      cats,
      scalaCheck % Test,
      scalaTest % Test)
  )
  .disablePlugins(JmhPlugin)

lazy val fastparse = (project in file("fastparse"))
  .settings(parserSettings: _*)
  .settings(
    name := "unuhi-fastparse",
    libraryDependencies ++= Seq(
      cats,
      Dependencies.fastparse,
      scalaCheck % Test,
      scalaTest % Test)
  )
  .dependsOn(core)
  .disablePlugins(JmhPlugin)


lazy val bench = (project in file("bench"))
  .dependsOn(core, fastparse)
  .settings(name := "unuhi-benchmark")
  .settings(parserSettings: _*)
  .enablePlugins(JmhPlugin)
