import ReleaseTransformations._
import sbtcrossproject.{crossProject, CrossType}
import com.typesafe.tools.mima.plugin.MimaPlugin.mimaDefaultSettings

lazy val catsVersion = "1.2.0"

lazy val noPublish = Seq(
  publish := {},
  publishLocal := {},
  publishArtifact := false)

lazy val unuhiSettings = Seq(
  organization := "org.bykn",
  scalaVersion := "2.12.6",
  crossScalaVersions := Seq("2.10.7", "2.11.12", "2.12.6", "2.13.0-M4"),
  version      := "0.1.0-SNAPSHOT",
  scalacOptions ++= (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, n)) =>
      val xs0 = if (n <= 12) List("-Xfatal-warnings", "-Yno-adapted-args") else Nil
      val xs1 = if (n >= 12) List("-Ypartial-unification") else Nil
      xs0 ::: xs1
    case _ =>
      Nil
  }),
  scalacOptions ++= Seq(
    "-deprecation",
    "-encoding", "UTF-8",
    "-feature",
    "-language:existentials",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-language:experimental.macros",
    "-unchecked",
    "-Xlint",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard",
    "-Xfuture"),
  // HACK: without these lines, the console is basically unusable,
  // since all imports are reported as being unused (and then become
  // fatal errors).
  scalacOptions in (Compile, console) ~= {_.filterNot("-Xlint" == _)},
  scalacOptions in (Test, console) := (scalacOptions in (Compile, console)).value,

  // release stuff
  releaseCrossBuild := true,
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := Function.const(false),
  releaseProcess := Seq[ReleaseStep](
    checkSnapshotDependencies,
    inquireVersions,
    runClean,
    runTest,
    setReleaseVersion,
    commitReleaseVersion,
    tagRelease,
    publishArtifacts,
    setNextVersion,
    commitNextVersion,
    releaseStepCommand("sonatypeReleaseAll"),
    pushChanges),
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("Snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("Releases" at nexus + "service/local/staging/deploy/maven2")
  },
  pomExtra := (
    <url>https://github.com/bykn/unuhi</url>
    <licenses>
      <license>
        <name>Apache 2</name>
        <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
        <distribution>repo</distribution>
        <comments>A business-friendly OSS license</comments>
      </license>
    </licenses>
    <scm>
      <url>git@github.com:bykn/unuhi.git</url>
      <connection>scm:git:git@github.com:bykn/unuhi.git</connection>
    </scm>
    <developers>
      <developer>
        <id>johnynek</id>
        <name>Oscar Boykin</name>
        <url>http://github.com/johnynek/</url>
      </developer>
    </developers>
  ),
  coverageMinimum := 60,
  coverageFailOnMinimum := false) ++ mimaDefaultSettings

def previousArtifact(version: String, proj: String) = {
  // the "-dbuild..." part is for Scala community build friendliness
  val regex = "0\\.([0-9]+)\\.[0-9]+(-SNAPSHOT|-dbuild[a-z0-9]*)?".r
  version match {
    case regex("1", _) => Set("org.bykn" %% s"unuhi-$proj" % "0.1.0")
    case regex("2", _) => Set.empty[ModuleID]
    case _ => throw new RuntimeException(s"Unexpected version: ${version}")
  }
}

lazy val commonJvmSettings = Seq(
  testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oDF"))

lazy val commonJsSettings = Seq(
  scalaJSStage in Global := FastOptStage,
  parallelExecution := false,
  jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv(),
  // batch mode decreases the amount of memory needed to compile scala.js code
  scalaJSOptimizerOptions := scalaJSOptimizerOptions.value.withBatchMode(scala.sys.env.get("TRAVIS").isDefined))

lazy val unuhi = project
  .in(file("."))
  .settings(name := "root")
  .settings(unuhiSettings: _*)
  .settings(noPublish: _*)
  .aggregate(unuhiJVM, unuhiJS)
  .dependsOn(unuhiJVM, unuhiJS)

lazy val unuhiJVM = project
  .in(file(".unuhiJVM"))
  .settings(moduleName := "unuhi")
  .settings(unuhiSettings)
  .settings(commonJvmSettings)
  .aggregate(coreJVM)
  .dependsOn(coreJVM)

lazy val unuhiJS = project
  .in(file(".unuhiJS"))
  .settings(moduleName := "unuhi")
  .settings(unuhiSettings)
  .settings(commonJsSettings)
  .aggregate(coreJS)
  .dependsOn(coreJS)
  .enablePlugins(ScalaJSPlugin)

lazy val core = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure)
  .in(file("core"))
  .settings(name := "unuhi-core")
  .settings(moduleName := "unuhi-core")
  .settings(unuhiSettings: _*)
  //.settings(mimaPreviousArtifacts := previousArtifact(version.value, "core"))
  .disablePlugins(JmhPlugin)
  .jsSettings(commonJsSettings:_*)
  .jsSettings(coverageEnabled := false)
  .jvmSettings(commonJvmSettings:_*)
  .platformsSettings(JVMPlatform, JSPlatform)(
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % catsVersion,
      "org.scalacheck" %%% "scalacheck" % "1.13.4" % Test,
      "org.scalatest" %%% "scalatest" % "3.0.5" % Test
    )
  )

lazy val coreJVM = core.jvm
lazy val coreJS = core.js

lazy val laws = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure)
  .in(file("laws"))
  .settings(name := "unuhi-laws")
  .settings(moduleName := "unuhi-laws")
  .settings(unuhiSettings: _*)
  //.settings(mimaPreviousArtifacts := previousArtifact(version.value, "laws"))
  .disablePlugins(JmhPlugin)
  .jsSettings(commonJsSettings:_*)
  .jsSettings(coverageEnabled := false)
  .jvmSettings(commonJvmSettings:_*)
  .platformsSettings(JVMPlatform, JSPlatform)(
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % catsVersion,
      "org.typelevel" %%% "cats-laws" % catsVersion,
      "org.typelevel" %%% "cats-testkit" % catsVersion,
      "org.scalacheck" %%% "scalacheck" % "1.13.4" % Test,
      "org.scalatest" %%% "scalatest" % "3.0.5" % Test
    )
  )
  .dependsOn(core)

lazy val lawsJVM = laws.jvm
lazy val lawsJS = laws.js

lazy val fastparse = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure)
  .in(file("fastparse"))
  .settings(name := "unuhi-fastparse")
  .settings(moduleName := "unuhi-fastparse")
  .settings(unuhiSettings: _*)
  //.settings(mimaPreviousArtifacts := previousArtifact(version.value, "fastparse"))
  .disablePlugins(JmhPlugin)
  .jsSettings(commonJsSettings:_*)
  .jsSettings(coverageEnabled := false)
  .jvmSettings(commonJvmSettings:_*)
  .platformsSettings(JVMPlatform, JSPlatform)(
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % catsVersion,
      "com.lihaoyi" %%% "fastparse" % "1.0.0",
      "org.scalacheck" %%% "scalacheck" % "1.13.4" % Test,
      "org.scalatest" %%% "scalatest" % "3.0.5" % Test
    )
  )
  .dependsOn(core)

lazy val fastparseJVM = fastparse.jvm
lazy val fastparseJS = fastparse.js

lazy val bench = project.in(file("bench"))
  .dependsOn(coreJVM, fastparseJVM)
  .settings(name := "unuhi-benchmark")
  .settings(unuhiSettings: _*)
  .settings(noPublish: _*)
  .settings(coverageEnabled := false)
  .enablePlugins(JmhPlugin)
