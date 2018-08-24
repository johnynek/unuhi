import sbt._

object Dependencies {
  lazy val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.13.4"
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.5"
  lazy val cats = "org.typelevel" %% "cats-core" % "1.2.0"
  lazy val catsLaws = "org.typelevel" %% "cats-laws" % "1.2.0"
  lazy val catsTestKit = "org.typelevel" %% "cats-testkit" % "1.1.0"
  lazy val fastparse = "com.lihaoyi" %% "fastparse" % "1.0.0"
}
