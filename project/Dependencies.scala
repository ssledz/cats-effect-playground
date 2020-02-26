import sbt._

object Dependencies {

  val doobieVersion = "0.8.6"
  val catsVersion = "2.0.0"
  val catsEffectVersion = catsVersion
  val catsFreeVersion = catsVersion

  val fs2 = Seq(
    "fs2-core",
    "fs2-io"
  ).map("co.fs2" %% _ % "2.1.0")

  val doobie = Seq(
    "doobie-core",
    "doobie-hikari" // HikariCP transactor.
  ).map("org.tpolecat" %% _ % doobieVersion)

  val doobiePostgres = "org.tpolecat" %% "doobie-postgres" % doobieVersion // Postgres driver + type mappings.

  val doobieScalaTest = "org.tpolecat" %% "doobie-scalatest" % doobieVersion % Test // Postgres driver + type mappings.

  val doobieH2 = "org.tpolecat" %% "doobie-h2" % doobieVersion // H2 driver + type mappings.

  val http4s = Seq(
    "http4s-dsl",
    "http4s-blaze-server",
    "http4s-blaze-client"
  ).map("org.http4s" %% _ % "0.21.0-M6")

  val circe = Seq(
    "circe-core",
    "circe-generic",
    "circe-parser",
    "circe-generic-extras"
  ).map("io.circe" %% _ % "0.12.1")

  val jsoniter = Seq(
    ("jsoniter-scala-core", Compile),
    ("jsoniter-scala-macros", Provided) // required only in compile-time
  ).map {
    case (name, scope) =>
      "com.github.plokhotnyuk.jsoniter-scala" %% name % "2.0.2" % scope
  }

  val enumeratum = "com.beachape" %% "enumeratum" % "1.5.13"

  val enumeratumCirce = "com.beachape" %% "enumeratum-circe" % "1.5.22"

  val catsCore = "org.typelevel" %% "cats-core" % catsVersion

  val catsEffect = "org.typelevel" %% "cats-effect" % catsEffectVersion

  val catsFree = "org.typelevel" %% "cats-free" % catsFreeVersion

  val cats = Seq(catsCore, catsEffect, catsFree)

  val logback = "ch.qos.logback" % "logback-classic" % "1.2.3"

  val logging = "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2"

  val pureConfig = "com.github.pureconfig" %% "pureconfig" % "0.12.1"

  val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.14.2" % Test

  val scalaTest = "org.scalatest" %% "scalatest" % "3.0.8" % Test

  val mockitoScala = "org.mockito" %% "mockito-scala" % "1.7.1" % Test

  val tests = Seq(scalaTest, scalaCheck, mockitoScala)
}