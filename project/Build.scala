
import sbt._
import Keys._

object Settings {
  lazy val basicSettings = Seq[Setting[_]](
    scalaVersion := "2.11.5" 
    , scalacOptions := Seq("-deprecation", "-encoding", "utf8")
    , organization := ""
    , version := "0.1.0"
    , description := "ttfi"
    , resolvers ++=
      Seq(
        Resolver.sonatypeRepo("releases"),
        Resolver.sonatypeRepo("snapshots"),
        "bintray/non" at "http://dl.bintray.com/non/maven"
      )
  )
}

object deps {
  object v {
    val scalaz = "7.1.0"
    val nscalaTime = "1.2.0"
    val jodaTime = "2.3"
  }
  val scalaz = Seq(
    "org.scalaz" %% "scalaz-core" % v.scalaz,
    "org.scalaz" %% "scalaz-concurrent" % v.scalaz
  )
  val shapeless = Seq(
    "com.chuusai" %% "shapeless" % "2.0.0"
  )
  val shapeless_2_1_0 = Seq("com.chuusai" %% "shapeless" % "2.1.0")

  val argonaut = Seq("io.argonaut" %% "argonaut" % "6.1-M4")
  val time = Seq(
    "joda-time" % "joda-time" % v.jodaTime
    , "com.github.nscala-time" %% "nscala-time" % v.nscalaTime
  )
  val spire = Seq("org.spire-math" %% "spire" % "0.9.0")

  val machinist = Seq("org.typelevel" %% "machinist" % "0.2.2")

  val macroParadise = Seq(compilerPlugin("org.scalamacros" % "paradise_2.10.4" % "2.0.1"))
}

object ProjectBuild extends Build {

  import Settings._

  lazy val macros = Project("macros", file("macros"))
    .settings(basicSettings: _*)
    .settings(
      libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-reflect" % _),
      libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _),
      libraryDependencies := {
        CrossVersion.partialVersion(scalaVersion.value) match {
          // if Scala 2.11+ is used, quasiquotes are available in the standard distribution
          case Some((2, x)) if x >= 11 =>
            libraryDependencies.value
          // in Scala 2.10, quasiquotes are provided by macro paradise
          case Some((2, 10)) =>
            libraryDependencies.value ++ Seq(
              compilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full),
              "org.scalamacros" %% "quasiquotes" % "2.1.0-M5" cross CrossVersion.binary)
        }
      }
    )

  lazy val core = Project("ttfi", file("."))
    .settings(basicSettings: _*)
    .settings(
      projectDependencies ++= deps.scalaz ++ deps.argonaut ++ deps.time ++ deps.shapeless_2_1_0 ++ deps.spire
    )
   .settings(scalaSource in Compile := baseDirectory.value / "src" / "scala")
   .dependsOn(macros)
}
