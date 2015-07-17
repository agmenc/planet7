import sbt._
import Keys._

object Build extends Build {

  def sharedSettings = Seq(
    name := "planet7",
    scalaVersion:= "2.11.7",
    incOptions := incOptions.value.withNameHashing(true),
    updateOptions := updateOptions.value.withCachedResolution(true),
    crossScalaVersions := Seq("2.10.5", "2.11.7"),
    scalacOptions += "-deprecation",
    resolvers += Resolver.sonatypeRepo("releases"),
    libraryDependencies ++= Seq(
      "junit" % "junit" % "4.11" % "test->default",
      "org.scalatest" %% "scalatest" % "2.1.3" % "test",
      "org.mockito" % "mockito-core" % "1.9.5" % "test",
      "com.github.tototoshi" %% "scala-csv" % "1.0.0" % "test",
      "net.sf.opencsv" % "opencsv" % "2.0" % "test"
    ),
    // add scala-xml dependency when needed (for Scala 2.11 and newer)
    // this mechanism supports cross-version publishing
    libraryDependencies := {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, scalaMajor)) if scalaMajor >= 11 => libraryDependencies.value :+ "org.scala-lang.modules" %% "scala-xml" % "1.0.4"
        case _ => libraryDependencies.value
      }
    }
  )

  lazy val main = Project(id = "main", base = file(".")).settings(sharedSettings: _*)
}
