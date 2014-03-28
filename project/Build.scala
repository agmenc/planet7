import sbt._
import Keys._
import xerial.sbt.Sonatype.SonatypeKeys._

object Build extends Build {

  def sharedSettings = Seq(
    name := "planet7",
    scalaVersion:= "2.10.3",
    crossScalaVersions := Seq("2.10.3"),
    scalacOptions += "-deprecation",
    libraryDependencies ++= Seq(
      "junit" % "junit" % "4.11" % "test->default",
      "org.scalatest" % "scalatest_2.10" % "2.0" % "test"
    )
  )

  lazy val main = Project(id = "main", base = file(".")).settings(sharedSettings: _*)
}
