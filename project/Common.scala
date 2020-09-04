import sbt._
import sbt.Keys._
import sbt.URL

object Common {

  val scala213               = "2.13.3"
  val scala212               = "2.12.10"
  val supportedScalaVersions = List(scala213, scala212)

  def apply() = Seq(
    autoCompilerPlugins := true,
    scalaVersion := scala213,
    organization := "com.engitano",
    organizationName := "Engitano",
    startYear := Some(2019),
    licenses += ("MIT", new URL("http://opensource.org/licenses/MIT")),
    crossScalaVersions := supportedScalaVersions,
    resolvers ++= Dependencies.resolvers(),
    libraryDependencies ++= Seq(
      Dependencies.collectionCompat,
      compilerPlugin("org.typelevel" % "kind-projector" % "0.11.0" cross CrossVersion.full)
    )
  )
}
