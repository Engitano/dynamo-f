import sbt._
import sbt.Keys._
import sbt.URL

object Common {

  val kindProjector = "org.typelevel" % "kind-projector" % "0.10.3" cross CrossVersion.binary
  val scala213 = "2.13.0"
  // val scala212 = "2.12.8"
  val supportedScalaVersions = List(scala213)

  def apply() = Seq(
    scalaVersion := "2.13.0",
    organization := "com.engitano",
    organizationName := "Engitano",
    startYear := Some(2019),    
    licenses += ("MIT", new URL("http://opensource.org/licenses/MIT")),
    crossScalaVersions := supportedScalaVersions,
    resolvers ++= Dependencies.resolvers(),
    addCompilerPlugin(kindProjector),
    libraryDependencies += Dependencies.collectionCompat
  )
}
