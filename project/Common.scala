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
    ),
    scalacOptions ++= Seq(
        // "-deprecation", // Emit warning and location for usages of deprecated APIs.
        "-encoding",
        "utf-8",                  // Specify character encoding used by source files.
        "-explaintypes",          // Explain type errors in more detail.
        "-feature",               // Emit warning and location for usages of features that should be imported explicitly.
        "-language:existentials", // Existential types (besides wildcard types) can be written and inferred
        "-language:experimental.macros",   // Allow macro definition (besides implementation and application). Disabled, as this will significantly change in Scala 3
        "-language:higherKinds", // Allow higher-kinded types
        "-language:implicitConversions",   // Allow definition of implicit functions called views. Disabled, as it might be dropped in Scala 3. Instead use extension methods (implemented as implicit class Wrapper(val inner: Foo) extends AnyVal {}
        "-unchecked",                    // Enable additional warnings where generated code depends on assumptions.
        "-Xcheckinit",                   // Wrap field accessors to throw an exception on uninitialized access.
        "-Xlint:adapted-args",           // Warn if an argument list is modified to match the receiver.
        "-Xlint:constant",               // Evaluation of a constant arithmetic expression results in an error.
        "-Xlint:delayedinit-select",     // Selecting member of DelayedInit.
        "-Xlint:doc-detached",           // A Scaladoc comment appears to be detached from its element.
        "-Xlint:inaccessible",           // Warn about inaccessible types in method signatures.
        "-Xlint:infer-any",              // Warn when a type argument is inferred to be `Any`.
        "-Xlint:missing-interpolator",   // A string literal appears to be missing an interpolator id.
        "-Xlint:nullary-unit",           // Warn when nullary methods return Unit.
        "-Xlint:option-implicit",        // Option.apply used implicit view.
        "-Xlint:package-object-classes", // Class or object defined in package object.
        "-Xlint:poly-implicit-overload", // Parameterized overloaded implicit methods are not visible as view bounds.
        "-Xlint:private-shadow",         // A private field (or class parameter) shadows a superclass field.
        "-Xlint:stars-align",            // Pattern sequence wildcard must align with sequence component.
        "-Xlint:type-parameter-shadow",  // A local type parameter shadows a type already in scope.
        "-Ybackend-parallelism",
        "8",                                         // Enable paralellisation — change to desired number!
        "-Ycache-plugin-class-loader:last-modified", // Enables caching of classloaders for compiler plugins
        "-Ycache-macro-class-loader:last-modified"   // and macro definitions. This can lead to performance improvements.
      )
  )
}
