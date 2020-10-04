import sbt.configs

// resolvers += Resolver.sonatypeRepo("releases")

val majorVersion = SettingKey[String]("major version")
val minorVersion = SettingKey[String]("minor version")
val patchVersion = SettingKey[Option[String]]("patch version")

Global / majorVersion := "0"
Global / minorVersion := "2"
Global / patchVersion := Some("0")

val writeVersion = taskKey[Unit]("Writes the version to version.txt")
writeVersion := {
  IO.write(baseDirectory.value / "version.txt", (`dynamo-f` / version).value)
}

test in publish := {}

lazy val root = (project in file("."))
  .settings(Common())
  .settings(
    version := s"${majorVersion.value}.${minorVersion.value}${patchVersion.value.fold("")(p => s".$p")}",
    skip in publish := true
  )
  .aggregate(`dynamo-f`, `dynamo-f-formats`)

lazy val `dynamo-f-formats` = (project in file("formats"))
  .settings(
    Common(),
    // addCompilerPlugin("io.tryp" % "splain" % "0.4.1" cross CrossVersion.patch),
    version := s"${majorVersion.value}.${minorVersion.value}${patchVersion.value.fold("")(p => s".$p")}",
    libraryDependencies ++= Seq(
      Dependencies.catsCore,
      Dependencies.shapeless,
      Dependencies.dynamo,
      Dependencies.refined,
      Dependencies.scalatest      % "test",
      Dependencies.scalacheck     % "test",
      Dependencies.scalacheckAuto % "test"
    ),
    bintrayOrganization := Some("engitano"),
    bintrayPackageLabels := Seq("dynamodb", "fs2", "cats"),
    addCompilerPlugin("io.tryp" % "splain" % "0.5.7" cross CrossVersion.patch)
  )

lazy val `dynamo-f`: Project = (project in file("core"))
  .configs(IntegrationTest)
  .settings(
    Common(),
    version := s"${majorVersion.value}.${minorVersion.value}${patchVersion.value.fold("")(p => s".$p")}",
    libraryDependencies ++= Seq(
      Dependencies.catsF,
      Dependencies.catsFree,
      Dependencies.shapeless,
      Dependencies.dynamo,
      Dependencies.scalatest % "test, it"
    ),
    bintrayOrganization := Some("engitano"),
    bintrayPackageLabels := Seq("dynamodb", "fs2", "cats"),
    Defaults.itSettings ++ headerSettings(IntegrationTest) ++ automateHeaderSettings(IntegrationTest),
    parallelExecution in IntegrationTest := false,
    Dynamo.settings
  )
  .dependsOn(`dynamo-f-formats`)

addCommandAlias("fullBuild", ";project root;clean;coverage;test;it:test;coverageAggregate")

