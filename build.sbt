import sbt.configs

// resolvers += Resolver.sonatypeRepo("releases")

val majorVersion = SettingKey[String]("major version")
val minorVersion = SettingKey[String]("minor version")
val patchVersion = SettingKey[Option[String]]("patch version")

Global / majorVersion := "0"
Global / minorVersion := "1"
Global / patchVersion := Some("0")

val writeVersion = taskKey[Unit]("Writes the version to version.txt")
writeVersion := {
  IO.write(baseDirectory.value / "version.txt", (`dynamo-f` / version).value)
}

test in publish := {}

lazy val root = (project in file("."))
  .settings(scalaVersion := "2.13.0")
  .aggregate(`dynamo-f`,`dynamo-f-formats`)

lazy val `dynamo-f-formats` = (project in file("formats"))
  .settings(
    Common(),
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
    bintrayPackageLabels := Seq("dynamodb", "fs2", "cats")
  )

lazy val `dynamo-f` = (project in file("core"))
  .configs(IntegrationTest)
  .settings(
    Common(),
    version := s"${majorVersion.value}.${minorVersion.value}${patchVersion.value.fold("")(p => s".$p")}",
    libraryDependencies ++= Seq(
      Dependencies.catsF,
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

addCommandAlias("fullBuild", ";project root;clean;coverage;test;it:test;coverageReport")
