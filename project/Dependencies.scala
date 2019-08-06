import sbt._

object Dependencies {

  def resolvers(): Seq[MavenRepository] = Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.bintrayRepo("engitano", "maven")
  )

  val fs2              = "co.fs2"                     %% "fs2-core"                  % "1.1.0-M1"
  val catsCore         = "org.typelevel"              %% "cats-core"                 % "2.0.0-M4"
  val catsF            = "org.typelevel"              %% "cats-effect"               % "2.0.0-M4"
  val shapeless        = "com.chuusai"                %% "shapeless"                 % "2.3.3"
  val dynamo           = "software.amazon.awssdk"     % "dynamodb"                   % "2.7.5"
  val refined          = "eu.timepit"                 %% "refined"                   % "0.9.8"
  val collectionCompat = "org.scala-lang.modules"     %% "scala-collection-compat"   % "2.1.1"
  val scalatest        = "org.scalatest"              %% "scalatest"                 % "3.0.8"
  val scalacheck       = "org.scalacheck"             %% "scalacheck"                % "1.14.0"
  val scalacheckAuto   = "com.github.alexarchambault" %% "scalacheck-shapeless_1.14" % "1.2.3"
}
