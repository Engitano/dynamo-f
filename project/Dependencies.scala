import sbt._

object Dependencies {

  def resolvers(): Seq[MavenRepository] = Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.bintrayRepo("engitano", "maven")
  )

  val fs2              = "co.fs2"                     %% "fs2-core"                  % "2.2.0"
  val catsCore         = "org.typelevel"              %% "cats-core"                 % "2.2.0"
  val catsFree         = "org.typelevel"              %% "cats-free"                 % "2.2.0"
  val catsF            = "org.typelevel"              %% "cats-effect"               % "2.2.0"
  val shapeless        = "com.chuusai"                %% "shapeless"                 % "2.3.3"
  val dynamo           = "software.amazon.awssdk"     % "dynamodb"                   % "2.14.12"
  val refined          = "eu.timepit"                 %% "refined"                   % "0.9.8"
  val collectionCompat = "org.scala-lang.modules"     %% "scala-collection-compat"   % "2.1.1"
  val scalatest        = "org.scalatest"              %% "scalatest"                 % "3.0.8"
  val scalacheck       = "org.scalacheck"             %% "scalacheck"                % "1.14.0"
  val scalacheckAuto   = "com.github.alexarchambault" %% "scalacheck-shapeless_1.14" % "1.2.3"
}
