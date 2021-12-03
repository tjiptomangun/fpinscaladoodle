import sbt._

//https://stackoverflow.com/questions/44426150/unable-to-resolve-org-scalatestscalatest3-0-1-not-found-dependency-issue
object Dependencies {
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.2.8"
  //lazy val scalactic = "org.scalactic" % "scalactic_2.13" % "3.2.9"
  lazy val scalactic = "org.scalactic" %% "scalactic" % "3.2.9"
  //lazy val scalaCheck  = "org.scalacheck" % "scalacheck_2.13" % "1.14.1"
  lazy val scalaCheck  = "org.scalacheck" %% "scalacheck" % "1.14.1"
}
