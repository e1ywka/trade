name := "waves-test"

version := "0.1"

scalaVersion := "2.12.3"


libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.5.4",
  "com.typesafe.akka" %% "akka-testkit" % "2.5.4" % Test,

  "org.scalatest" %% "scalatest" % "3.0.1" % Test
)

