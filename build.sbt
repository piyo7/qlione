name := "qlione"
version := "0.1.0"
javacOptions ++= Seq("-source", "1.8", "-target", "1.8")
scalaVersion := "2.12.4"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.5" % "test"
)
