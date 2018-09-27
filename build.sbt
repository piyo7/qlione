organization := "com.github.piyo7"
name := "qlione"
version := "0.1.0-SNAPSHOT"
javacOptions ++= Seq("-source", "1.8", "-target", "1.8")
scalaVersion := "2.12.4"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

publishTo := Some(Resolver.file("qlione", file("maven")))
