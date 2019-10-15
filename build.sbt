name := "macro-test"

version := "0.1"

scalaVersion := "2.12.10"

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value % Test
libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % Test
