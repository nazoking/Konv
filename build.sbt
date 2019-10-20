name := "macro-test"

version := "0.1"

//scalaVersion := "2.12.10"
scalaVersion := "2.13.0"
crossScalaVersions := Seq("2.12.10", "2.13.0")

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value % Test
libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
libraryDependencies += "com.lihaoyi" %% "utest" % "0.7.1" % Test
testFrameworks += new TestFramework("utest.runner.Framework")

scalacOptions in ThisBuild ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Ywarn-macros:after",
  "-Xlint",
  "-Ywarn-unused:imports",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen")

scalacOptions += "-Ywarn-unused:imports,patvars,privates,locals,explicits,-implicits"
scalacOptions in Test += "-Ywarn-unused:imports,-patvars,-privates,-locals,-explicits,-implicits"

