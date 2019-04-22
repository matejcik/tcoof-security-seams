name := "tcof-security-seams"

version := "1.0"

scalaVersion := "2.12.8"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % "2.12.8",
  "org.choco-solver" % "choco-solver" % "4.10.0",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test",
)
