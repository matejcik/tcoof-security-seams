name := "tcof-security"

version := "1.0"

scalaVersion := "2.12.8"

compileOrder := CompileOrder.Mixed

resolvers += "SnakeYAML repository" at "http://oss.sonatype.org/content/groups/public/"

libraryDependencies ++= Seq(

  // Required for mpmens
  "org.scala-lang" % "scala-reflect" % "2.12.8",
  "org.choco-solver" % "choco-solver" % "4.10.0",

  // Required for map2d trait
  "de.ummels" %% "scala-prioritymap" % "1.0.0",

  // parsing yaml
  "org.yaml" % "snakeyaml" % "1.24-SNAPSHOT"
)
