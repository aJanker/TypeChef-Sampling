
name := "Typechef-Sampling"

version := "1.0"

organization := "de.fosd.typechef"

scalaVersion := "2.11.4"

libraryDependencies += "de.fosd.typechef" % "frontend_2.11" % "0.4.1"

libraryDependencies += "gnu.getopt" % "java-getopt" % "1.0.13"

mainClass in Runtime := Some("de.fosd.typechef.Sampling")


//generate typechef.sh file with full classpath
TaskKey[File]("mkrun") <<= (baseDirectory, fullClasspath in Runtime, mainClass in Runtime) map { (base, cp, main) =>
  val template = """#!/bin/sh
java -ea -Xmx4096m -Xms128m -Xss10m -classpath "%s" %s "$@"
"""
  val mainStr = main getOrElse sys.error("No main class specified")
  val contents = template.format(cp.files.absString, mainStr)
  val out = base / "typechefsampling.sh"
  IO.write(out, contents)
  out.setExecutable(true)
  out
}



