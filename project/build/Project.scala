import sbt._
class Project(info:ProjectInfo) extends DefaultProject(info){
    val scalaToolsRepo = "Scala Tools Repo" at "http://nexus-direct.scala-tools.org/content/repositories/releases"
    val cglib = "cglib"%"cglib"%"2.2"
    val specs = "org.scala-tools.testing" % "specs_2.8.1" % "1.6.7"
}
