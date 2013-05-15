
import sbt._
import Keys._

object SimPuzzleBuild extends Build {

 override def settings = super.settings ++ Seq(scalaVersion := "2.10.1")

 lazy val simpuzzle = Project(id = "simpuzzle", base = file("simpuzzle"))

 lazy val simpoplocal = Project(id = "simpoplocal", base = file("simpoplocal")) dependsOn(simpuzzle)

 lazy val marius = Project(id = "marius", base = file("marius")) dependsOn (simpuzzle)

}



