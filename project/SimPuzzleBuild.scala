
import sbt._
import Keys._

object SimPuzzleBuild extends Build {

 override def settings = super.settings ++ Seq(scalaVersion := "2.10.1")

 lazy val simpuzzle = Project(id = "simpuzzle", base = file("simpuzzle"))

 lazy val marius = Project(id = "marius", base = file("marius")) dependsOn (simpuzzle)
 
 lazy val simpoplocal = Project(id = "simpoplocal", base = file("simpoplocal")) dependsOn(simpuzzle)

 lazy val schelling = Project(id = "schelling", base = file("schelling")) dependsOn(simpuzzle)

 lazy val all = Project(id = "all", base = file("")) dependsOn(simpuzzle, marius, simpoplocal, schelling)
}



