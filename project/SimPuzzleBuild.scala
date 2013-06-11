
import sbt._
import Keys._
import com.typesafe.sbt.osgi.OsgiKeys._
import com.typesafe.sbt.osgi.SbtOsgi._

object SimPuzzleBuild extends Build {

 override def settings = 
   super.settings ++ Seq(
     scalaVersion := "2.10.1",
     organization := "fr.geocite"
   )

  lazy val globalSettings = Seq(
     publishTo <<= isSnapshot(if(_) Some("Openmole Nexus" at "http://maven.iscpif.fr/snapshots") else Some("Openmole Nexus" at "http://maven.iscpif.fr/releases")),
     credentials += Credentials(Path.userHome / ".sbt" / "iscpif.credentials")
   )


 lazy val simpuzzle = Project(id = "simpuzzle", base = file("simpuzzle")) settings (globalSettings: _*)

 lazy val marius = Project(id = "marius", base = file("marius")) dependsOn (simpuzzle, gibrat) settings (globalSettings: _*)
 
 lazy val simpoplocal = Project(id = "simpoplocal", base = file("simpoplocal")) dependsOn(simpuzzle) settings (globalSettings: _*)

 lazy val schelling = Project(id = "schelling", base = file("schelling")) dependsOn(simpuzzle) settings (globalSettings: _*)

 lazy val gibrat = Project(id = "gibrat", base = file("gibrat")) dependsOn(simpuzzle) settings (globalSettings: _*)

 lazy val all = Project(id = "all", base = file("."), settings = Project.defaultSettings ++ osgiSettings ++ globalSettings)  settings (publish := { }, bundleSymbolicName := "fr.geocite.simpuzzle", bundleVersion := "1.0", exportPackage := Seq("fr.geocite.*")) dependsOn(simpuzzle, marius, simpoplocal, schelling, gibrat) aggregate(simpuzzle, marius, simpoplocal, schelling, gibrat)
}



