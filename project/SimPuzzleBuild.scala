
import sbt._
import Keys._
import com.typesafe.sbt.osgi.OsgiKeys._
import com.typesafe.sbt.osgi.SbtOsgi._
import sbtrelease.ReleasePlugin._

object SimPuzzleBuild extends Build {

 override def settings = 
   super.settings ++ Seq(
     scalaVersion := "2.10.2",
     organization := "fr.geocite"
   ) 

  lazy val globalSettings = Project.defaultSettings ++ Seq(
     publishTo <<= 
       isSnapshot(
          if(_) Some("Openmole Nexus" at "http://maven.iscpif.fr/snapshots") 
          else Some("Openmole Nexus" at "http://maven.iscpif.fr/releases")
        ),
     credentials += Credentials(Path.userHome / ".sbt" / "iscpif.credentials"),
     libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-core" % "0.4.2",
     libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.0.3",
     libraryDependencies += "com.chuusai" % "shapeless" % "2.0.0-M1" cross CrossVersion.full,
     libraryDependencies += "org.apache.commons" % "commons-math3" % "3.2",
     resolvers += "ISC-PIF" at "http://maven.iscpif.fr/public"
   ) ++ releaseSettings


 lazy val geotools = "org.geotools" % "gt-referencing" % "9.3"

 lazy val graphstream = "org.graphstream" % "gs-core" % "1.2"

 lazy val gexf4j = "it.uniroma1.dis.wsngroup.gexf4j" % "gexf4j" % "0.4.4-BETA"

 lazy val simpuzzle = Project(id = "simpuzzle", base = file("simpuzzle")) settings (globalSettings: _*)

 lazy val gis = Project(id = "gis", base = file("gis")) dependsOn(simpuzzle) settings (libraryDependencies += geotools)

 lazy val marius = Project(id = "marius", base = file("marius")) dependsOn (simpuzzle, gibrat, gis) settings (globalSettings: _*)
 
 lazy val simpoplocal = Project(id = "simpoplocal", base = file("simpoplocal")) dependsOn(simpuzzle) settings (globalSettings: _*)

 lazy val schelling = Project(id = "schelling", base = file("schelling")) dependsOn(simpuzzle) settings (globalSettings: _*)
 
 lazy val sugarscape = Project(id = "sugarscape", base = file("sugarscape")) dependsOn(simpuzzle) settings (globalSettings: _*)

 lazy val gibrat = Project(id = "gibrat", base = file("gibrat")) dependsOn(simpuzzle) settings (globalSettings: _*)

 lazy val mariusmodel = Project(id = "mariusmodel", base = file("models/marius")) dependsOn(marius) settings (libraryDependencies += graphstream)

 //lazy val all = Project(id = "all", base = file("."), settings = Project.defaultSettings ++ osgiSettings ++ globalSettings ++ Seq(geotools))  settings (publish := { }, bundleSymbolicName := "fr.geocite.simpuzzle", bundleVersion := "1.0", exportPackage := Seq("fr.geocite.*")) dependsOn(simpuzzle, marius, simpoplocal, schelling, gibrat, sugarscape) aggregate(simpuzzle, marius, simpoplocal, schelling, gibrat, gis, sugarscape)
}



