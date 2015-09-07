
import com.typesafe.sbt.SbtScalariform
import sbt._
import sbt.Keys._
import com.typesafe.sbt.osgi.SbtOsgi._

object SimPuzzleBuild extends Build with Marius with Indus with Schelling with DefaultSettings {

  override def settings = super.settings ++ Seq (
    scalaVersion := "2.11.7"
  )

  lazy val simpoplocal = Project(id = "simpoplocal", base = file("models/simpoplocal"), settings = defaultSettings) dependsOn(simpuzzle)

  lazy val sugarscape = Project(id = "sugarscape", base = file("models/sugarscape"), settings = defaultSettings) dependsOn(simpuzzle)

  lazy val gibrat = Project(id = "gibrat", base = file("models/gibrat"), settings = defaultSettings) dependsOn(simpuzzle)

  lazy val flocking = Project(id = "flocking", base = file("models/flocking/model"))

  lazy val flockingVisualisation = Project(id = "flockingvisualisation", base = file("models/flocking/visualisation"), settings = defaultSettings) settings {
    libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "1.0.2"
  } dependsOn(flocking)

  lazy val flockingbehaviour = Project(id = "flockingbehaviour", base = file("models/flocking/behaviour"), settings = defaultSettings  ++ osgiSettings) dependsOn(flocking) settings (
    OsgiKeys.exportPackage := Seq("fr.iscpif.flocking.*"),
    OsgiKeys.importPackage := Seq("*;resolution:=optional"),
    OsgiKeys.privatePackage := Seq("!scala.*", "*")
    )

}


trait DefaultSettings {
  val monocleVersion = "1.1.1"  // or "0.5-SNAPSHOT"
  val scalazVersion = "7.1.2"

  val defaultSettings = SbtScalariform.scalariformSettings ++ Seq(
     organization := "fr.geocites",
     publishTo := {
       if (version.value.trim.endsWith("SNAPSHOT")) Some("ISCPIF Nexus snapshot" at "http://maven.iscpif.fr/snapshots")
       else Some("ISCPIF Nexus" at "http://maven.iscpif.fr/releases")
     },
     libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-core" % "0.4.3-1",
     libraryDependencies += "org.scalaz" %% "scalaz-core" % scalazVersion,
     libraryDependencies += "org.scalaz" %% "scalaz-iteratee" % scalazVersion,
     libraryDependencies ++= Seq(
       "com.github.julien-truffaut"  %%  "monocle-core"    % monocleVersion,
       "com.github.julien-truffaut"  %%  "monocle-generic" % monocleVersion,
       "com.github.julien-truffaut"  %%  "monocle-macro"   % monocleVersion
     ),
     libraryDependencies += "com.chuusai" %% "shapeless" % "2.1.0",
     libraryDependencies += "org.apache.commons" % "commons-math3" % "3.5",
     libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),
     resolvers += Resolver.sonatypeRepo("snapshots"),
     resolvers += "ISC-PIF" at "http://maven.iscpif.fr/public/"
   )
}

trait Simpuzzle <: DefaultSettings {
  lazy val geotools = "org.geotools" % "gt-referencing" % "13.0"

  lazy val simpuzzle = Project(id = "simpuzzle", base = file("simpuzzle"), settings = defaultSettings)

  lazy val gis = Project(id = "gis", base = file("gis"), settings = defaultSettings) dependsOn(simpuzzle) settings (libraryDependencies += geotools)

}

trait Gugus <: Simpuzzle {
  lazy val gugus = Project(id = "gugus", base = file("models/gugus/model"), settings = defaultSettings) dependsOn(simpuzzle, gis)
  lazy val guguscalibration = Project(id = "gugus-calibration", base = file("models/gugus/calibration"), settings = defaultSettings) dependsOn(gugus)
}

trait Marius <: Gugus {

  lazy val marius = Project(id = "marius", base = file("models/marius/model"), settings = defaultSettings) dependsOn (gugus)

  lazy val mariusrun = Project(id = "mariusrun", base = file("models/marius/run"), settings = defaultSettings) dependsOn(marius)

  lazy val mariuscalibration = Project(id = "mariuscalibration", base = file("models/marius/calibration"), settings = defaultSettings ++ osgiSettings) dependsOn(marius, mariusrun, guguscalibration) settings (
    OsgiKeys.exportPackage := Seq("fr.geocites.marius.*,fr.geocites.gugus.*,fr.geocites.simpuzzle.*"),
    OsgiKeys.importPackage := Seq("scala.*"),
    OsgiKeys.privatePackage := Seq("!scala.*", "*")
    )

  lazy val mariusbehaviour = Project(id = "marius-behaviour", base = file("models/marius/behaviour"), settings = defaultSettings ++ osgiSettings) dependsOn(marius, mariusrun, mariuscalibration) settings (
    OsgiKeys.exportPackage := Seq("fr.geocites.marius.*,fr.geocites.gugus.*,fr.geocites.simpuzzle.*"),
    OsgiKeys.importPackage := Seq("scala.*"),
    OsgiKeys.privatePackage := Seq("!scala.*", "*")
    )

  lazy val mariusrest = Project(id = "marius-rest", base = file("models/marius/rest"), settings = defaultSettings) dependsOn(mariuscalibration) settings (
    libraryDependencies += "fr.iscpif" %% "family" % "1.1",
    libraryDependencies += "org.scalatra" %% "scalatra" % "2.3.1",
    libraryDependencies += "org.eclipse.jetty" % "jetty-webapp" % "9.3.0.M2",
    libraryDependencies += "org.slf4j" % "slf4j-simple" % "1.7.12",
    mainClass := Some("fr.iscpif.marius.RESTAPI")
    )
}

trait Indus <: Gugus {
  lazy val indus = Project(id = "indus", base = file("models/indus/model"), settings = defaultSettings) dependsOn (gugus)
  lazy val indusrun = Project(id = "indus-run", base = file("models/indus/run"), settings = defaultSettings) dependsOn (indus)
  lazy val induscalibration = Project(id = "indus-calibration", base = file("models/indus/calibration"), settings = defaultSettings ++ osgiSettings) dependsOn(indus, indusrun, guguscalibration) settings (
    OsgiKeys.exportPackage := Seq("fr.geocites.indus.*,fr.geocites.gugus.*,fr.geocites.simpuzzle.*"),
    OsgiKeys.importPackage := Seq("scala.*"),
    OsgiKeys.privatePackage := Seq("!scala.*", "*")
    )
}

trait Schelling <: Simpuzzle {
  lazy val schelling = Project(id = "schelling", base = file("models/schelling"), settings = defaultSettings) dependsOn(simpuzzle)
}