
import com.typesafe.sbt.SbtScalariform
import sbt._
import sbt.Keys._
import com.typesafe.sbt.osgi.SbtOsgi._

object SimPuzzleBuild extends Build with Marius with Indus with DefaultSettings {

  override def settings = super.settings ++ Seq (
    scalaVersion := "2.11.6",
    crossScalaVersions := Seq("2.10.4", "2.11.6")
  )

  lazy val simpoplocal = Project(id = "simpoplocal", base = file("models/simpoplocal"), settings = defaultSettings) dependsOn(simpuzzle)

  lazy val schelling = Project(id = "schelling", base = file("models/schelling"), settings = defaultSettings) dependsOn(simpuzzle)
 
  lazy val sugarscape = Project(id = "sugarscape", base = file("models/sugarscape"), settings = defaultSettings) dependsOn(simpuzzle)

  lazy val gibrat = Project(id = "gibrat", base = file("models/gibrat"), settings = defaultSettings) dependsOn(simpuzzle)

  lazy val flocking = Project(id = "flocking", base = file("models/flocking/model"))

  lazy val flockingVisualisation = Project(id = "flockingvisualisation", base = file("models/flocking/visualisation"), settings = defaultSettings) settings {
    libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "1.0.1"
  } dependsOn(flocking)

  lazy val flockingbehaviour = Project(id = "flockingbehaviour", base = file("models/flocking/behaviour"), settings = defaultSettings  ++ osgiSettings) dependsOn(flocking) settings (
    OsgiKeys.exportPackage := Seq("fr.iscpif.flocking.*"),
    OsgiKeys.importPackage := Seq("*;resolution:=optional"),
    OsgiKeys.privatePackage := Seq("!scala.*", "*")
    )

}


trait DefaultSettings {
  val monocleVersion = "0.5.1"  // or "0.5-SNAPSHOT"
  val scalazVersion = "7.1.0"

  val defaultSettings = SbtScalariform.scalariformSettings ++ Seq(
     organization := "fr.geocites",
     publishTo := {
       if (version.value.trim.endsWith("SNAPSHOT")) Some("ISCPIF Nexus snapshot" at "http://maven.iscpif.fr/snapshots")
       else Some("ISCPIF Nexus" at "http://maven.iscpif.fr/releases")
     },
     libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-core" % "0.4.3",
     libraryDependencies += "org.scalaz" %% "scalaz-core" % scalazVersion,
     libraryDependencies += "org.scalaz" %% "scalaz-iteratee" % scalazVersion,
     libraryDependencies ++= Seq(
       "com.github.julien-truffaut"  %%  "monocle-core"    % monocleVersion,
       "com.github.julien-truffaut"  %%  "monocle-generic" % monocleVersion,
       "com.github.julien-truffaut"  %%  "monocle-macro"   % monocleVersion
     ),
     libraryDependencies += (
       if (scalaVersion.value.startsWith("2.10")) "com.chuusai" %% "shapeless" % "2.0.0" cross CrossVersion.full else "com.chuusai" %% "shapeless" % "2.0.0"),
     libraryDependencies += "org.apache.commons" % "commons-math3" % "3.3",
     libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),
     libraryDependencies ++= (
       if (scalaVersion.value.startsWith("2.10")) List("org.scalamacros" %% "quasiquotes" % "2.0.0")
       else Nil
     ),
     resolvers += Resolver.sonatypeRepo("snapshots"),
     resolvers += "ISC-PIF" at "http://maven.iscpif.fr/public/"
   )
}

trait Simpuzzle <: DefaultSettings {
  lazy val geotools = "org.geotools" % "gt-referencing" % "12.2"

  lazy val simpuzzle = Project(id = "simpuzzle", base = file("simpuzzle"), settings = defaultSettings)

  lazy val gis = Project(id = "gis", base = file("gis"), settings = defaultSettings) dependsOn(simpuzzle) settings (libraryDependencies += geotools)

}

trait Gugus <: Simpuzzle {
  lazy val gugus = Project(id = "gugus", base = file("models/gugus/model"), settings = defaultSettings) dependsOn(simpuzzle, gis)

  lazy val guguscalibration = Project(id = "guguscalibration", base = file("models/gugus/calibration"), settings = defaultSettings) dependsOn(gugus)

}

trait Marius <: Gugus {

  lazy val marius = Project(id = "marius", base = file("models/marius/model"), settings = defaultSettings) dependsOn (gugus)


  lazy val mariusrun = Project(id = "mariusrun", base = file("models/marius/run"), settings = defaultSettings) dependsOn(marius)

  lazy val mariuscalibration = Project(id = "mariuscalibration", base = file("models/marius/calibration"), settings = defaultSettings ++ osgiSettings) dependsOn(marius, mariusrun, guguscalibration) settings (
    OsgiKeys.exportPackage := Seq("fr.geocites.marius.*,fr.geocites.gugus.*,fr.geocites.simpuzzle.*"),
    OsgiKeys.importPackage := Seq("scala.*"),
    OsgiKeys.privatePackage := Seq("!scala.*", "*")
    )

  lazy val mariusbehaviour = Project(id = "mariusbehaviour", base = file("models/marius/behaviour"), settings = defaultSettings ++ osgiSettings) dependsOn(marius, mariusrun, mariuscalibration) settings (
    OsgiKeys.exportPackage := Seq("fr.geocites.marius.*,fr.geocites.gugus.*,fr.geocites.simpuzzle.*"),
    OsgiKeys.importPackage := Seq("scala.*"),
    OsgiKeys.privatePackage := Seq("!scala.*", "*")
    )

  lazy val mariusrest = Project(id = "mariusrest", base = file("models/marius/rest"), settings = defaultSettings) dependsOn(mariuscalibration) settings (
    libraryDependencies += "fr.iscpif" %% "family" % "1.0-SNAPSHOT",
    libraryDependencies += "org.scalatra" %% "scalatra" % "2.3.0",
    libraryDependencies += "org.eclipse.jetty" % "jetty-webapp" % "9.3.0.M1",
    libraryDependencies += "org.slf4j" % "slf4j-simple" % "1.7.10"
    )
}

trait Indus <: Gugus {
  lazy val indus = Project(id = "indus", base = file("models/indus/model"), settings = defaultSettings) dependsOn (gugus)
  lazy val indusrun = Project(id = "indusrun", base = file("models/indus/run"), settings = defaultSettings) dependsOn (indus)
  lazy val induscalibration = Project(id = "induscalibration", base = file("models/indus/calibration"), settings = defaultSettings ++ osgiSettings) dependsOn(indus, indusrun, guguscalibration) settings (
    OsgiKeys.exportPackage := Seq("fr.geocites.indus.*,fr.geocites.gugus.*,fr.geocites.simpuzzle.*"),
    OsgiKeys.importPackage := Seq("scala.*"),
    OsgiKeys.privatePackage := Seq("!scala.*", "*")
    )
}
