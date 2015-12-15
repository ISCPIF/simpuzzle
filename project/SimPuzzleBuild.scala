
import com.typesafe.sbt.SbtScalariform
import sbt._
import sbt.Keys._
import com.typesafe.sbt.osgi.SbtOsgi._

object SimPuzzleBuild extends Build {



  override def settings = super.settings ++ Seq (
    scalaVersion := "2.11.7"
  )

  val monocleVersion = "1.2.0-M2"
  val scalazVersion = "7.1.5"

  val defaultSettings = SbtScalariform.scalariformSettings ++ Seq(
     organization := "fr.geocites",
     publishTo := {
       if (version.value.trim.endsWith("SNAPSHOT")) Some("ISCPIF Nexus snapshot" at "http://maven.iscpif.fr/snapshots")
       else Some("ISCPIF Nexus" at "http://maven.iscpif.fr/releases")
     },
     libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-core" % "0.4.3-1",
     libraryDependencies += "org.scalaz" %% "scalaz-core" % scalazVersion,
     libraryDependencies ++= Seq(
       "com.github.julien-truffaut"  %%  "monocle-core"    % monocleVersion,
       "com.github.julien-truffaut"  %%  "monocle-generic" % monocleVersion,
       "com.github.julien-truffaut"  %%  "monocle-macro"   % monocleVersion
     ),
     libraryDependencies += "com.chuusai" %% "shapeless" % "2.2.5",
     libraryDependencies += "org.apache.commons" % "commons-math3" % "3.5",
     libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),
     resolvers += Resolver.sonatypeRepo("snapshots"),
     resolvers += "ISC-PIF" at "http://maven.iscpif.fr/public/"
   ) ++ addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1")

  lazy val geotools = "org.geotools" % "gt-referencing" % "13.2"

  lazy val simpuzzle = Project(id = "simpuzzle", base = file("simpuzzle"), settings = defaultSettings)

  lazy val gis = Project(id = "gis", base = file("gis"), settings = defaultSettings) dependsOn(simpuzzle) settings (libraryDependencies += geotools)

  lazy val gugus = Project(id = "gugus", base = file("models/gugus/model"), settings = defaultSettings) dependsOn(simpuzzle, gis)

  lazy val marius = Project(id = "marius", base = file("models/marius/model"), settings = defaultSettings) dependsOn (gugus)

}

