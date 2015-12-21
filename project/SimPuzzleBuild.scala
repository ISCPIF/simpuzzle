
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
     organization := "fr.iscpif.simpuzzle",
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
     resolvers += "ISC-PIF" at "http://maven.iscpif.fr/public/",
    publishTo <<= isSnapshot { snapshot =>
      val nexus = "https://oss.sonatype.org/"
      if (snapshot) Some("snapshots" at nexus + "content/repositories/snapshots")
      else Some("releases" at nexus + "service/local/staging/deploy/maven2")
    },
    pomIncludeRepository := { _ => false},
    licenses := Seq("Affero GPLv3" -> url("http://www.gnu.org/licenses/")),
    homepage := Some(url("https://github.com/ISCPIF/simpuzzle")),
    scmInfo := Some(ScmInfo(url("https://github.com/ISCPIF/simpuzzle.git"), "scm:git:git@github.com:ISCPIF/simpuzzle.git")),
    // To sync with Maven central, you need to supply the following information:
    pomExtra := {
      <!-- Developer contact information -->
        <developers>
          <developer>
            <id>romainreuillon</id>
            <name>Romain Reuillon</name>
            <url>https://github.com/romainreuillon/</url>
          </developer>
        </developers>
    }
   ) ++ addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1")

  lazy val geotools = "org.geotools" % "gt-referencing" % "13.2"

  lazy val puzzle = Project(id = "puzzle", base = file("puzzle"), settings = defaultSettings)

  lazy val gis = Project(id = "gis", base = file("gis"), settings = defaultSettings) dependsOn(puzzle) settings (libraryDependencies += geotools)

  lazy val marius = Project(id = "marius", base = file("models/marius/model"), settings = defaultSettings) dependsOn (puzzle, gis)

}

