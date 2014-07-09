version := "0.1-SNAPSHOT"

resolvers ++= Seq(
  "ISC-PIF Release" at "http://maven.iscpif.fr/public/",
  "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/",
  "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.11.4" % "test"
)

libraryDependencies += "org.scala-lang" % "scala-swing" % "2.11+"

