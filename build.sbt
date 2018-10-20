
name := "nta"

organization := "eu.cdevreeze.nta"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.12.7"

crossScalaVersions := Seq("2.12.7", "2.11.12")

// See: Toward a safer Scala
// http://downloads.typesafe.com/website/presentations/ScalaDaysSF2015/Toward%20a%20Safer%20Scala%20@%20Scala%20Days%20SF%202015.pdf

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Xfatal-warnings", "-Xlint", "-target:jvm-1.8")

libraryDependencies += "com.typesafe" % "config" % "1.3.2"

libraryDependencies += "eu.cdevreeze.tqa" %% "tqa" % "0.8.6"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.5" % "test"


// resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

// addCompilerPlugin("com.artima.supersafe" %% "supersafe" % "1.0.3")

publishMavenStyle := true

publishTo <<= version { (v: String) =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

publishArtifact in Test := false

pomIncludeRepository := { repo => false }

pomExtra := {
  <url>https://github.com/dvreeze/nta</url>
  <licenses>
    <license>
      <name>Apache License, Version 2.0</name>
      <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
      <distribution>repo</distribution>
      <comments>Yaidom is licensed under Apache License, Version 2.0</comments>
    </license>
  </licenses>
  <scm>
    <connection>scm:git:git@github.com:dvreeze/nta.git</connection>
    <url>https://github.com/dvreeze/nta.git</url>
    <developerConnection>scm:git:git@github.com:dvreeze/nta.git</developerConnection>
  </scm>
  <developers>
    <developer>
      <id>dvreeze</id>
      <name>Chris de Vreeze</name>
      <email>chris.de.vreeze@caiway.net</email>
    </developer>
  </developers>
}
