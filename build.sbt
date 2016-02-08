
lazy val commonSettings = Seq(
  organization := "de.surfice",
  version := "0.1-SNAPSHOT",
  name := "smacrotools",
  scalaVersion := "2.11.7",
  scalacOptions ++= Seq("-deprecation","-feature","-Xlint"),
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-reflect" % scalaVersion.value
    ),
  resolvers += Resolver.sonatypeRepo("releases")
)


lazy val root = project.in(file("."))
  .aggregate(jvm,js)
  .settings(commonSettings:_*)
  .settings(
    publish := {},
    publishLocal := {}
  )

lazy val jvm = project
  .settings(commonSettings:_*)
  .settings(publishingSettings: _*)
  .settings(
     unmanagedSourceDirectories in Compile += baseDirectory.value / ".." / "shared" / "src" / "main" / "scala"
   )

lazy val js = project
  .enablePlugins(ScalaJSPlugin)
  .settings(commonSettings:_*)
  .settings(publishingSettings:_*)
  .settings(
    unmanagedSourceDirectories in Compile += baseDirectory.value / ".." / "shared" / "src" / "main" / "scala"
  )


/*
lazy val testMacros = project.
  dependsOn(root).
  settings(commonSettings:_*).
  settings(
    publish := {},
    publishLocal := {}
  )

lazy val tests = project.
  dependsOn(testMacros).
  settings(commonSettings:_*).
  settings(
    publish := {},
    publishLocal := {}
  )
*/

lazy val publishingSettings = Seq(
  publishMavenStyle := true,
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases"  at nexus + "service/local/staging/deploy/maven2")
  },
  pomExtra := (
    <url>https://github.com/jokade/smacrotools</url>
    <licenses>
      <license>
        <name>MIT License</name>
        <url>http://www.opensource.org/licenses/mit-license.php</url>
      </license>
    </licenses>
    <scm>
      <url>git@github.com:jokade/smacrotools</url>
      <connection>scm:git:git@github.com:jokade/smacrotools.git</connection>
    </scm>
    <developers>
      <developer>
        <id>jokade</id>
        <name>Johannes Kastner</name>
        <email>jokade@karchedon.de</email>
      </developer>
    </developers>
  )
)

