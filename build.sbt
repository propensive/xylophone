import com.typesafe.sbt.pgp.PgpKeys.publishSigned
import ReleaseTransformations._

lazy val core = project
  .in(file("core"))
  .settings(buildSettings: _*)
  .settings(publishSettings: _*)
  .settings(scalaMacroDependencies: _*)
  .settings(moduleName := "xylophone")

lazy val tests = project
  .in(file("tests"))
  .settings(buildSettings: _*)
  .settings(noPublishSettings: _*)
  .settings(moduleName := "xylophone-tests")
  .settings(quasiQuotesDependencies)
  .settings(libraryDependencies ++= Seq(
    "com.propensive" %% "rapture-test" % "2.0.0-M8" % "test"
  ))
  .dependsOn(core)

lazy val buildSettings = Seq(
  organization := "com.propensive",
  scalaVersion := "2.12.1",
  name := "xylophone",
  version := "1.0.0",
  scalacOptions ++= Seq(
    "-deprecation", // Emit warning and location for usages of deprecated APIs.
    "-feature", // Emit warning and location for usages of features that should be imported explicitly.
    "-Ywarn-value-discard", // Warn wen value was discard
    "-Ywarn-dead-code",  // Warn when dead code is identified.
    "-Ywarn-nullary-unit",
    "-Ywarn-numeric-widen", // Warn when numerics are widened.
    "-Ywarn-inaccessible",  // Warn about inaccessible types in method signatures.
    "-Ywarn-adapted-args",
    "-unchecked", // Enable additional warnings where generated code depends on assumptions.
    "-Xfatal-warnings", // Fail the compilation if there are any warnings.
    "-Ywarn-unused-import", //Warn when imports are unused
    "-Ywarn-unused",  // Warn when local and private vals, vars, defs, and types are unused.
    "-Yno-adapted-args", // Warn if an argument list is modified to match the receiver.
    "-Ywarn-nullary-override", // Warn when non-nullary overrides nullary, e.g. def foo() over def foo.
    "-Xlint:-stars-align,_", // Enable recommended additional warnings.
    "-Xlint:missing-interpolator", // A string literal appears to be missing an interpolator id.
    "-explaintypes", //Explain type errors in more detail.
    "-encoding",
    "UTF-8",
    "-Xexperimental",
    "-Xfuture" // Turn on future language features and guard against a few deprecated features around Futures.
  ),
  crossScalaVersions := Seq("2.10.6", "2.11.8", "2.12.1"),
  scmInfo := Some(ScmInfo(url("https://github.com/propensive/xylophone"),
    "scm:git:git@github.com:propensive/xylophone.git"))
)

lazy val publishSettings = Seq(
  homepage := Some(url("http://co.ntextu.al/")),
  licenses := Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  autoAPIMappings := true,
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := { _ => false },
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if(isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases" at nexus + "service/local/staging/deploy/maven2")
  },
  pomExtra := (
    <developers>
      <developer>
        <id>propensive</id>
        <name>Jon Pretty</name>
        <url>https://github.com/propensive/xylophone/</url>
      </developer>
    </developers>
  ),
  releaseProcess := Seq[ReleaseStep](
    checkSnapshotDependencies,
    inquireVersions,
    runTest,
    setReleaseVersion,
    commitReleaseVersion,
    tagRelease,
    publishArtifacts,
    setNextVersion,
    commitNextVersion,
    ReleaseStep(action = Command.process("sonatypeReleaseAll", _)),
    pushChanges
  ),
  releaseCrossBuild := true,
  releasePublishArtifactsAction := PgpKeys.publishSigned.value
)

lazy val noPublishSettings = Seq(
  publish := (),
  publishLocal := (),
  publishArtifact := false
)

import java.io.File

def crossVersionSharedSources()  = Seq(
 (unmanagedSourceDirectories in Compile) ++= { (unmanagedSourceDirectories in Compile ).value.map {
     dir:File => new File(dir.getPath + "_" + scalaBinaryVersion.value)}}
)

lazy val quasiQuotesDependencies: Seq[Setting[_]] =
  libraryDependencies ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, scalaMajor)) if scalaMajor >= 11 => Seq()
      case Some((2, 10)) => Seq(
        compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
        "org.scalamacros" %% "quasiquotes" % "2.1.0" cross CrossVersion.binary
      )
    }
  }

lazy val scalaMacroDependencies: Seq[Setting[_]] = Seq(
  libraryDependencies += "org.typelevel" %% "macro-compat" % "1.1.1",
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value,
  libraryDependencies += compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
)

credentials ++= (for {
  username <- Option(System.getenv().get("SONATYPE_USERNAME"))
  password <- Option(System.getenv().get("SONATYPE_PASSWORD"))
} yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)).toSeq
