def scalacOptionsVersion(scalaVersion: String): Seq[String] = {
  Seq() ++ {
    // If we're building with Scala > 2.11, enable the compile option
    //  switch to support our anonymous Bundle definitions:
    //  https://github.com/scala/bug/issues/10047
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, scalaMajor: Long)) if scalaMajor < 12 => Seq()
      case _ => Seq("-Xsource:2.11")
    }
  }
}

def javacOptionsVersion(scalaVersion: String): Seq[String] = {
  Seq() ++ {
    // Scala 2.12 requires Java 8. We continue to generate
    //  Java 7 compatible code for Scala 2.11
    //  for compatibility with old clients.
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, scalaMajor: Long)) if scalaMajor < 12 =>
        Seq("-source", "1.7", "-target", "1.7")
      case _ =>
        Seq("-source", "1.8", "-target", "1.8")
    }
  }
}

name := "chisel-module-template"

version := "3.1.0"

scalaVersion := "2.12.4"

crossScalaVersions := Seq("2.11.12", "2.12.4")

resolvers ++= Seq(
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("releases")
)

// Provide a managed dependency on X if -DXVersion="" is supplied on the command line.
val defaultVersions = Map(
  "chisel3" -> "3.1.+",
  "chisel-iotesters" -> "1.2.+"
  )

libraryDependencies ++= (Seq("chisel3","chisel-iotesters").map {
  dep: String => "edu.berkeley.cs" %% dep % sys.props.getOrElse(dep + "Version", defaultVersions(dep)) })

val versionOfScala = "2.12.4"

val fs2Version = "0.10.3"
val catsVersion = "1.1.0"
val catsEffectVersion = "0.10"
libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core" % catsVersion,             // abstract category dork stuff
    "com.lihaoyi" %% "sourcecode" % "0.1.4",                  // expert println debugging
    "com.lihaoyi" %% "pprint" % "0.5.3",                      // pretty print for types and case classes
    "com.chuusai" %% "shapeless" % "2.3.2",                   // Abstract level category dork stuff

    "org.typelevel" %% "cats-effect" % catsEffectVersion,     // IO monad category wank

    "co.fs2" %% "fs2-core" % fs2Version,                      // The best library
    "co.fs2" %% "fs2-io"   % fs2Version,                      // The best library
    "org.tpolecat" %% "atto-core"    % "0.6.3",
    "org.tpolecat" %% "atto-refined" % "0.6.3",
    "org.typelevel" %% "spire" % "0.14.1"


    // "npm" %%% "is-even" %% "12.3-SNAPSHOT"
    // "npm" %%% "is-odd" %% "12.3-SNAPSHOT"
    // "npm" %%% "is-number" %% "12.3-SNAPSHOT"
  )

scalacOptions ++= scalacOptionsVersion(scalaVersion.value)
scalacOptions ++= Seq("-language:reflectiveCalls")

javacOptions ++= javacOptionsVersion(scalaVersion.value)

resolvers += Resolver.sonatypeRepo("releases")
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.7")
addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.2.4")

testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-f", "result.txt", "-fS", "/dev/null")
