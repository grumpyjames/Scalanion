name := "Scalanion"

version := "0.2"

scalaVersion := "2.9.1"

libraryDependencies ++= Seq(
    "com.google.protobuf" % "protobuf-java" % "2.4.0a",
    "org.mockito" % "mockito-all" % "1.8.5" % "test",
    "org.specs2" %% "specs2" % "1.8.2" % "test"
)
 
resolvers ++= Seq("snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
                  "releases" at "http://oss.sonatype.org/content/repositories/releases")
