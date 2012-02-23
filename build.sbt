name := "Scalanion"

version := "0.2"

scalaVersion := "2.9.1"

libraryDependencies ++= Seq(
    "junit" % "junit-dep" % "4.10" % "test->default",
    "com.google.protobuf" % "protobuf-java" % "2.4.0a",
    "com.novocode" % "junit-interface" % "0.7" % "test->default",		    
    "org.hamcrest" % "hamcrest-core" % "1.3.RC2" % "test->default",
    "org.hamcrest" % "hamcrest-library" % "1.3.RC2" % "test->default",
    "org.mockito" % "mockito-all" % "1.8.5" % "test",
    "org.scalatest" %% "scalatest" % "1.7.1" % "test",
    "org.specs2" %% "specs2" % "1.8.2" % "test"
)
 
resolvers ++= Seq("snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
                  "releases" at "http://oss.sonatype.org/content/repositories/releases")
