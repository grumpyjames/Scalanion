import sbt._

class ScalanionProject(info: ProjectInfo) extends DefaultProject(info) with protobuf.ProtobufCompiler {

  val mavenLocal = "Local Maven Repository" at "file://"+Path.userHome+"/.m2/repository"

  val scalaTest = "org.scalatest" % "scalatest" % "1.3"
  val specs = "org.specs" % "specs" % "1.6.7" from "http://specs.googlecode.com/files/specs_2.8.1-1.6.7.jar"
  val mockito = "org.mockito" % "mockito-all" % "1.8.5" from "http://mockito.googlecode.com/files/mockito-all-1.8.5.jar"
  val hamcrest = "org.hamcrest" % "hamcrest-all" % "1.3.0RC2" from "http://hamcrest.googlecode.com/files/hamcrest-all-1.3.0RC2.jar"
  val protobuf = "com.google.protobuf" % "protobuf-java" % "2.4.0a"

  def srcPath = "scalanion" / "src"

  override def mainScalaSourcePath = srcPath / "main" / "scala"
  override def mainJavaSourcePath = srcPath / "main" / "java"
  override def testScalaSourcePath = srcPath / "test" / "scala"

  override def protobufDirectory = srcPath / "main" / "protobuf"
  override def protobufOutputPath = srcPath / "main" / "java"
  override def protobufSchemas = srcPath / "main" / "protobuf" / "scalanion.proto"

}
