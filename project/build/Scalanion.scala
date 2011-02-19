import sbt._

class ScalanionProject(info: ProjectInfo) extends DefaultProject(info) {
  def srcPath = "scalanion" / "src"

  override def mainScalaSourcePath = srcPath / "main" / "scala"
  override def testScalaSourcePath = srcPath / "test" / "scala"
}
