name := "essential-scala"

organization := "com.codinginflipflops"

version := "0.0.1"

scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test" withSources() withJavadoc(),
  "org.scalacheck" %% "scalacheck" % "1.12.3" % "test" withSources() withJavadoc()
)

initialCommands := "import com.codinginflipflops.essentialscala._"

