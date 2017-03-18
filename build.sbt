name := "dhall-scala"
version := "0.1"
scalaVersion := "2.11.8"
libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core" % "3.8.9" % "test",
  "org.typelevel" %% "cats" % "0.9.0",
  "org.parboiled" %% "parboiled" % "2.1.4"
)

scalacOptions ++= Seq(
  "-language:higherKinds"
)
