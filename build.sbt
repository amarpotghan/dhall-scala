name := "dhall-scala"
version := "0.1"
scalaVersion := "2.12.4"
crossScalaVersions := List(scalaVersion.value, "2.11.12")

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core" % "4.0.3" % "test",
  "org.typelevel" %% "cats-core" % "1.0.1",
  "org.parboiled" %% "parboiled" % "2.1.4"
)

scalacOptions ++= Seq(
  "-language:higherKinds"
)
