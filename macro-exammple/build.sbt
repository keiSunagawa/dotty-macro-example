val dottyVersion = "0.17.0-bin-20190711-e2130b9-NIGHTLY"

lazy val macros = project
  .in(file("./macros"))
  .settings(
    name := "macros",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
  )

lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty-simple",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
  ).dependsOn(macros)
