inThisBuild(Def.settings(
  scalaVersion := "2.13.4",
  version := "0.1.0-SNAPSHOT",
))

lazy val tint = project
  .in(file("tint"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    Compile / unmanagedSourceDirectories += baseDirectory.value / "src",
    Test / unmanagedSourceDirectories += baseDirectory.value / "test/src",
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-linker" % "1.3.1",
      "com.lihaoyi" %%% "utest" % "0.7.5" % "test",
    ),
    testFrameworks += new TestFramework("utest.runner.Framework"),
    scalaJSUseMainModuleInitializer := true,
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.CommonJSModule),
    }
  )

lazy val sample = project
  .in(file("sample"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    Compile / unmanagedSourceDirectories += baseDirectory.value / "src",
    Test / unmanagedSourceDirectories += baseDirectory.value / "test/src",
    scalaJSUseMainModuleInitializer := true,
  )
