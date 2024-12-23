scalaVersion := "3.5.2"

import org.scalajs.linker.interface.ModuleSplitStyle.SmallModulesFor

lazy val livechart = project
  .in(file("."))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    scalacOptions ++= Seq("-encoding", "utf-8", "-deprecation", "-feature"),

    // We have a `main` method
    scalaJSUseMainModuleInitializer := true,

    // Emit modules in the most Vite-friendly way
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.ESModule)
        .withModuleSplitStyle(SmallModulesFor(List("livechart")))
    }
  )
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.8.0"
    libraryDependencies += "com.raquo" %%% "laminar" % "17.2.0"