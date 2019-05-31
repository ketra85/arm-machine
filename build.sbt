val ScalatraVersion = "2.6.+"

name := "ArmMachine"

lazy val commonSettings = Seq(
  organization := "com.ketra85",
  version := "0.1.0-SNAPSHOT",
  scalaVersion := "2.12.6",
  resolvers += Classpaths.typesafeReleases,
  javaOptions ++= Seq(
    "-Xdebug",
    "-Xrunjdwp:transport=dt_socket,server=y,suspend=n,address=5005"
  )
)

lazy val root = project
  .in(file("."))
  .aggregate(core, macros)
  .settings(
    commonSettings
  )
  .enablePlugins(SbtPlugin)

lazy val core = project
  .in(file("core"))
  .settings(
    commonSettings,
    libraryDependencies ++= Seq(
      "org.scalatra" %% "scalatra" % ScalatraVersion,
      "org.scalatra" %% "scalatra-scalate" % ScalatraVersion,
      "org.scalatra" %% "scalatra-specs2" % ScalatraVersion % "test",
      "ch.qos.logback" % "logback-classic" % "1.2.3" % "provided",
      "org.eclipse.jetty" % "jetty-webapp" % "9.4.9.v20180320" % "provided",
      "javax.servlet" % "javax.servlet-api" % "3.1.0" % "provided",
      "org.scala-lang.modules" %% "scala-xml" % "1.2.0"
    ),
  )

lazy val macros = project
  .in(file("macros"))
  .dependsOn(core)
  .settings(
    commonSettings,
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value
    )
  )

//run <<= run in Compile in core
//lazy val macros = (project in file("macros")).settings(
//  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
//)
//
//lazy val core = (project in file("core")) dependsOn macros


enablePlugins(SbtTwirl)
enablePlugins(ScalatraPlugin)
