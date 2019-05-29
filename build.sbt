val ScalatraVersion = "2.6.4"

organization := "com.ketra85"

name := "ArmMachine"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.12.6"

resolvers += Classpaths.typesafeReleases

libraryDependencies ++= Seq(
  "org.scalatra" %% "scalatra" % ScalatraVersion,
  "org.scalatra" %% "scalatra-scalate" % ScalatraVersion,
  "org.scalatra" %% "scalatra-specs2" % ScalatraVersion % "test",
  "ch.qos.logback" % "logback-classic" % "1.2.3" % "runtime",
  "org.eclipse.jetty" % "jetty-webapp" % "9.4.9.v20180320" % "container",
  "javax.servlet" % "javax.servlet-api" % "3.1.0" % "provided",
  "org.clapper" %% "classutil" % "1.4.0"
)

lazy val root = project
  .in(file("."))
  .aggregate(core, macros)

lazy val core = project
  .in(file("core"))
  .settings(
    
  )

lazy val macros = project
  .in(file("macros"))
  .dependsOn(core)
  .settings(
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
