
val defaultSettings = Seq(
  scalaVersion := "2.12.1",
  scalacOptions ++=
    Seq("-deprecation", "-encoding", "UTF-8", "-feature", "-unchecked", "-target:jvm-1.8", "-Xfatal-warnings"))

val protobufSettings = Seq(
  PB.targets in Compile := Seq(
    PB.gens.java -> (sourceManaged in Compile).value,
    scalapb.gen(javaConversions = true, grpc = false, flatPackage = true) -> (sourceManaged in Compile).value))

lazy val protobuf = (project in file("protobuf")).settings(defaultSettings, protobufSettings)

defaultSettings
