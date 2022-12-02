lazy val root = project in file(".") settings (
    name := "Pneuma Language",
    version := "0.0.0",
    scalaVersion := "3.2.1",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.14" % "test",
    libraryDependencies += "org.ow2.asm"    % "asm"       % "9.4",
    scalacOptions ++= Seq("-feature", "-deprecation")
)
