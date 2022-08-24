lazy val root = project in file(".") settings (
    name := "Pneuma Language",
    version := "0.0.0",
    scalaVersion := "3.1.3",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.12" % "test",
    libraryDependencies += "org.ow2.asm"    % "asm"       % "9.3",
    scalacOptions ++= Seq("-feature", "-deprecation")
)
