lazy val root = (project in file(".")).
    settings(
        name := "scheme--",
        version := "0.01",
        scalaVersion := "2.11.7",
        libraryDependencies ++= Seq(
            "junit" % "junit" % "4.11" % "test",
            "org.scalatest" %% "scalatest" % "2.2.6" % "test"
        )
    )
