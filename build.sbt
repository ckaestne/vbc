scalaVersion := "2.11.7"

//libraryDependencies += "org.ow2.asm" % "asm" % "5.0.4"

libraryDependencies += "de.fosd.typechef" % "featureexprlib_2.11" % "0.4.1"

libraryDependencies += "de.fosd.typechef" % "conditionallib_2.11" % "0.4.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"

libraryDependencies += "com.storm-enroute" %% "scalameter" % "0.7" % "test"

parallelExecution in Test := false

javacOptions ++= Seq("-source", "1.8", "-target", "1.8", "-Xlint")

initialize := {
    val _ = initialize.value
    if (sys.props("java.specification.version") != "1.8")
        sys.error("Java 8 is required for this project.")
}
