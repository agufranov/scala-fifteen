scalacOptions ++= Seq("-feature")

libraryDependencies <+= scalaVersion { "org.scala-lang" % "scala-swing" % _ }
