resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"

libraryDependencies ++= Seq("org.scalaz" %% "scalaz-core" % "7.1.0",
                            "org.scalaz" %% "scalaz-concurrent" % "7.1.0")
