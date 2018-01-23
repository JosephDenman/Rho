name := "Rho"
version := "0.1"
scalaVersion := "2.12.4"
libraryDependencies += "com.github.ancane" %% "hashids-scala" % "1.3"
libraryDependencies += "org.typelevel" %% "cats-core" % "1.0.1"
libraryDependencies += "org.typelevel" %% "cats-free" % "1.0.0-RC1"
libraryDependencies += "io.monix" %% "monix" % "2.3.0"
libraryDependencies += "io.monix" %% "monix-cats" % "2.3.0"
resolvers += Resolver.sonatypeRepo("releases")
scalacOptions += "-Ypartial-unification"
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
libraryDependencies += "com.github.mpilquist" %% "simulacrum" % "0.11.0"