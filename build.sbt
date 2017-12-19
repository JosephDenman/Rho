name := "Rho"

version := "0.1"

scalaVersion := "2.12.4"

libraryDependencies += "org.typelevel" %% "cats-core" % "1.0.0-RC1"

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")
