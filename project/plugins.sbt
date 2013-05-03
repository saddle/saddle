resolvers += "sbt-idea-repo" at "http://mpeltonen.github.com/maven/"

addSbtPlugin("com.typesafe.sbt" % "sbt-pgp" % "0.8")

addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.1.0")

addSbtPlugin("com.orrsella" % "sbt-sublime" % "1.0.5")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.8.4")