name:= "concurrency-examples"
version:= "1.0"
scalaVersion:= "2.11.1"

resolvers ++= Seq(
	"Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
	"Sonatype OSS Releases" at "https://oss.sonatype.org/content/repositories/releases",
	"Typesafe Repository" at "https://typesafe.com/typesafe/releases",
)

libraryDependencies += "commons-io" % "commons-io" % "2.4"

//Do not fork , run it and spawned threads in the same jvm instance as sbt itself.
fork:= false
