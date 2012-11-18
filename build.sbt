name := "Parsing papers"

version := "1.0"

scalaVersion := "2.9.2"

scalaSource in Compile <<= baseDirectory(_ / "src/paper")

scalacOptions ++= Seq("-unchecked", "-Ywarn-dead-code", "-deprecation")

libraryDependencies  ++= Seq(
            // other dependencies here
            // pick and choose:
            "org.scalanlp" %% "breeze-process" % "0.1"
)

resolvers ++= Seq(
            // other resolvers here
            // if you want to use snapshot builds (currently 0.2-SNAPSHOT), use this.
            "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
)

initialCommands := """
  import System.{currentTimeMillis => now}
  def time[T](f: => T): T = {
    val start = now
    try { f } finally { println("Elapsed: " + (now - start)/1000.0 + " s") }
  }
"""

// The main class
mainClass in (Compile, run) := Some("paper.Analyze")

// The sources to be watched
//watchSources <+= baseDirectory map { _ / "lexer" }

//watchSources <+= baseDirectory map { _ / "parser" }
