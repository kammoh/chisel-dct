name := "dct"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies += "edu.berkeley.cs" %% "chisel" % "2.3-SNAPSHOT"

libraryDependencies  ++= Seq(
//  // other dependencies here
//  "org.scalanlp" %% "breeze" % "0.11.2",
//  // native libraries are not included by default. add this if you want them (as of 0.7)
//  // native libraries greatly improve performance, but increase jar sizes.
//  // It also packages various blas implementations, which have licenses that may or may not
//  // be compatible with the Apache License. No GPL code, as best I know.
//  "org.scalanlp" %% "breeze-natives" % "0.11.2",
//  // the visualization library is distributed separately as well.
//  // It depends on LGPL code.
//  "org.scalanlp" %% "breeze-viz" % "0.11.2" //,
//
//  "dk.ange" %% "javaoctave" % "0.6.4"
)

libraryDependencies += "net.java.dev.jna" % "jna" % "4.1.0"
libraryDependencies += "org.scalafx" %% "scalafx" % "8.0.40-R8"

lazy val makeCSharedLibTask = TaskKey[Int]("Build JNA C shared library")

makeCSharedLibTask := {
  println("building C library")
  val pb = scala.sys.process.Process("""make -C src/main/cpp""")
  pb.!
}

compile in Compile := {
  makeCSharedLibTask.value
  (compile in Compile).value
}

run in Runtime := {
  makeCSharedLibTask.value
  (run in Runtime).value
}

//resolvers ++= Seq(
//  // other resolvers here
//  // if you want to use snapshot builds (currently 0.12-SNAPSHOT), use this.
//  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
//  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
//)


