lazy val versions = new {
  val bloomfilter = "0.4.0"
  val cats = "0.7.2"
  val debox = "0.7.3"
  val dogs = "0.3.1"
  val freasy = "0.4.1"
  val fs2 = "0.9.0-RC2"
  val fs2Cats = "0.1.0-RC2"
  val imp = "0.2.1"
  val monocle = "1.3.1"
  val paradise = "2.1.0"
  val scalacheck = "1.13.2"
  val scalameter = "0.7"
  val scalatest = "3.0.0"
  val shapeless = "2.3.2"
  val simulacrum = "0.9.0"
  val spire = "0.11.0"
}

lazy val functionalibs = Seq(
  "org.typelevel"          %% "cats"          % versions.cats,
  "com.chuusai"            %% "shapeless"     % versions.shapeless,
  "com.thangiee"           %% "freasy-monad"  % versions.freasy,
  "com.github.mpilquist"   %% "simulacrum"    % versions.simulacrum,
  "org.spire-math"         %% "imp"           % versions.imp % "provided"
)

lazy val datastructs = Seq(
  "org.typelevel"              %% "dogs-core"     % versions.dogs,
  "org.spire-math"             %% "debox"         % versions.debox,
  "com.github.alexandrnikitin" %% "bloom-filter" % versions.bloomfilter
)

lazy val mathlibs = Seq(
  "org.spire-math"         %% "spire"            % versions.spire
)

lazy val monocle = Seq(
  "com.github.julien-truffaut"  %%  "monocle-core"    % versions.monocle,
  "com.github.julien-truffaut"  %%  "monocle-generic" % versions.monocle,
  "com.github.julien-truffaut"  %%  "monocle-macro"   % versions.monocle,
  "com.github.julien-truffaut"  %%  "monocle-state"   % versions.monocle,
  "com.github.julien-truffaut"  %%  "monocle-refined" % versions.monocle,
  "com.github.julien-truffaut"  %%  "monocle-unsafe"  % versions.monocle,
  "com.github.julien-truffaut"  %%  "monocle-law"     % versions.monocle % "test"
                      )

lazy val streamlibs = Seq(
  "co.fs2"                 %% "fs2-core"      % versions.fs2,
  "co.fs2"                 %% "fs2-io"        % versions.fs2,
  "co.fs2"                 %% "fs2-cats"      % versions.fs2Cats
)

lazy val testlibs = Seq(
   "org.scalacheck" %% "scalacheck" % versions.scalacheck,
   "org.scalatest" %% "scalatest" % versions.scalatest,
   "com.storm-enroute" %% "scalameter-core" % versions.scalameter
)

lazy val sharedSettings = Defaults.coreDefaultSettings ++ Seq(
  organization := "co.quine",
  scalaVersion := "2.11.8",
  crossScalaVersions := Seq("2.10.6", "2.11.8"),

  resolvers ++= Seq(
    Resolver.mavenLocal,
    Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("releases"),
    Resolver.typesafeRepo("snapshots"),
    Resolver.jcenterRepo
  ),

  scalacOptions ++= Seq(
    "-encoding", "UTF-8",
    "-deprecation",
    "-optimize",
    "-Xlint",
    "-language:postfixOps",
    "-language:higherKinds",
    "-language:existentials",
    "-language:implicitConversions",
    "-language:experimental.macros"
  ),

  evictionWarningOptions in update := EvictionWarningOptions.default
   .withWarnTransitiveEvictions(false)
   .withWarnDirectEvictions(false)
   .withWarnScalaVersionEviction(false),

  publishMavenStyle := true,
  publishArtifact in Test := false
)

lazy val phronesis = Project(
  id = "phronesis",
  base = file("."),
  settings = sharedSettings
  ).settings(
    test := { },
    publish := { },
    publishLocal := { }
).aggregate(
  phronesisCore
)

def module(name: String) = {
  val id = s"phronesis-$name"
  Project(id = id, base = file(id), settings = sharedSettings ++ Seq(Keys.name := id))
}

lazy val phronesisCore = module("core").settings(
  libraryDependencies ++= (functionalibs ++ datastructs ++ mathlibs ++ monocle ++ streamlibs ++ testlibs),
  addCompilerPlugin("org.scalamacros" % "paradise" % versions.paradise cross CrossVersion.full)
)
