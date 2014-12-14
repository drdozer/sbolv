import sbt._
import sbt.Keys._
import com.inthenow.sbt.scalajs._
import com.inthenow.sbt.scalajs.SbtScalajs._
import scala.scalajs.sbtplugin.ScalaJSPlugin._
import ScalaJSKeys._
import bintray.Plugin._
import bintray.Keys._
import org.eclipse.jgit.lib._
import xerial.sbt.Pack._
import spray.revolver.RevolverPlugin._

object SbolvBuild extends Build {

  val logger = ConsoleLogger()

  val baseVersion = "0.1.2"

  val svUtil                  = XModule(id = "sbolv-util", defaultSettings = buildSettings, baseDir = "sbolv-util")
  lazy val util               = svUtil.project(utilPlatformJvm, utilPlatformJs)
  lazy val utilPlatformJvm    = svUtil.jvmProject(utilSharedJvm)
  lazy val utilPlatformJs     = svUtil.jsProject(utilSharedJs)
  lazy val utilSharedJvm      = svUtil.jvmShared()
  lazy val utilSharedJs       = svUtil.jsShared(utilSharedJvm)

  val svWidgets               = XModule(id = "sbolv-widgets", defaultSettings = buildSettings, baseDir = "sbolv-widgets")
  lazy val widgets            = svWidgets.project(widgetsPlatformJvm, widgetsPlatformJs)
  lazy val widgetsPlatformJvm = svWidgets.jvmProject(widgetsSharedJvm)
  lazy val widgetsPlatformJs  = svWidgets.jsProject(widgetsSharedJs)
  lazy val widgetsSharedJvm   = svWidgets.jvmShared()
  lazy val widgetsSharedJs    = svWidgets.jsShared(widgetsSharedJvm)

  val svExample               = XModule(id = "sbolv-demo", defaultSettings = buildSettings, baseDir = "sbolv-demo")
  lazy val demo            = svExample.project(demoPlatformJvm, demoPlatformJs)
  lazy val demoPlatformJvm = svExample.jvmProject(demoSharedJvm)
  lazy val demoPlatformJs  = svExample.jsProject(demoSharedJs)
  lazy val demoSharedJvm   = svExample.jvmShared()
  lazy val demoSharedJs    = svExample.jsShared(demoSharedJvm)

  
  lazy val buildSettings: Seq[Setting[_]] = bintrayPublishSettings ++ Seq(
    organization := "uk.co.turingatemyhamster",
    scalaVersion := "2.11.4",
    crossScalaVersions := Seq("2.11.4", "2.10.4"),
    scalacOptions ++= Seq("-deprecation", "-unchecked"),
    version := makeVersion(baseVersion),
    resolvers += Resolver.url(
      "bintray-scalajs-releases",
      url("http://dl.bintray.com/scala-js/scala-js-releases/"))(
        Resolver.ivyStylePatterns),
    resolvers ++= Seq("snapshots", "releases").map(Resolver.sonatypeRepo),
    resolvers += "spray repo" at "http://repo.spray.io",
    resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases",
    resolvers += "drdozer Bintray Repo" at "http://dl.bintray.com/content/drdozer/maven",
    publishMavenStyle := true,
    repository in bintray := "maven",
    bintrayOrganization in bintray := None,
    licenses +=("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0.html"))
  )

/*
  lazy val scalajvm = Project(
    id = "scalajvm",
    base = file("scalajvm")
  ) enablePlugins (play.PlayScala) settings (scalajvmSettings: _*) aggregate (scalajs)

  lazy val scalajs = Project(
    id   = "scalajs",
    base = file("scalajs")
  ) settings (scalajsSettings: _*)

  lazy val sharedScala = Project(
    id = "sharedScala",
    base = file(sharedSrcDir)
  ) settings (sharedScalaSettings: _*)

  lazy val scalajvmSettings =
    Seq(
      name := "sbolv_jvm",
      version := Versions.app,
      scalaVersion := Versions.scala,
      scalajsOutputDir := (crossTarget in Compile).value / "classes" / "public" / "javascripts",
      includeFilter in (Assets, LessKeys.less) := "*.less",
      compile in Compile <<= (compile in Compile) dependsOn (fastOptJS in (scalajs, Compile)),
      dist <<= dist dependsOn (fullOptJS in (scalajs, Compile)),
      libraryDependencies ++= scalajvmDependencies,
      commands += preStartCommand,
      EclipseKeys.skipParents in ThisBuild := false,
      resolvers += Resolver.url("intact.nexus",
        url("http://www.ebi.ac.uk/intact/maven/nexus/content/groups/public")),
      resolvers += Resolver.url("scala-js-releases",
        url("http://dl.bintray.com/content/scala-js/scala-js-releases"))(
        Resolver.ivyStylePatterns)
    ) ++ (
      // ask scalajs project to put its outputs in scalajsOutputDir
      Seq(packageExternalDepsJS, packageInternalDepsJS, packageExportedProductsJS, packageLauncher, fastOptJS, fullOptJS) map { packageJSKey =>
        crossTarget in (scalajs, Compile, packageJSKey) := scalajsOutputDir.value
      }
    ) ++ sharedDirectorySettings

  lazy val scalajsSettings =
    scalaJSSettings ++ Seq(
      name := "sbolv_js",
      version := Versions.app,
      scalaVersion := Versions.scala,
      persistLauncher := true,
      persistLauncher in Test := false,
      libraryDependencies ++= scalajsDependencies
    ) ++ sharedDirectorySettings

  lazy val sharedScalaSettings =
    Seq(
      name := "sbolv_shared",
      EclipseKeys.skipProject := true,
      libraryDependencies ++= sharedDependencies
    )

  lazy val sharedDirectorySettings = Seq(
    unmanagedSourceDirectories in Compile += new File((file(".") / sharedSrcDir / "src" / "main" / "scala").getCanonicalPath),
    unmanagedSourceDirectories in Test += new File((file(".") / sharedSrcDir / "src" / "test" / "scala").getCanonicalPath),
    unmanagedResourceDirectories in Compile += file(".") / sharedSrcDir / "src" / "main" / "resources",
    unmanagedResourceDirectories in Test += file(".") / sharedSrcDir / "src" / "test" / "resources"
  )

  // Use reflection to rename the 'start' command to 'play-start'
  Option(play.Play.playStartCommand.getClass.getDeclaredField("name")) map { field =>
    field.setAccessible(true)
    field.set(playStartCommand, "play-start")
  }

  // The new 'start' command optimises the JS before calling the Play 'start' renamed 'play-start'
  val preStartCommand = Command.args("start", "<port>") { (state: State, args: Seq[String]) =>
    Project.runTask(fullOptJS in (scalajs, Compile), state)
    state.copy(remainingCommands = ("play-start " + args.mkString(" ")) +: state.remainingCommands)
  }

  lazy val sharedDependencies = Seq(
    "com.lihaoyi" %%%! "upickle" % Versions.upickle,
    "com.scalarx" %%%! "scalarx" % Versions.scalarx,
    "org.scalajs" %%%! "scala-parser-combinators" % Versions.parserCombinators )

  lazy val scalajvmDependencies = Seq(
//    "uk.ac.ebi.chebi.webapps.chebiWS.client" % "chebiWS-client" % Versions.chebiWsClient,
    "uk.co.turingatemyhamster" %% "scalajs-ext" % Versions.scalajs_ext,
    "com.lihaoyi" %% "upickle" % Versions.upickle,
    "org.webjars" % "jquery" % Versions.jquery
  ) ++ sharedDependencies

  lazy val scalajsDependencies = Seq(
    "org.scala-lang" % "scala-reflect" % Versions.scala,
    "com.scalatags" %%%! "scalatags" % Versions.scalatags,
    "org.scala-lang.modules.scalajs" %%%! "scalajs-dom" % Versions.scalajsDom,
    "uk.co.turingatemyhamster" %%%! "scalajs-ext_js" % Versions.scalajs_ext,
    "org.scala-lang.modules.scalajs" %% "scalajs-jasmine-test-framework" % scalaJSVersion % "test"
  ) ++ sharedDependencies

*/

  def fetchGitBranch(): String = {
    val builder = new RepositoryBuilder()
    builder.setGitDir(file(".git"))
    val repo = builder.readEnvironment().findGitDir().build()
    val gitBranch = repo.getBranch
    logger.info(s"Git branch reported as: $gitBranch")
    repo.close()
    val travisBranch = Option(System.getenv("TRAVIS_BRANCH"))
    logger.info(s"Travis branch reported as: $travisBranch")

    travisBranch getOrElse gitBranch

    val branch = (travisBranch getOrElse gitBranch) replaceAll ("/", "_")
    logger.info(s"Computed branch is $branch")
    branch
  }

  def makeVersion(baseVersion: String): String = {
    val branch = fetchGitBranch()
    if(branch == "master") {
      baseVersion
    } else {
      val tjn = Option(System.getenv("TRAVIS_JOB_NUMBER"))
      s"$branch-$baseVersion${
        tjn.map("." + _) getOrElse ""
      }"
    }
  }

}

object Versions {
  val app = "0.1.1-SNAPSHOT"
  val scala = "2.11.2"
  val scalatags = "0.4.1"
  val scalajsDom = "0.6"
  val scalarx = "0.2.6"
  val upickle = "0.2.0"
  val jquery = "1.9.0"
  val chebiWsClient = "2.2.1"
  val parserCombinators = "1.0.2"
  val scalajs_ext = "0.1-SNAPSHOT"
}

