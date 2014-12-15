import com.typesafe.sbt.less.Import.LessKeys
import com.typesafe.sbt.web.Import.{WebKeys, Assets}
import com.typesafe.sbt.web.SbtWeb
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
  lazy val utilPlatformJs     = svUtil.jsProject(utilSharedJs).
    settings(utilPlatformJsSettings : _*)
  lazy val utilSharedJvm      = svUtil.jvmShared()
  lazy val utilSharedJs       = svUtil.jsShared(utilSharedJvm)

  val svWidgets               = XModule(id = "sbolv-widgets", defaultSettings = buildSettings, baseDir = "sbolv-widgets")
  lazy val widgets            = svWidgets.project(widgetsPlatformJvm, widgetsPlatformJs)
  lazy val widgetsPlatformJvm = svWidgets.jvmProject(widgetsSharedJvm).dependsOn(utilPlatformJvm)
  lazy val widgetsPlatformJs  = svWidgets.jsProject(widgetsSharedJs).dependsOn(utilPlatformJs).
    settings(widgetsPlatformJsSettings : _*)
  lazy val widgetsSharedJvm   = svWidgets.jvmShared().dependsOn(utilSharedJvm)
  lazy val widgetsSharedJs    = svWidgets.jsShared(widgetsSharedJvm).dependsOn(utilSharedJs)

  val svExample               = XModule(id = "sbolv-demo", defaultSettings = buildSettings, baseDir = "sbolv-demo")
  lazy val demo               = svExample.project(demoPlatformJvm, demoPlatformJs)
  lazy val demoPlatformJvm    = svExample.jvmProject(demoSharedJvm).dependsOn(widgetsPlatformJvm).enablePlugins(SbtWeb).
    settings(demoPlatformJvmSettings : _*)
  lazy val demoPlatformJs     = svExample.jsProject(demoSharedJs).dependsOn(widgetsPlatformJs)
  lazy val demoSharedJvm      = svExample.jvmShared().dependsOn(widgetsSharedJvm)
  lazy val demoSharedJs       = svExample.jsShared(demoSharedJvm).dependsOn(widgetsSharedJs)

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

  lazy val utilPlatformJsSettings = Seq(
    libraryDependencies ++= Seq(
      "uk.co.turingatemyhamster" %%% "scalatags-ext" % "0.1.1",
      "com.scalatags" %%% "scalatags" % "0.4.2",
      "com.scalarx" %%% "scalarx" % "0.2.6",
      "org.scala-lang.modules.scalajs" %%% "scalajs-dom" % "0.6"
    )
  )

  lazy val widgetsPlatformJsSettings = Seq(
    libraryDependencies ++= Seq(
      "org.scalajs" %%% "scala-parser-combinators" % "1.0.2"
    )
  )

  lazy val demoPlatformJvmSettings = packAutoSettings ++ Revolver.settings ++ Seq(
    libraryDependencies ++= Seq(
      "io.spray" %% "spray-routing" % "1.3.2",
      "io.spray" %% "spray-can" % "1.3.2",
      "com.typesafe.akka" %% "akka-actor" % "2.3.7",
      "com.scalatags" %% "scalatags" % "0.4.2"
    ),
    ScalaJSKeys.emitSourceMaps := true,
    (crossTarget in (demoPlatformJs, Compile, fastOptJS)) := crossTarget.value / "classes" / "public" / "javascript",
    (resources in Compile) += {
      (fastOptJS in (demoPlatformJs, Compile)).value
      (artifactPath in (demoPlatformJs, Compile, fastOptJS)).value
    },
    includeFilter in (Assets, LessKeys.less) := "*.less",
    excludeFilter in (Assets, LessKeys.less) := "_*.less",
    WebKeys.packagePrefix in Assets := "public/",
    (managedClasspath in Runtime) += (packageBin in Assets).value
  )


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
