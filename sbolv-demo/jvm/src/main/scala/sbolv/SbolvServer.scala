package sbolv

import spray.http.{MediaTypes, MediaType, StatusCodes}

import scala.concurrent.ExecutionContext.Implicits.global
import akka.actor.ActorSystem
import spray.routing.SimpleRoutingApp

import scala.util.{Success, Failure}
/**
 *
 *
 * @author Matthew Pocock
 */
object SbolvServer extends App with SimpleRoutingApp with Demos {

  implicit val system = ActorSystem("app-server")

  startServer(interface = "localhost", port = 9200) {
    pathPrefix("sbolv")  {
      get {
        respondWithMediaType(MediaTypes.`text/html`) {
          (path("index.html") | pathSingleSlash) {
            complete(
              index.render
            )
          }  ~
            path("pigeonParser.html") {
              complete {
                pigeonParser.render
              }
            } ~
            path("fixedOrProportional.html") {
              complete(
                fixedOrProportional.render
              )
            } ~
            path("cds.html") {
              complete {
                cds.render
              }
            } ~
            path("primerBindingSite.html") {
              complete {
                primerBindingSite.render
              }
            } ~
            path("promoter.html") {
              complete {
                promoter.render
              }
            } ~
            path("proteaseSite.html") {
              complete {
                proteaseSite.render
              }
            } ~
            path("proteinStabilityElement.html") {
              complete {
                proteinStabilityElement.render
              }
            } ~
            path("ribonucleaseSite.html") {
              complete {
                ribonucleaseSite.render
              }
            } ~
            path("rnaStabilityElement.html") {
              complete {
                rnaStabilityElement.render
              }
            } ~
            path("terminator.html") {
              complete {
                terminator.render
              }
            }
        }
      }
    } ~
      pathPrefix("public") {
        get {
          getFromResourceDirectory("public")
        }
      }
  }.onComplete {
    case Success(b) =>
      println(s"Successfully bound to ${b.localAddress}")
    case Failure(ex) =>
      println(ex.getMessage)
      system.shutdown()
  }
}
