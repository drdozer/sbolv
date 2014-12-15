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
      path("cds.html") {
        get {
          respondWithMediaType(MediaTypes.`text/html`) {
            complete {
              cdsDemo.render
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
