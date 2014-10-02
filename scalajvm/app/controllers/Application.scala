package controllers

import play.api.mvc._
import play.twirl.api._

import scala.io.Codec

object Application extends Controller {
  implicit val codec = Codec.UTF8

  def cds = Action {
    Ok(views.html.cds())
  }

  def promoter = Action {
    Ok(views.html.promoter())
  }

  def fixedOrProportional = Action {
    Ok(views.html.fixedOrProportional())
  }
}
