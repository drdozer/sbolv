package controllers

import play.api.mvc._
import play.twirl.api._

import scala.io.Codec

object Application extends Controller {
  implicit val codec = Codec.UTF8

  def demo = Action {
    Ok(views.html.demo())
  }

  def cds = Action {
    Ok(views.html.cds())
  }

  def promoter = Action {
    Ok(views.html.promoter())
  }

  def ribosomeEntrySite = Action {
    Ok(views.html.ribosomeEntrySite())
  }

  def fixedOrProportional = Action {
    Ok(views.html.fixedOrProportional())
  }

  def terminator = Action {
    Ok(views.html.terminator())
  }

  def primerBindingSite = Action {
    Ok(views.html.primerBindingSite())
  }

  def proteaseSite = Action {
    Ok(views.html.proteaseSite())
  }
}
