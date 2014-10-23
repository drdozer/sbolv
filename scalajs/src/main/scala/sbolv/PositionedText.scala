package sbolv

import org.scalajs.dom.{SVGLocatable, SVGElement, Event}
import rx._
import sbolv.geom.Box
import sbolv.geom.Box.Positioning

import scalatags.JsDom.all._
import scalatags.JsDom.svgTags._
import scalatags.JsDom.svgAttrs._
import Framework._

/**
 *
 *
 * @author Matthew Pocock
 */
case class PositionedText(content: Rx[String], 
                          relativeTo: Rx[Box],
                          hPos: Rx[Positioning],
                          vPos: Rx[Positioning],
                          hAt: Rx[Double], 
                          vAt: Rx[Double])
{

  val txt = text(
    content
  ).render

  val boxOfTxt = BoxOfSVG(txt)

  lazy val relativePosition = RelativePosition(relativeTo, boxOfTxt.boundingBox, hPos, vPos, hAt, vAt)

  lazy val translate = Rx {
    println("Calculating translate")
    val at = relativePosition.at()
    println(s"At $at")
    s"translate(${at.x} ${at.y})"
  }

  lazy val positionedText = g(
    transform := translate,
    txt
  ).render
}

case class RelativePosition(parentBox: Rx[Box],
                            childBox: Rx[Box],
                            hPos: Rx[Positioning],
                            vPos: Rx[Positioning],
                            hAlpha: Rx[Double],
                            vAlpha: Rx[Double])
{
  val placement = Rx {
    parentBox() places childBox()
  }

  val positioned = Rx {
    placement() positionedAt (hPos(), vPos())
  }

  lazy val at = Rx {
    positioned().at(hAlpha(), vAlpha())
  }
}

case class BoxOfSVG(elem: SVGElement with SVGLocatable) {
  println("Creating BoxOfSVG")
  var positionTick = Var(0)
  private def tickPosition() = positionTick() = positionTick() + 1

  println("Created ticker")

  lazy val boundingBox = {
    println("Initializing bounding box events")
    elem.modifyWith(
      Events.DOMNodeInsertedIntoDocument := { (e: Event) => tickPosition() },
      Events.DOMNodeInserted := { (e: Event) => tickPosition() },
      Events.DOMSubtreeModified := { (e: Event) => tickPosition() }
    ).render

    Rx {
      println("Calculating bounding box rx")
      positionTick() // for the dependency
      println("bunding box rx ticked")
      val b = Box(elem.getBBox())
      println(s"Calculated box as $b")
      b
    }
  }
}