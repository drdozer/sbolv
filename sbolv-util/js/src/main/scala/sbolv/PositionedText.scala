package sbolv

import org.scalajs.dom._
import rx._
import scala.scalajs.js.Dynamic
import scalatags.JsDom
import JsDom.all._
import JsDom.svgTags._
import JsDom.svgAttrs._
import scalatags.ext.Framework._

import sbolv.geom.Box

/**
 *
 *
 * @author Matthew Pocock
 */
case class PositionedText(content: Rx[Option[String]],
                          relativeTo: Rx[Box],
                          hPos: Rx[Box.Positioning],
                          vPos: Rx[Box.Positioning],
                          hAt: Rx[Double], 
                          vAt: Rx[Double])
{

  val txt = text(
    content
  ).render

  val boxOfTxt = BoxOfSVG(txt)

  lazy val relativePosition = RelativePosition(relativeTo, boxOfTxt.boundingBox, hPos, vPos, hAt, vAt)

  lazy val translate = Rx {
    val at = relativePosition.at()
    println(s"Placing text at $at")
    s"translate(${at.x} ${at.y})"
  }

  lazy val positionedText = g(
    transform := translate,
    txt
  ).render
}

case class RelativePosition(parentBox: Rx[Box],
                            childBox: Rx[Box],
                            hPos: Rx[Box.Positioning],
                            vPos: Rx[Box.Positioning],
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
  var positionTick = Var(0)
  private def tickPosition() = positionTick() = positionTick() + 1

  lazy val boundingBox = {
    // fixme: this is the only reliable way I can find to track the bounding box - looservile
    Dynamic.global.window.setInterval({() => tickPosition()}, 1000 / 10)

    Rx {
      positionTick() // for the dependency
      val b = Box(elem.getBBox())
      b
    }
  }
}