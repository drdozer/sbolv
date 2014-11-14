
package sbolv

import org.scalajs.dom._
import rx.core.Rx

import scalatags.JsDom.implicits._
import scalatags.JsDom.svgAttrs._
import scalatags.JsDom.svgTags._
import Framework._

/**
 *
 *
 * @author Matthew Pocock
 */
trait BoxyGlyph extends GlyphFamily {
  override final type Metrics = BoxyGlyph.Metrics
  override final type Geometry = BoxyGlyph.Geometry

  override final def metricsToGeometry(m: Metrics) = {
    val w = width()
    val w2 = w * 0.5
    val h = height()
    val h2 = h * 0.5

    val length = m.length * w
    val l2 = length * 0.5
    val depth = m.depth * h
    val d2 = depth * 0.5

    val xSgn = horizontalOrientation().sgn
    val ySgn = verticalOrientation().sgn

    val top = h2 - ySgn * d2
    val bot = h2 + ySgn * d2

    val start = w2 - xSgn * l2
    val end = w2 + xSgn * l2

    BoxyGlyph.Geometry(top = top, bot = bot, start = start, end = end, xMid = w2, yMid = h2)
  }

}

object BoxyGlyph {

  trait Metrics {
    def length: Double
    def depth: Double
  }

  object Metrics {
    def apply(length: Double, depth: Double): Metrics = MetricsImpl(length, depth)
  }

  case class MetricsImpl(length: Double, depth: Double) extends Metrics

  case class Geometry(top: Double, bot: Double, start: Double, end: Double, xMid: Double, yMid: Double)

  trait BaselineAtBot extends BoxyGlyph {
    override final def geometryToBaseline(g: Geometry) = g.bot
  }
}
