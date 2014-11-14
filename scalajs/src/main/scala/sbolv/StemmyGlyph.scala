
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
trait StemmyGlyph extends GlyphFamily {
  override final type Metrics = StemmyGlyph.Metrics
  override final type Geometry = StemmyGlyph.Geometry

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

    val stemSize = m.stemHeight * h
    val stemTop = h2 + (stemSize - h2) * ySgn

    val head2 = stemTop - top
    val headBot = stemTop + head2

    StemmyGlyph.Geometry(top = top, bot = bot, start = start, end = end, xMid = w2, stemTop = stemTop, headBot = headBot)
  }

}

object StemmyGlyph {

  trait Metrics {
    def length: Double
    def depth: Double
    def stemHeight: Double
  }

  object Metrics {
    def apply(length: Double, depth: Double, stemHeight: Double): Metrics = MetricsImpl(length, depth, stemHeight)
  }

  case class MetricsImpl(length: Double, depth: Double, stemHeight: Double) extends Metrics

  case class Geometry(top: Double, bot: Double, start: Double, end: Double, xMid: Double, stemTop: Double, headBot: Double)

  trait BaselineAtBot extends StemmyGlyph {
    override final def geometryToBaseline(g: Geometry) = g.bot
  }
}
