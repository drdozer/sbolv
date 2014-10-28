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
trait GlyphFamily {
  /** The type describing the vital statistics of the concrete glyph. These are provided as proportions 0..1 in terms
    * of the width/height box. */
  type Metrics

  /** The type describing the fully scaled geometry of the concrete glyph. These are usually key x/y offsets in pixels
    * that mark out the glyph's shape. */
  type Geometry

  /** Conversion from metrics to geometry. This will be called within an Rx, so can rely upon other Rx variables. */
  protected def metricsToGeometry(m: Metrics): Geometry

  /** The geometry of this glyph. */
  lazy val geometry = Rx {
    metricsToGeometry(metrics())
  }

  /** Conversion from geometry to a SVG d string for a path. */
  protected def geometryToPath(g: Geometry): String

  private val path_d = Rx {
    geometryToPath(geometry())
  }

  def cssClass: String

  // reactive variables defining the glyph
  def horizontalOrientation: Rx[HorizontalOrientation]
  def verticalOrientation: Rx[VerticalOrientation]
  def width: Rx[Double]
  def height: Rx[Double]
  def metrics: Rx[Metrics]

  /** Calculate the baseline from the geometry. This will be called within an Rx, so can rely upon other Rx variables.
    *  */
  def geometryToBaseline(g: Geometry): Double

  /** The baseline of the glyph, were the DNA backbone would go. This may run through the centre of a strand-centred
    * glyph such as the CDS, or through the base of a strand-anchored glyph, such as the promoter, or at any other
    * point. */
  lazy val baseline = Rx {
    geometryToBaseline(geometry())
  }

  /** The SVG element that this instance manages. */
  final lazy val glyph: SVGElement with SVGLocatable = path(`class` := s"sbolv_glyph $cssClass", d := path_d).render
}

object GlyphFamily {
  private var ctr = 0
  trait FixedWidth {
    def apply(direction: HorizontalOrientation):
    (Rx[Double], Rx[VerticalOrientation]) => GlyphFamily
    val uuid: Int = {
      val u = ctr
      ctr = ctr + 1
      u
    }
  }

  object FixedWidth {
    implicit val ordering: Ordering[FixedWidth] = Ordering.by(_.uuid)
  }
}

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