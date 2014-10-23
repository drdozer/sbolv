package sbolv

import java.util.Random

import org.scalajs.dom._
import rx.core.{Var, Obs, Rx}
import sbolv.geom.{Box}

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
  final lazy val glyph: SVGElement = path(`class` := s"sbolv_glyph $cssClass", d := path_d).render
}

object GlyphFamily {
  private var ctr = 0
  trait FixedWidth {
    def apply(direction: HorizontalOrientation, label: Option[String] = None):
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

    val xSgn = verticalOrientation().sgn
    val ySgn = horizontalOrientation().sgn

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

//trait GlyphFamilyWithInnerLabel {
//  self : GlyphFamily =>
//
//  import scalatags.JsDom.implicits._
//  import scalatags.JsDom.svgTags._
//  import scalatags.JsDom.svgAttrs._
//  import Framework._
//
//  def innerLabel: Rx[Option[String]]
//
//  private val innerLabelTick = Var(0)
//  private def tickInner() = innerLabelTick() = innerLabelTick() + 1
//
//  private val innerLabelClean = Rx {
//    innerLabel() getOrElse ""
//  }
//
//  private lazy val innerLabelText_transform = Var("translate(0 0)")
//
//  protected final val innerLabelText = {
//    val txt = text(
//      `class` := "sbolv_glyph_label",
//      transform := (innerLabelText_transform : Rx[String]),
//      DOMNodeInsertedIntoDocument := { (e: Event) =>
//        tickInner() },
//      DOMNodeInserted := { (e: Event) =>
//        tickInner() },
//      DOMSubtreeModified := { (e: Event) =>
//        tickInner() }
//    )(innerLabelClean).render
//
//
//    val textBox = Rx {
//      innerLabelTick()
//      innerLabelClean()
//      Box(txt.getBBox())
//    }
//
//    val innerLabelText_transform_obs = Obs(textBox) {
//      val bounds = textBox()
//      val centre = bounds.centre
//      innerLabelText_transform() = s"translate(${-centre.x} ${-centre.y})"
//    }
//
//    txt
//  }
//
//}
//
//trait GlyphFamilyWithOuterLabel {
//  self : GlyphFamily =>
//
//  import scalatags.JsDom.implicits._
//  import scalatags.JsDom.svgTags._
//  import scalatags.JsDom.svgAttrs._
//  import Framework._
//
//  def outerLabel: Rx[Option[String]]
//  protected def offset: Rx[Double]
//
//  private val outerLabelTick = Var(0)
//  private def tickOuter() = outerLabelTick() = outerLabelTick() + 1
//
//  private lazy val outerLabelClean = Rx {
//    outerLabel() getOrElse ""
//  }
//
//  private lazy val outerLabelText_transform = Var("translate(0 0)")
//
//  protected final val outerLabelText = {
//    val txt = text(
//      `class` := "sbolv_glyph_label",
//      transform := (outerLabelText_transform : Rx[String]),
//      DOMNodeInsertedIntoDocument := { (e: Event) => tickOuter() },
//      DOMNodeInserted := { (e: Event) => tickOuter() },
//      DOMSubtreeModified := { (e: Event) => tickOuter() }
//    )(outerLabelClean).render
//
//    val textBox = Rx {
//      outerLabelTick()
//      outerLabelClean()
//      Box(txt.getBBox())
//    }
//
//    val trans = Rx {
//      val bounds = textBox()
//      val centre = bounds.centre
//      val m = metrics()
//      val o = offset()
//      alignment() match {
//        case AboveBackbone => s"translate(${-centre.x} ${+o +bounds.bottom})"
//        case CentredOnBackbone => s"translate(${-centre.x} ${+o +bounds.bottom})"
//        case BelowBackbone => s"translate(${-centre.x} ${+o +bounds.bottom})"
//      }
//    }
//
//    val transform_obs = Obs(trans) {
//      outerLabelText_transform() = trans()
//    }
//
//    txt
//  }
//}
