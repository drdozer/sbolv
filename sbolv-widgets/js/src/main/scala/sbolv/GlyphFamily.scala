package sbolv

import rx._
import org.scalajs.dom._
import scala.Predef.String
import scalatags.JsDom
import scalatags.ext._
import JsDom.all._
import JsDom.svgTags._
import JsDom.svgAttrs.{`class` => _, _}
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
  def takeFixedWidthId(): Int = {
    val c = ctr
    ctr += 1
    c
  }
  trait GlyphType {
    def apply(boxWidthHeight: Rx[Double],
              horizontalOrientation: Rx[HorizontalOrientation],
              verticalOrientation: Rx[VerticalOrientation],
              stroke: Rx[Option[String]] = Var(None),
              fill: Rx[Option[String]] = Var(None),
              cssClasses: Rx[Seq[String]] = Var(Seq()),
              label: Rx[Option[String]] = Var(None)): GlyphFamily

    def fixedWidthId: Int

    override def toString = super.toString + "#" + fixedWidthId
  }

  case class GlyphSpec(glyphType: GlyphType,
                       horizontalOrientation: HorizontalOrientation,
                       verticalOrientation: Option[VerticalOrientation] = None,
                       stroke: Option[String] = None,
                       fill: Option[String] = None,
                       cssClasses: Seq[String] = Seq(),
                       label: Option[String] = None)
}

