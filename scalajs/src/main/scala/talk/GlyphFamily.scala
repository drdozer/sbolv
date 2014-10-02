package talk

import org.scalajs.dom._
import rx.core.{Var, Obs, Rx}
import talk.geom.{Box}

/**
 *
 *
 * @author Matthew Pocock
 */
trait GlyphFamily {
  type Metrics

  def glyph: SVGGElement
  def direction: Rx[Direction]
  def alignment: Rx[Alignment]
  def metrics: Rx[Metrics]
}

trait GlyphFamilyWithInnerLabel {
  self : GlyphFamily =>

  import Enhancements._

  def innerLabel: Rx[Option[String]]

  protected final val innerLabelText = "text".asSVGElement[SVGTextElement]("class" -> "sbolv_glyph_label")

  private val innerLabelTick = Var(0)

  private val innerLabelText_transform = Rx {
    innerLabelTick() // for the temporal dependency
    val bounds = Box(innerLabelText.getBBox())
    val centre = bounds.centre
    s"translate(${-centre.x} ${-centre.y})"
  }

  private val innerLabelText_obs = Obs(innerLabel) {
    innerLabelText.textContent = innerLabel().getOrElse("")
    innerLabelTick() = innerLabelTick() + 1
  }

  private val innerLabelText_transform_obs = Obs(innerLabelText_transform) {
    innerLabelText("transform" -> innerLabelText_transform())
  }
}

trait GlyphFamilyWithOuterLabel {
  self : GlyphFamily =>

  import Enhancements._
  def outerLabel: Rx[Option[String]]
  protected def offset: Rx[Double]

  protected final val outerLabelText = "text".asSVGElement[SVGTextElement]("class" -> "sbolv_glyph_label")
  private val outerLabelTick = Var(0)

  private val outerLabelText_obs = Obs(outerLabel) {
    outerLabelText.textContent = outerLabel().getOrElse("")
    outerLabelTick() = outerLabelTick() + 1
  }


  private val outerLabelText_transform = Rx {
    outerLabelTick() // for the temporal dependency
    val bounds = Box(outerLabelText.getBBox())
    val centre = bounds.centre
    val m = metrics()
    val o = offset()
    alignment() match {
      case BackboneCentred => s"translate(${-centre.x} ${+o +bounds.bottom})"
      case BackboneAbove => s"translate(${-centre.x} ${+o +bounds.bottom})"
      case BackboneStrandRelative =>
        direction() match {
          case Forward => s"translate(${-centre.x} ${+o +bounds.bottom})"
          case Reverse => s"translate(${-centre.x} ${+o -bounds.top})"
        }
      case BackboneBelow => s"translate(${-centre.x} ${+o -bounds.top})"
    }
  }

  private val outerLabelText_transform_obs = Obs(outerLabelText_transform) {
    outerLabelText("transform" -> outerLabelText_transform())
  }
}

case class Promoter(direction: Rx[Direction],
                    alignment: Rx[Alignment],
                    outerLabel: Rx[Option[String]],
                    backboneWidth: Rx[Double],
                    metrics: Rx[Promoter.Metrics])
  extends GlyphFamily with GlyphFamilyWithOuterLabel
{
  override type Metrics = Promoter.Metrics

  import Enhancements._

  private val horizontal = "line".asSVGElement[SVGLineElement]("class" -> "promoter_horizontal")
  private val vertical = "line".asSVGElement[SVGLineElement]("class" -> "promoter_vertical")
  private val arrowUpper = "line".asSVGElement[SVGLineElement]("class" -> "promoter_arrow_upper")
  private val arrowLower = "line".asSVGElement[SVGLineElement]("class" -> "promoter_arrow_lower")
  private val arrow = "g".asSVGElement[SVGGElement]("class" -> "promoter_arrow").apply(arrowUpper, arrowLower)

  val path = "g".asSVGElement[SVGGElement]("class" -> "sbolv_glyph").apply(horizontal, vertical, arrow)

  val glyph = "g".asSVGElement[SVGGElement]("class" -> "sbolv promoter").apply(path, outerLabelText)

  override protected def offset = Rx {
    if(transMetrics != null) {
      val m = transMetrics()
      m.vertical + m.arrowHeight
    } else {
      0.0 // initialization order bug
    }
  }

  private val transMetrics = Rx {
    val m = metrics()

    val v = (direction(), alignment()) match {
      case (Forward, BackboneCentred | BackboneStrandRelative | BackboneAbove) | (Reverse, BackboneAbove) =>
        -m.vertical
      case (Forward, BackboneBelow) | (Reverse, BackboneCentred | BackboneStrandRelative | BackboneBelow) =>
        +m.vertical
    }

    val h = direction() match {
      case Forward =>
        +m.horizontal
      case Reverse =>
        -m.horizontal
    }

    val ah = (direction(), alignment()) match {
      case (Forward, BackboneCentred | BackboneStrandRelative | BackboneAbove) | (Reverse, BackboneAbove) =>
        -m.arrowHeight
      case (Forward, BackboneBelow) | (Reverse, BackboneCentred | BackboneStrandRelative | BackboneBelow) =>
        +m.arrowHeight
    }

    val aw = direction() match {
      case Forward =>
        +m.arrowWidth
      case Reverse =>
        -m.arrowWidth
    }

    Promoter.Metrics(v, h, ah, aw)
  }

  private val vertical_obs = Obs(transMetrics) {
    val m = transMetrics()
    vertical(
      "x1" -> 0.toString,
      "y1" -> 0.toString,
      "x2" -> 0.toString,
      "y2" -> m.vertical.toString)
  }
  private val horizontal_obs = Obs(transMetrics) {
    val m = transMetrics()
    horizontal(
      "x1" -> 0.toString,
      "y1" -> m.vertical.toString,
      "x2" -> m.horizontal.toString,
      "y2" -> m.vertical.toString)
  }
  private val arrowUpper_obs = Obs(transMetrics) {
    val m = transMetrics()
    arrowUpper(
      "x1" -> m.horizontal.toString,
      "y1" -> m.vertical.toString,
      "x2" -> (m.horizontal - m.arrowWidth).toString,
      "y2" -> (m.vertical - m.arrowHeight).toString)
  }
  private val arrowLower_obs = Obs(transMetrics) {
    val m = transMetrics()
    arrowLower(
      "x1" -> m.horizontal.toString,
      "y1" -> m.vertical.toString,
      "x2" -> (m.horizontal - m.arrowWidth).toString,
      "y2" -> (m.vertical + m.arrowHeight).toString)
  }

  private val glyph_transform = Rx {
    val offset = backboneWidth()
    alignment() match {
      case BackboneCentred => "translate(0 0)"
      case BackboneAbove => s"translate(0 ${-offset})"
      case BackboneStrandRelative => direction() match {
        case Forward => s"translate(0 ${-offset})"
        case Reverse => s"translate(0 ${+offset})"
      }
      case BackboneBelow => s"translate(0 ${+offset})"
    }
  }

  private val glyph_transform_obs = Obs(glyph_transform) {
    glyph("transform" -> glyph_transform())
  }
}

object Promoter {
  trait Metrics {
    def vertical: Double
    def horizontal: Double
    def arrowHeight: Double
    def arrowWidth: Double
  }

  object Metrics {
    def apply(vertical: Double, horizontal: Double, arrowHeight: Double, arrowWidth: Double): Metrics =
      MetricsImpl(vertical, horizontal, arrowHeight, arrowWidth)
  }

  case class MetricsImpl(vertical: Double, horizontal: Double, arrowHeight: Double, arrowWidth: Double) extends Metrics
}

case class Cds(direction: Rx[Direction],
               alignment: Rx[Alignment],
               innerLabel: Rx[Option[String]],
               outerLabel: Rx[Option[String]],
               backboneWidth: Rx[Double],
               metrics: Rx[Cds.Metrics])
  extends GlyphFamily with GlyphFamilyWithInnerLabel with GlyphFamilyWithOuterLabel
{
  override type Metrics = Cds.Metrics

  import Enhancements._

  override protected def offset = Rx {
    (direction(), alignment()) match {
      case (Forward, BackboneAbove | BackboneCentred | BackboneStrandRelative) | (Reverse, BackboneAbove) =>
        -metrics().depth
      case (Forward, BackboneBelow) | (Reverse, BackboneCentred | BackboneStrandRelative | BackboneBelow) =>
        +metrics().depth
    }
  }

  private val path = "path".asSVGElement[SVGPathElement]("class" -> "sbolv_glyph")

  val glyph = "g".asSVGElement[SVGGElement](
    "class" -> "sbolv cds"
  ) apply (path, innerLabelText, outerLabelText)

  private val path_d = Rx {
    val m = metrics()
    direction() match {
      case Forward => s"M${ m.l2} 0 L${ m.l2h} ${-m.d2} H${-m.l2} v${ m.depth} H${ m.l2h} L${ m.l2} 0 Z"
      case Reverse => s"M${-m.l2} 0 L${-m.l2h} ${ m.d2} H${ m.l2} v${-m.depth} H${-m.l2h} L${-m.l2} 0 Z"
    }
  }

  private val glyph_transform = Rx {
    val m = metrics()
    val offset = m.d2 + backboneWidth()
    alignment() match {
      case BackboneCentred => "translate(0 0)"
      case BackboneAbove => s"translate(0 ${-offset})"
      case BackboneStrandRelative => direction() match {
        case Forward => s"translate(0 ${-offset})"
        case Reverse => s"translate(0 ${+offset})"
      }
      case BackboneBelow => s"translate(0 ${+offset})"
    }
  }

  private val path_d_obs = Obs(path_d) {
    path("d" -> path_d())
  }

  private val glyph_transform_obs = Obs(glyph_transform) {
    glyph("transform" -> glyph_transform())
  }

}

object Cds {

  trait Metrics {
    def length: Double
    def depth: Double

    def head: Double = length - body
    def body: Double = length - head

    def l2 = length / 2.0
    def d2 = depth / 2.0
    def l2h = l2 - head
  }

  object Metrics {
    def apply(length: Double, depth: Double, head: Double): Metrics = MetricsImpl(length, depth, head)
  }

  case class MetricsImpl(length: Double, depth: Double, override val head: Double) extends Metrics
}
