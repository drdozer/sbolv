package talk

import org.scalajs.dom.{SVGPathElement, SVGGElement}
import rx.core.{Obs, Rx, Var}


case class Promoter(direction: Rx[Direction],
                    alignment: Rx[BackboneAlignment],
                    outerLabel: Rx[Option[String]],
                    backboneWidth: Rx[Double],
                    metrics: Rx[Promoter.Metrics])
  extends GlyphFamily with GlyphFamilyWithOuterLabel
{
  override type Metrics = Promoter.Metrics

  import Enhancements._

  val path = "path".asSVGElement[SVGPathElement]("class" -> "sbolv_glyph")

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
      case (Rightwards, CentredOnBackbone | AboveBackbone) | (Leftwards, AboveBackbone) =>
        -m.vertical
      case (Rightwards, BelowBackbone) | (Leftwards, CentredOnBackbone | BelowBackbone) =>
        +m.vertical
    }

    val h = direction() match {
      case Rightwards =>
        +m.horizontal
      case Leftwards =>
        -m.horizontal
    }

    val ah = (direction(), alignment()) match {
      case (Rightwards, CentredOnBackbone | AboveBackbone) | (Leftwards, AboveBackbone) =>
        -m.arrowHeight
      case (Rightwards, BelowBackbone) | (Leftwards, CentredOnBackbone | BelowBackbone) =>
        +m.arrowHeight
    }

    val aw = direction() match {
      case Rightwards =>
        +m.arrowWidth
      case Leftwards =>
        -m.arrowWidth
    }

    Promoter.Metrics(v, h, ah, aw)
  }

  private val path_d = Rx {
    val m = transMetrics()
    s"M0 0 V${m.vertical} H${m.horizontal} M${m.horizontal - m.arrowWidth} ${m.vertical - m.arrowHeight} L${m.horizontal} ${m.vertical} L${m.horizontal - m.arrowWidth} ${m.vertical + m.arrowHeight}"
  }

  private val path_d_obs = Obs(path_d) {
    path("d" -> path_d())
  }

  private val glyph_transform = Rx {
    val offset = backboneWidth()
    alignment() match {
      case CentredOnBackbone => "translate(0 0)"
      case AboveBackbone => s"translate(0 ${-offset})"
      case BelowBackbone => s"translate(0 ${+offset})"
    }
  }

  private val glyph_transform_obs = Obs(glyph_transform) {
    glyph("transform" -> glyph_transform())
  }
}

object Promoter {
  def fixedWidth(direction: Direction): (Rx[Double], Rx[BackboneAlignment]) => GlyphFamily = (width, alignment) =>
    Promoter(Var(direction), alignment, Var(None), Var(0), Rx {
      val w = width() * 0.9
      Metrics(w * 0.4, w * 0.4, w * 0.1, w * 0.1)
    })

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
