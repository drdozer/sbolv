package sbolv

import org.scalajs.dom.Node
import rx.core.{Rx, Var}


case class Promoter(horizontalOrientation: Rx[HorizontalOrientation],
                    alignment: Rx[BackboneAlignment],
                    outerLabel: Rx[Option[String]],
                    backboneWidth: Rx[Double],
                    metrics: Rx[Promoter.Metrics])
  extends GlyphFamily with GlyphFamilyWithOuterLabel
{
  override type Metrics = Promoter.Metrics

  import scalatags.JsDom.all.bindNode
  import scalatags.JsDom.implicits._
  import scalatags.JsDom.svgTags._
  import scalatags.JsDom.svgAttrs._
  import Framework._

  override protected lazy val offset = Rx {
    if(transMetrics != null) {
      val m = transMetrics()
      m.vertical + m.arrowHeight
    } else {
      0.0 // initialization order bug
    }
  }

  private val transMetrics = Rx {
    val m = metrics()

    val v = (horizontalOrientation(), alignment()) match {
      case (Rightwards, CentredOnBackbone | AboveBackbone) | (Leftwards, AboveBackbone) =>
        -m.vertical
      case (Rightwards, BelowBackbone) | (Leftwards, CentredOnBackbone | BelowBackbone) =>
        +m.vertical
    }

    val h = horizontalOrientation() match {
      case Rightwards =>
        +m.horizontal
      case Leftwards =>
        -m.horizontal
    }

    val ah = (horizontalOrientation(), alignment()) match {
      case (Rightwards, CentredOnBackbone | AboveBackbone) | (Leftwards, AboveBackbone) =>
        -m.arrowHeight
      case (Rightwards, BelowBackbone) | (Leftwards, CentredOnBackbone | BelowBackbone) =>
        +m.arrowHeight
    }

    val aw = horizontalOrientation() match {
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

  private val glyph_transform = Rx {
    val offset = backboneWidth()
    alignment() match {
      case CentredOnBackbone => "translate(0 0)"
      case AboveBackbone => s"translate(0 ${-offset})"
      case BelowBackbone => s"translate(0 ${+offset})"
    }
  }

  private val glyphPath = path(
    `class` := "sbolv_glyph",
    d := path_d)

  val glyph = g(
    `class` := "sbolv promoter",
    transform := glyph_transform)(glyphPath, outerLabelText).render
}

object Promoter {

  object FixedWidth extends GlyphFamily.FixedWidth {
    def apply(direction: HorizontalOrientation, label: Option[String] = None): (Rx[Double], Rx[BackboneAlignment]) => GlyphFamily = (width, alignment) =>
      Promoter(Var(direction), alignment, Var(label), Var(0), Rx {
        val w = width() * 0.9
        Metrics(w * 0.4, w * 0.4, w * 0.1, w * 0.1)
      })
  }

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

  trait SCProvider extends ShortcodeProvider {
    import scalatags.JsDom.all.bindNode
    import scalatags.JsDom.implicits._
    import scalatags.JsDom.svgTags._
    import scalatags.JsDom.svgAttrs._

    private val promoterHandler: PartialFunction[Shortcode, Node] = {
      case Shortcode("promoter", attrs, content) =>
        val attrsM = attrs.toMap
        val wdth = attrsM.get("width").map(_.toDouble).getOrElse(50.0)
        val dir = asDirection(attrsM.get("dir"))
        val promoter = FixedWidth(dir, content).apply(Var(wdth), Var(AboveBackbone))

        val hoff = 0.3 * (dir match {
          case Rightwards => -1
          case Leftwards => 1
        })

        svg(width := wdth, height := wdth * 0.5, `class` := "sbolv_inline")(
          g(transform := s"translate(${wdth * (0.5 + hoff)} ${wdth * 0.47})")(promoter.glyph)
        ).render
    }

    abstract override def shortcodeHandlers(sc: Shortcode) = super.shortcodeHandlers(sc) orElse promoterHandler.lift(sc)

  }

  trait FWSC extends FixedWidthShorcodeContent {
    abstract override def Code(c: String) = if(c == "p") FixedWidth else super.Code(c)
  }
}
