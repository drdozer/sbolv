package sbolv

import org.scalajs.dom.Node
import rx.core.{Rx, Var}


final case class Terminator(direction: Rx[Direction],
               alignment: Rx[BackboneAlignment],
               outerLabel: Rx[Option[String]],
               backboneWidth: Rx[Double],
               metrics: Rx[Terminator.Metrics])
  extends GlyphFamily with GlyphFamilyWithOuterLabel
{
  override type Metrics = Terminator.Metrics

  import scalatags.JsDom.all.bindNode
  import scalatags.JsDom.implicits._
  import scalatags.JsDom.svgTags._
  import scalatags.JsDom.svgAttrs._
  import Framework._

  override protected lazy val offset = Rx {
    (direction(), alignment()) match {
      case (Rightwards, AboveBackbone | CentredOnBackbone) | (Leftwards, AboveBackbone) =>
        -metrics().depth
      case (Rightwards, BelowBackbone) | (Leftwards, CentredOnBackbone | BelowBackbone) =>
        +metrics().depth
    }
  }

  private val glyphPath_d = Rx {
    val m = metrics()
    direction() match {
      case Rightwards => s"M${-m.l2} ${-m.d2} L${m.l2} ${-m.d2} M0 ${-m.d2} L0 ${m.d2} Z"
      case Leftwards => s"M${-m.l2} ${m.d2} L${m.l2} ${m.d2} M0 ${m.d2} L0 ${-m.d2} Z"
    }
  }

  private val glyph_transform = Rx {
    val m = metrics()
    val offset = m.d2 + backboneWidth()
    alignment() match {
      case CentredOnBackbone => "translate(0 0)"
      case AboveBackbone => s"translate(0 ${-offset})"
      case BelowBackbone => s"translate(0 ${+offset})"
    }
  }

  private val glyphPath = path(`class` := "sbolv_glyph", d := glyphPath_d)

  override val glyph = g(
    `class` := "sbolv term",
    transform := glyph_transform
  )(glyphPath, outerLabelText).render
}

object Terminator {
  object FixedWidth extends GlyphFamily.FixedWidth {
    def apply(direction: Direction, label: Option[String] = None): (Rx[Double], Rx[BackboneAlignment]) => GlyphFamily = (width, alignment) =>
      Terminator(Var(direction), alignment, Var(label), Var(0), Rx {
        val w = width() * 0.9
        new Metrics {
          def length = w * 0.66
          def depth = w * 0.5
        }
      })
  }

  trait Metrics {
    def length: Double
    def depth: Double

    def d2 = depth / 2.0
    def l2 = length / 2.0
  }

  object Metrics {
    def apply(length: Double, depth: Double): Metrics = MetricsImpl(length, depth)
  }

  case class MetricsImpl(length: Double, depth: Double) extends Metrics

  trait SCProvider extends ShortcodeProvider {
    import scalatags.JsDom.all.bindNode
    import scalatags.JsDom.implicits._
    import scalatags.JsDom.svgTags._
    import scalatags.JsDom.svgAttrs._

    private val termHandler: PartialFunction[Shortcode, Node] = {
      case Shortcode("term", attrs, content) =>
        val attrsM = attrs.toMap
        val wdth = attrsM.get("width").map(_.toDouble).getOrElse(50.0)
        val dir = asDirection(attrsM.get("dir"))
        val term = FixedWidth(dir, content).apply(Var(wdth), Var(AboveBackbone))

        svg(width := wdth, height := wdth * 0.5, `class` := "sbolv_inline")(
          g(transform := s"translate(${wdth * 0.5} ${wdth * 0.47})")(term.glyph)
        ).render
    }

    abstract override def shortcodeHandlers(sc: Shortcode) = super.shortcodeHandlers(sc) orElse termHandler.lift(sc)
  }

  trait FWSC extends FixedWidthShorcodeContent {
    abstract override def Code(c: String) = if(c == "t") FixedWidth else super.Code(c)
  }
}
