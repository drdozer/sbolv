package talk

import org.scalajs.dom.{Node, SVGGElement, SVGPathElement}
import rx.core.{Obs, Rx, Var}

import scalatags.JsDom


case class Cds(direction: Rx[Direction],
               alignment: Rx[BackboneAlignment],
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
      case (Rightwards, AboveBackbone | CentredOnBackbone) | (Leftwards, AboveBackbone) =>
        -metrics().depth
      case (Rightwards, BelowBackbone) | (Leftwards, CentredOnBackbone | BelowBackbone) =>
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
      case Rightwards => s"M${ m.l2} 0 L${ m.l2h} ${-m.d2} H${-m.l2} v${ m.depth} H${ m.l2h} L${ m.l2} 0 Z"
      case Leftwards => s"M${-m.l2} 0 L${-m.l2h} ${ m.d2} H${ m.l2} v${-m.depth} H${-m.l2h} L${-m.l2} 0 Z"
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

  private val path_d_obs = Obs(path_d) {
    path("d" -> path_d())
  }

  private val glyph_transform_obs = Obs(glyph_transform) {
    glyph("transform" -> glyph_transform())
  }

}

object Cds {
  def fixedWidth(direction: Direction, label: Option[String] = None): (Rx[Double], Rx[BackboneAlignment]) => GlyphFamily = (width, alignment) =>
    Cds(Var(direction), alignment, Var(label), Var(None), Var(0), Rx {
      val w = width() * 0.9
      new Metrics {
        def length = w
        def depth = w * 0.5
        override def head = d2
      }
    })

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

  trait SCProvider extends ShortcodeProvider {
    import scalatags.JsDom.all.{bindNode}
    import scalatags.JsDom.{all => html}
    import scalatags.JsDom.implicits._
    import scalatags.JsDom.svgTags._
    import scalatags.JsDom.svgAttrs._

    private val cdsHandler: PartialFunction[Shortcode, Node] = {
      case Shortcode("cds", attrs, content) =>
        val attrsM = attrs.toMap
        val wdth = attrsM.get("width").map(_.toDouble).getOrElse(50.0)
        val dir = asDirection(attrsM.get("dir"))
        val cds = fixedWidth(dir, content).apply(Var(wdth), Var(AboveBackbone))

        svg(width := wdth, height := wdth * 0.5, `class` := "sbolv_inline")(
          g(transform := s"translate(${wdth * 0.5} ${wdth * 0.47})")(cds.glyph)
        ).render
    }

    abstract override def shortcodeHandlers(sc: Shortcode) = super.shortcodeHandlers(sc) orElse cdsHandler.lift(sc)
  }
}
