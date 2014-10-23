package sbolv

import org.scalajs.dom.Node
import rx.core.{Rx, Var}


case class Cds(horizontalOrientation: Rx[HorizontalOrientation],
               verticalOrientation: Rx[VerticalOrientation],
               width: Rx[Double],
               height: Rx[Double],
               metrics: Rx[Cds.Metrics])
  extends GlyphFamily
{
  override type Metrics = Cds.Metrics
  override type Geometry = Cds.Geometry

  override protected def metricsToGeometry(m: Metrics) = {
    val w = width()
    val w2 = w * 0.5
    val h = height()

    val length = m.length * w
    val l2 = length * 0.5
    val depth = m.depth * h
    val d2 = depth * 0.5
    val head = w * m.head

    val xSgn = verticalOrientation().sgn
    val ySgn = horizontalOrientation().sgn

    val mid = h * 0.5
    val top = mid - ySgn * d2
    val bot = mid + ySgn * d2

    val start = w2 - xSgn * l2
    val end = w2 + xSgn * l2
    val arrow = end - xSgn * head

    Cds.Geometry(top = top, mid = mid, bot = bot, start = start, arrow = arrow, end = end)
  }


  override protected def geometryToPath(g: Geometry) = {
    import g._
    s"M$end $mid L$top $arrow L$top $start L$bot $start L$bot $arrow L$end $mid Z"
  }

  override def geometryToBaseline(g: Geometry) = g.mid

  override val cssClass = "cds"
}

object Cds {
  object FixedWidth extends GlyphFamily.FixedWidth {
    def apply(horizontalDirection: HorizontalOrientation, label: Option[String] = None):
    (Rx[Double], Rx[VerticalOrientation]) => GlyphFamily = (width, verticalOrientation) =>
      Cds(Var(horizontalDirection), verticalOrientation, width, width, Var(
        new Metrics {
          def length = 0.9
          def depth = length * 0.5
          override def head = depth * 0.5
        }
      ))
  }

  trait Metrics {
    def length: Double
    def depth: Double

    def head: Double = length - body
    def body: Double = length - head
  }

  object Metrics {
    def apply(length: Double, depth: Double, head: Double): Metrics = MetricsImpl(length, depth, head)
  }

  case class MetricsImpl(length: Double, depth: Double, override val head: Double) extends Metrics

  case class Geometry(top: Double, mid: Double, bot: Double, start: Double, arrow: Double, end: Double)

  trait SCProvider extends ShortcodeProvider {
    import scalatags.JsDom.all.bindNode
    import scalatags.JsDom.implicits._
    import scalatags.JsDom.svgTags._
    import scalatags.JsDom.svgAttrs._

    private val cdsHandler: PartialFunction[Shortcode, Node] = {
      case Shortcode("cds", attrs, content) =>
        val attrsM = attrs.toMap
        val wdth = attrsM.get("width").map(_.toDouble).getOrElse(50.0)
        val dir = asDirection(attrsM.get("dir"))
        val cds = FixedWidth(dir, content).apply(Var(wdth), Var(Upwards))

        svg(width := wdth, height := wdth, `class` := "sbolv_inline")(cds.glyph).render
  }

  abstract override def shortcodeHandlers(sc: Shortcode) = super.shortcodeHandlers(sc) orElse cdsHandler.lift(sc)
}

trait FWSC extends FixedWidthShorcodeContent {
  abstract override def Code(c: String) = if(c == "c") FixedWidth else super.Code(c)
}
}
