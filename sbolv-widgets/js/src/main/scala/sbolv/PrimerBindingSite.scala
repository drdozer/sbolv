package sbolv

import rx._

case class PrimerBindingSite(horizontalOrientation: Rx[HorizontalOrientation],
               verticalOrientation: Rx[VerticalOrientation],
               width: Rx[Double],
               height: Rx[Double],
               metrics: Rx[PrimerBindingSite.Metrics])
  extends GlyphFamily
{
  override type Metrics = PrimerBindingSite.Metrics
  override type Geometry = PrimerBindingSite.Geometry


  override protected def metricsToGeometry(m: Metrics) = {
    val w = width()
    val w2 = w * 0.5
    val h = height()

    val length = m.length * w
    val l2 = length * 0.5
    val depth = m.depth * h
    val d2 = depth * 0.5

    val xSgn = horizontalOrientation().sgn
    val ySgn = verticalOrientation().sgn

    val mid = h * 0.5
    val top = mid - ySgn * d2
    val bot = mid + ySgn * d2

    val start = w2 - xSgn * l2
    val end = w2 + xSgn * l2
    val tick = end + xSgn * m.tick

    PrimerBindingSite.Geometry(top = top, mid = mid, bot = bot, start = start,
                                tick = tick, end = end)
  }


  override protected def geometryToPath(g: Geometry) = {
    import g._
    s"M$start $mid L$end $mid L$tick $top"
  }

  override def geometryToBaseline(g: Geometry) = g.mid

  override val cssClass = "pbs"
}

object PrimerBindingSite {
  object GlyphType extends GlyphFamily.GlyphType {
    def apply(boxWidthHeight: Rx[Double],
                  horizontalOrientation: Rx[HorizontalOrientation],
                  verticalOrientation: Rx[VerticalOrientation]): GlyphFamily =
      PrimerBindingSite(horizontalOrientation, verticalOrientation, boxWidthHeight, boxWidthHeight, Var(
        new Metrics {
          def length = 0.9
          def depth = length * 0.5
          override def tick = -15
        }
      ))

    val fixedWidthId = GlyphFamily.takeFixedWidthId()
  }

  trait Metrics {
    def length: Double
    def depth: Double
    def tick: Double
  }

  object Metrics {
    def apply(length: Double, depth: Double, tick: Double): Metrics = MetricsImpl(length, depth, tick)
  }

  case class MetricsImpl(length: Double, depth: Double, override val tick: Double) extends Metrics

  case class Geometry(top: Double, mid: Double, bot: Double, start: Double, tick: Double, end: Double)

  trait SCProvider extends GlyphProvider {
    private val pbsHandler: PartialFunction[Shortcode, GlyphFamily.GlyphType] = {
      case Shortcode("pbs", _, _) =>
        GlyphType
  }

  abstract override def glyphHandler(sc: Shortcode) = super.glyphHandler(sc) orElse pbsHandler.lift(sc)
}

trait FWSC extends FixedWidthShortcodeContent {
  abstract override def Code(c: String) = if(c == "p") GlyphType else super.Code(c)
}
}
