package sbolv

import rx._

case class Promoter(horizontalOrientation: Rx[HorizontalOrientation],
                    verticalOrientation: Rx[VerticalOrientation],
                    width: Rx[Double],
                    height: Rx[Double],
                    metrics: Rx[Promoter.Metrics])
  extends GlyphFamily
{
  override type Metrics = Promoter.Metrics
  override type Geometry = Promoter.Geometry


  override def metricsToGeometry(m: Metrics) = {
    val w = width()
    val w2 = w * 0.5
    val h = height()
    val h2 = h * 0.5

    val hori = m.horizontal * w
    val hori2 = hori * 0.5


    val vert = m.vertical * h
    val vert2 = vert * 0.5

    val xSgn = horizontalOrientation().sgn
    val ySgn = verticalOrientation().sgn

    val top = h2 - ySgn * vert2
    val bot = h2 + ySgn * vert2

    val left = w2 - xSgn * hori2
    val right = w2 + xSgn * hori2

    val arrowW = right - xSgn * m.arrowWidth * w
    val arrowHDelta = ySgn * m.arrowHeight * h
    
    Promoter.Geometry(top = top, bot = bot, left = left, right = right, arrowW = arrowW, arrowHDelta = arrowHDelta)
  }

  override protected def geometryToPath(g: Geometry) = {
    import g._
    s"M$left $bot L$left $top L$right $top M$arrowW ${top + arrowHDelta} L$right $top L$arrowW ${top - arrowHDelta}"
  }

  /** Calculate the baseline from the geometry. This will be called within an Rx, so can rely upon other Rx variables.
    * */
  override def geometryToBaseline(g: Geometry) = g.bot

  override def cssClass = "promoter"
}

object Promoter {

  object GlyphType extends GlyphFamily.GlyphType {
    def apply(boxWidthHeight: Rx[Double],
                  horizontalOrientation: Rx[HorizontalOrientation],
                  verticalOrientation: Rx[VerticalOrientation]): GlyphFamily =
      Promoter(horizontalOrientation, verticalOrientation, boxWidthHeight, boxWidthHeight, Var(Metrics(0.4, 0.7, 0.1, 0.1)))

    val fixedWidthId = GlyphFamily.takeFixedWidthId()
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

  case class Geometry(top: Double, bot: Double, left: Double, right: Double, arrowW: Double, arrowHDelta: Double)

  trait SCProvider extends GlyphProvider {
    private val promoterHandler: PartialFunction[Shortcode, GlyphFamily.GlyphType] = {
      case Shortcode("promoter", _, _) =>
        GlyphType
    }

    override abstract def glyphHandler(sc: Shortcode) = super.glyphHandler(sc) orElse promoterHandler.lift(sc)
  }

  trait FWSC extends FixedWidthShortcodeContent {
    abstract override def Code(c: String) = if(c == "p") GlyphType else super.Code(c)
  }
}
