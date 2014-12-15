package sbolv

import rx._

case class RibosomeEntrySite(horizontalOrientation: Rx[HorizontalOrientation],
                             verticalOrientation: Rx[VerticalOrientation],
                             width: Rx[Double],
                             height: Rx[Double],
                             metrics: Rx[BoxyGlyph.Metrics])
  extends BoxyGlyph.BaselineAtBot
{
  override protected def geometryToPath(g: Geometry) = {
    import g._
    s"M$start $bot L$end $bot C$end $top $start $top $start $bot Z"
  }

  override def cssClass = "res"
}

object RibosomeEntrySite {
  object FixedWidth extends GlyphFamily.FixedWidth {
    def apply(direction: HorizontalOrientation): (Rx[Double], Rx[VerticalOrientation]) => GlyphFamily = (width, verticalOrientation) =>
      RibosomeEntrySite(Var(direction), verticalOrientation, width, width, Rx {
        val w = 0.9
        new BoxyGlyph.Metrics {
          def length = w
          def depth = w * 0.5
        }
      })
  }

  trait SCProvider extends GlyphProvider {
    private val resHandler: PartialFunction[Shortcode, GlyphFamily.FixedWidth] = {
      case Shortcode("res", _, _) =>
        FixedWidth
    }

    abstract override def glyphHandler(sc: Shortcode) = super.glyphHandler(sc) orElse resHandler.lift(sc)
  }

  trait FWSC extends FixedWidthShortcodeContent {
    abstract override def Code(c: String) = if(c == "r") FixedWidth else super.Code(c)
  }
}
