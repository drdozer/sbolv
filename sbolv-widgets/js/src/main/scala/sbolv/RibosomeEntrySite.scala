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
  object GlyphType extends GlyphFamily.GlyphType {
    def apply(boxWidthHeight: Rx[Double],
                  horizontalOrientation: Rx[HorizontalOrientation],
                  verticalOrientation: Rx[VerticalOrientation]): GlyphFamily =
      RibosomeEntrySite(horizontalOrientation, verticalOrientation, boxWidthHeight, boxWidthHeight, Rx {
        val w = 0.9
        new BoxyGlyph.Metrics {
          def length = w
          def depth = w * 0.5
        }
      })
        
    val fixedWidthId = GlyphFamily.takeFixedWidthId()
  }

  trait SCProvider extends GlyphProvider {
    private val resHandler: PartialFunction[Shortcode, GlyphFamily.GlyphType] = {
      case Shortcode("res", _, _) =>
        GlyphType
    }

    abstract override def glyphHandler(sc: Shortcode) = super.glyphHandler(sc) orElse resHandler.lift(sc)
  }

  trait FWSC extends FixedWidthShortcodeContent {
    abstract override def Code(c: String) = if(c == "r") GlyphType else super.Code(c)
  }
}
