package sbolv

import rx._

final case class Terminator(horizontalOrientation: Rx[HorizontalOrientation],
                            verticalOrientation: Rx[VerticalOrientation],
                            width: Rx[Double],
                            height: Rx[Double],
                            metrics: Rx[BoxyGlyph.Metrics])
  extends BoxyGlyph.BaselineAtBot
{
  override protected def geometryToPath(g: Geometry) = {
    import g._
    s"M$xMid $bot L$xMid $top M$start $top L$end $top"
  }

  override def cssClass = "terminator"
}

object Terminator {
  object FixedWidth extends GlyphFamily.FixedWidth {
    def apply(boxWidthHeight: Rx[Double],
                  horizontalOrientation: Rx[HorizontalOrientation],
                  verticalOrientation: Rx[VerticalOrientation]): GlyphFamily =
      Terminator(horizontalOrientation, verticalOrientation, boxWidthHeight, boxWidthHeight, Rx {
        new BoxyGlyph.Metrics {
          def length = 0.6
          def depth = 0.3
        }
      })

    val fixedWidthId = GlyphFamily.takeFixedWidthId()
  }

  trait SCProvider extends GlyphProvider {
    private val termHandler: PartialFunction[Shortcode, GlyphFamily.FixedWidth] = {
      case Shortcode("term", _, _) =>
        FixedWidth
    }

    abstract override def glyphHandler(sc: Shortcode) = super.glyphHandler(sc) orElse termHandler.lift(sc)
  }

  trait FWSC extends FixedWidthShortcodeContent {
    abstract override def Code(c: String) = if(c == "t") FixedWidth else super.Code(c)
  }
}
