package sbolv

import org.scalajs.dom.Node
import rx.core.{Rx, Var}


final case class ProteaseSite(horizontalOrientation: Rx[HorizontalOrientation],
                            verticalOrientation: Rx[VerticalOrientation],
                            width: Rx[Double],
                            height: Rx[Double],
                            metrics: Rx[StemmyGlyph.Metrics])
  extends StemmyGlyph.BaselineAtBot
{
  override protected def geometryToPath(g: Geometry) = {
    import g._
    s"M$xMid $bot L$xMid $stemTop M$start $top L$end $headBot M$end $top L$start $headBot"
  }

  override def cssClass = "ProteaseSite"
}

object ProteaseSite {
  object FixedWidth extends GlyphFamily.FixedWidth {
    def apply(horizontalOrientation: HorizontalOrientation): (Rx[Double], Rx[VerticalOrientation]) => GlyphFamily = (width, verticalOrientation) =>
      ProteaseSite(Var(horizontalOrientation), verticalOrientation, width, width, Rx {
        new StemmyGlyph.Metrics {
          def length = 0.6
          def depth = 0.6
          def stemHeight = 0.4
        }
      })
  }

  trait SCProvider extends GlyphProvider {
    private val ptsHandler: PartialFunction[Shortcode, GlyphFamily.FixedWidth] = {
      case Shortcode("pts", _, _) =>
        FixedWidth
    }

    abstract override def glyphHandler(sc: Shortcode) = super.glyphHandler(sc) orElse ptsHandler.lift(sc)
  }

  trait FWSC extends FixedWidthShortcodeContent {
    abstract override def Code(c: String) = if(c == "p") FixedWidth else super.Code(c)
  }
}
