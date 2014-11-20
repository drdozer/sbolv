package sbolv

import org.scalajs.dom.Node
import rx.core.{Rx, Var}


final case class Operator(horizontalOrientation: Rx[HorizontalOrientation],
                          verticalOrientation: Rx[VerticalOrientation],
                          width: Rx[Double],
                          height: Rx[Double],
                          metrics: Rx[BoxyGlyph.Metrics])
  extends BoxyGlyph.BaselineAtBot
{
  override protected def geometryToPath(g: Geometry) = {
    import g._
    s"M$start $top L$end $top L$end $bot L$start $bot Z"
  }

  override def cssClass = "operator"
}

object Operator {
  object FixedWidth extends GlyphFamily.FixedWidth {
    def apply(horizontalOrientation: HorizontalOrientation): (Rx[Double], Rx[VerticalOrientation]) => GlyphFamily = (width, verticalOrientation) =>
      Operator(Var(horizontalOrientation), verticalOrientation, width, width, Rx {
        new BoxyGlyph.Metrics {
          def length = 0.5
          def depth = 0.5
        }
      })
  }

  trait SCProvider extends GlyphProvider {
    private val termHandler: PartialFunction[Shortcode, GlyphFamily.FixedWidth] = {
      case Shortcode("term", _, _) =>
        FixedWidth
    }

    abstract override def glyphHandler(sc: Shortcode) = super.glyphHandler(sc) orElse termHandler.lift(sc)
  }

  trait FWSC extends FixedWidthShortcodeContent {
    abstract override def Code(c: String) = if(c == "o") FixedWidth else super.Code(c)
  }
}
