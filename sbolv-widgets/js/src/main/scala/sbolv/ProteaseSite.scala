package sbolv

import rx._

final case class ProteaseSite(horizontalOrientation: Rx[HorizontalOrientation],
                            verticalOrientation: Rx[VerticalOrientation],
                            stroke: Rx[Option[String]],
                            fill: Rx[Option[String]],
                            cssClasses: Rx[Seq[String]],
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
  object GlyphType extends GlyphFamily.GlyphType {
    def apply(boxWidthHeight: Rx[Double],
              horizontalOrientation: Rx[HorizontalOrientation],
              verticalOrientation: Rx[VerticalOrientation],
              stroke: Rx[Option[String]],
              fill: Rx[Option[String]],
              cssClasses: Rx[Seq[String]],
              label: Rx[Option[String]]): GlyphFamily =
      ProteaseSite(
        horizontalOrientation,
        verticalOrientation,
        stroke,
        fill,
        cssClasses,
        boxWidthHeight,
        boxWidthHeight,
        Rx {
          new StemmyGlyph.Metrics {
            def length = 0.6
            def depth = 0.6
            def stemHeight = 0.4
          }
        }
      )

    val fixedWidthId = GlyphFamily.takeFixedWidthId()
  }

  trait SCProvider extends GlyphProvider {
    private val ptsHandler: PartialFunction[Shortcode, GlyphFamily.GlyphType] = {
      case Shortcode("pts", _, _) =>
        GlyphType
    }

    abstract override def glyphHandler(sc: Shortcode) = super.glyphHandler(sc) orElse ptsHandler.lift(sc)
  }

  trait FWSC extends FixedWidthShortcodeContent {
    abstract override def Code(c: String) = if(c == "p") GlyphType else super.Code(c)
  }
}
