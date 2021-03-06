package sbolv

import rx._

final case class Terminator(horizontalOrientation: Rx[HorizontalOrientation],
                            verticalOrientation: Rx[VerticalOrientation],
                            stroke: Rx[Option[String]],
                            fill: Rx[Option[String]],
                            cssClasses: Rx[Seq[String]],
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
  object GlyphType extends GlyphFamily.GlyphType {
    def apply(boxWidthHeight: Rx[Double],
              horizontalOrientation: Rx[HorizontalOrientation],
              verticalOrientation: Rx[VerticalOrientation],
              stroke: Rx[Option[String]],
              fill: Rx[Option[String]],
              cssClasses: Rx[Seq[String]],
              label: Rx[Option[String]]): GlyphFamily =
      Terminator(
        horizontalOrientation,
        verticalOrientation,
        stroke,
        fill,
        cssClasses,
        boxWidthHeight,
        boxWidthHeight,
        Var(
          new BoxyGlyph.Metrics {
            def length = 0.6
            def depth = 0.3
          }
        )
      )

    val fixedWidthId = GlyphFamily.takeFixedWidthId()
  }

  trait SCProvider extends GlyphProvider {
    private val termHandler: PartialFunction[Shortcode, GlyphFamily.GlyphType] = {
      case Shortcode("term", _, _) =>
        GlyphType
    }

    abstract override def glyphHandler(sc: Shortcode) = super.glyphHandler(sc) orElse termHandler.lift(sc)
  }

  trait FWSC extends FixedWidthShortcodeContent {
    abstract override def Code(c: String) = if(c == "t") GlyphType else super.Code(c)
  }
}
