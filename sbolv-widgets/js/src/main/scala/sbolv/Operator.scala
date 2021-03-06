package sbolv

import rx._


final case class Operator(horizontalOrientation: Rx[HorizontalOrientation],
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
    s"M$start $top L$end $top L$end $bot L$start $bot Z"
  }

  override def cssClass = "operator"
}

object Operator {
  object GlyphType extends GlyphFamily.GlyphType {
    def apply(boxWidthHeight: Rx[Double],
              horizontalOrientation: Rx[HorizontalOrientation],
              verticalOrientation: Rx[VerticalOrientation],
              stroke: Rx[Option[String]],
              fill: Rx[Option[String]],
              cssClasses: Rx[Seq[String]],
              label: Rx[Option[String]]): GlyphFamily =
      Operator(
        horizontalOrientation,
        verticalOrientation,
        stroke,
        fill,
        cssClasses,
        boxWidthHeight,
        boxWidthHeight,
        Var(
          new BoxyGlyph.Metrics {
            def length = 0.5
            def depth = 0.5
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
    abstract override def Code(c: String) = if(c == "o") GlyphType else super.Code(c)
  }
}
