package sbolv

import org.scalajs.dom.Node
import rx.core.{Rx, Var}


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
    def apply(direction: HorizontalOrientation, label: Option[String] = None): (Rx[Double], Rx[VerticalOrientation]) => GlyphFamily = (width, verticalOrientation) =>
      RibosomeEntrySite(Var(direction), verticalOrientation, width, width, Rx {
        val w = 0.9
        new BoxyGlyph.Metrics {
          def length = w
          def depth = w * 0.5
        }
      })
  }

  trait SCProvider extends ShortcodeProvider {
    import scalatags.JsDom.all.bindNode
    import scalatags.JsDom.implicits._
    import scalatags.JsDom.svgTags._
    import scalatags.JsDom.svgAttrs._

    private val resHandler: PartialFunction[Shortcode, Node] = {
      case Shortcode("res", attrs, content) =>
        val attrsM = attrs.toMap
        val wdth = attrsM.get("width").map(_.toDouble).getOrElse(50.0)
        val dir = asDirection(attrsM.get("dir"))
        val res = FixedWidth(dir, content).apply(Var(wdth), Var(Upwards))

        svg(width := wdth, height := wdth * 0.5, `class` := "sbolv_inline")(
          g(transform := s"translate(${wdth * 0.5} ${wdth * 0.47})")(res.glyph)
        ).render
    }

    abstract override def shortcodeHandlers(sc: Shortcode) = super.shortcodeHandlers(sc) orElse resHandler.lift(sc)
  }

  trait FWSC extends FixedWidthShorcodeContent {
    abstract override def Code(c: String) = if(c == "r") FixedWidth else super.Code(c)
  }
}
