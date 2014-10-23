package sbolv

import org.scalajs.dom.Node
import rx.core.{Rx, Var}


final case class Terminator(horizontalOrientation: Rx[HorizontalOrientation],
                            verticalOrientation: Rx[VerticalOrientation],
                            width: Rx[Double],
                            height: Rx[Double],
                            metrics: Rx[BoxyGlyph.Metrics])
  extends BoxyGlyph.BaselineAtBot
{
  override protected def geometryToPath(g: Geometry) = {
    import g._
    s"M$bot $xMid L$top $xMid M$top $start $top $end"
  }

  override def cssClass = "terminator"
}

object Terminator {
  object FixedWidth extends GlyphFamily.FixedWidth {
    def apply(horizontalOrientation: HorizontalOrientation, label: Option[String] = None): (Rx[Double], Rx[VerticalOrientation]) => GlyphFamily = (width, verticalOrientation) =>
      Terminator(Var(horizontalOrientation), verticalOrientation, width, width, Rx {
        val w = width()
        new BoxyGlyph.Metrics {
          def length = w * 0.9
          def depth = w * 0.6
        }
      })
  }

  trait SCProvider extends ShortcodeProvider {
    import scalatags.JsDom.all.bindNode
    import scalatags.JsDom.implicits._
    import scalatags.JsDom.svgTags._
    import scalatags.JsDom.svgAttrs._

    private val termHandler: PartialFunction[Shortcode, Node] = {
      case Shortcode("term", attrs, content) =>
        val attrsM = attrs.toMap
        val wdth = attrsM.get("width").map(_.toDouble).getOrElse(50.0)
        val dir = asDirection(attrsM.get("dir"))
        val term = FixedWidth(dir, content).apply(Var(wdth), Var(Upwards))

        svg(width := wdth, height := wdth * 0.5, `class` := "sbolv_inline")(
          g(transform := s"translate(${wdth * 0.5} ${wdth * 0.47})")(term.glyph)
        ).render
    }

    abstract override def shortcodeHandlers(sc: Shortcode) = super.shortcodeHandlers(sc) orElse termHandler.lift(sc)
  }

  trait FWSC extends FixedWidthShorcodeContent {
    abstract override def Code(c: String) = if(c == "t") FixedWidth else super.Code(c)
  }
}
