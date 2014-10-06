package talk

import org.scalajs.dom.{Node, SVGGElement}
import rx.core.{Var, Obs, Rx}
import rx.ops._

import scala.scalajs.js

/**
 *
 *
 * @author Matthew Pocock
 */
case class FixedWidth(boxWidthHeight: Rx[Double],
                      alignment: Rx[BackboneAlignment],
                      glyphs: Rx[Seq[(Rx[Double], Rx[BackboneAlignment]) => GlyphFamily]])
{
  import Enhancements._

  private val verticalCentre = Rx {
    boxWidthHeight() * 0.5
  }
  private val allGlyphs_transform = Rx {
    s"translate(0 ${verticalCentre()})"
  }

  private val dd = DataDiff(glyphs)

  private val dd_obs = Obs(dd.updates) {
    val updates = dd.updates()
    for((g, i) <- updates.entered) {
      val gl = g(boxWidthHeight, alignment)
      val tr = "g".asSVGElement[SVGGElement](gl.glyph)
      Obs(boxWidthHeight) {
        tr("transform" -> s"translate(${(i + 0.5) * boxWidthHeight()} 0)")
      }

      allGlyphs.insertBefore(tr, Option(allGlyphs.childNodes.apply(i)).filterNot(_ == js.undefined).map(_.nextSibling).getOrElse(null))
    }
  }


  val allGlyphs = "g".asSVGElement[SVGGElement]("class" -> "sbolv fixed-width glyphs")

  private val allGlyphs_tranform_obs = Obs(allGlyphs_transform) {
    allGlyphs("transform" -> allGlyphs_transform())
  }

}

object FixedWidth {
  trait SCProvider extends ShortcodeProvider {
    import scalatags.JsDom.all.{bindNode}
    import scalatags.JsDom.{all => html}
    import scalatags.JsDom.implicits._
    import scalatags.JsDom.svgTags._
    import scalatags.JsDom.svgAttrs._

    private val sbolvHandler: PartialFunction[Shortcode, Node] = {
      case Shortcode("sbolv", attrs, content) =>
        val attrsM = attrs.toMap
        val wdth = attrsM.get("width").map(_.toDouble).getOrElse(50.0)

        println(s"Generating fixed-width display for '$content'")

        val glyphs = for {
          c <- content.to[Seq]
          g <- c.split("""\s+""")
        } yield {
          g match {
            case "p>" => Promoter.fixedWidth(Rightwards)
            case "p<" => Promoter.fixedWidth(Leftwards)
            case "c>" => Cds.fixedWidth(Rightwards)
            case "c<" => Cds.fixedWidth(Leftwards)
            case "r>" => RibosomeEntrySite.fixedWidth(Rightwards)
            case "r<" => RibosomeEntrySite.fixedWidth(Leftwards)
          }
        }
        println(s"Adding ${glyphs.length} glyphs")
        val glyphsV = Var(Seq.empty[(Rx[Double], Rx[BackboneAlignment]) => GlyphFamily])
        val fixedWidth = FixedWidth(Var(wdth), Var(AboveBackbone), glyphsV)
        glyphsV() = glyphs

        svg(width := wdth * glyphs.length, height := wdth * 0.5, `class` := "sbolv_inline")(
          g(transform := s"translate(0 -1)")(fixedWidth.allGlyphs)
        ).render
    }

    abstract override def shortcodeHandlers(sc: Shortcode) = super.shortcodeHandlers(sc) orElse sbolvHandler.lift(sc)

  }
}
