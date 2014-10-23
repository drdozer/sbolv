package sbolv

import org.scalajs.dom
import org.scalajs.dom.{Node, SVGGElement}
import rx.core.{Rx, Var, Obs}
import rx.ops._
import sbolv.FixedWidth.GlyphHolder
import sbolv.SeqDiff.{Modified, Exited, Entered}

import scala.scalajs.js._
import scala.util.parsing.combinator.RegexParsers

/**
 *
 *
 * @author Matthew Pocock
 */
case class FixedWidth(boxWidthHeight: Rx[Double],
                      alignment: Rx[BackboneAlignment],
                      glyphs: Rx[IndexedSeq[GlyphFactory]])
{
  import Enhancements._
  import scalatags.JsDom.implicits._
  import scalatags.JsDom.svgTags._
  import scalatags.JsDom.svgAttrs._
  import scalatags.JsDom.all.bindNode
  import Framework._
  import Updater._


  private val verticalCentre = Rx {
    boxWidthHeight() * 0.5
  }
  private val allGlyphs_transform = Rx {
    s"translate(${boxWidthHeight() * 0.5} ${verticalCentre()})"
  }

  val glyphUpdater = new Updater[GlyphFactory] {
    override def onEntered(en: Entered[GlyphFactory]) = {
      val vert: Var[VerticalOrientation] = Var(Upwards)
      lazy val gf: GlyphFamily = en.item.apply()(boxWidthHeight, Rx {
        (alignment(), gf.horizontalOrientation()) match {
          case (AboveBackbone, _) | (CentredOnBackbone, Rightwards) => Upwards
          case (BelowBackbone, _) | (CentredOnBackbone, Leftwards)  => Downwards
        }
      })
      val index = Var(en.at.index)
      val gh = FixedWidth.GlyphHolder(en.item, gf, index)


      val holder = g(transform := Rx {
        val x = boxWidthHeight() * index()
        s"translate($x 0)"
      }, gf.glyph).render

      Dynamic(holder).updateDynamic("__sbolv_widget")(Dynamic(gh))

      holder
    }

    override def onModified(mod: Modified[GlyphFactory], existing: Node): Option[Frag] = {
      val holder = Dynamic(existing).selectDynamic("__sbolv_widget").asInstanceOf[GlyphHolder]
      holder.index() = mod.at._2.index
      None
    }
  }

  val allGlyphs = g(
    `class` := "sbolv fixed-width glyphs",
    transform := allGlyphs_transform,
    glyphs updateWith glyphUpdater).render

  Dynamic(allGlyphs).updateDynamic("__sbolv_widget")(Dynamic(this))
}

object FixedWidth {
  case class GlyphHolder(gf: GlyphFactory, g: GlyphFamily, index: Var[Int])

  trait SCProvider extends ShortcodeProvider {
    import scalatags.JsDom.all.{bindNode}
    import scalatags.JsDom.{all => html}
    import scalatags.JsDom.implicits._
    import scalatags.JsDom.svgTags._
    import scalatags.JsDom.svgAttrs._

    object FWSC extends FixedWidthShorcodeContent
        with Promoter.FWSC
        with RibosomeEntrySite.FWSC
        with Cds.FWSC
        with Terminator.FWSC

    private val sbolvHandler: PartialFunction[Shortcode, Node] = {
      case Shortcode("sbolv", attrs, content) =>
        val attrsM = attrs.toMap
        val wdth = attrsM.get("width").map(_.toDouble).getOrElse(50.0)

        val glyphs = for {
          c <- content.to[IndexedSeq]
          g <- c.split("""\s+""")
        } yield {
          FWSC.parseAll(FWSC.entry, g).get
        }
        val glyphsV = Var(IndexedSeq.empty[GlyphFactory])
        val fixedWidth = FixedWidth(Var(wdth), Var(AboveBackbone), glyphsV)
        glyphsV() = glyphs

        svg(width := wdth * glyphs.length, height := wdth * 0.5, `class` := "sbolv_inline")(
          g(transform := s"translate(0 -1)")(fixedWidth.allGlyphs)
        ).render
    }

    abstract override def shortcodeHandlers(sc: Shortcode) = super.shortcodeHandlers(sc) orElse sbolvHandler.lift(sc)
  }
}

case class GlyphFactory(glyphFamily: GlyphFamily.FixedWidth, direction: HorizontalOrientation, label: Option[String] = None) {
  def apply() = glyphFamily(direction, label)
}

object GlyphFactory {
  implicit val ordering: Ordering[GlyphFactory] = Ordering by (_.glyphFamily)
}

abstract class FixedWidthShorcodeContent extends RegexParsers {
  private val code = """[a-zA-Z]""".r ^^ { case c => Code(c) }

  private val lt: Parser[HorizontalOrientation] = "<" ^^^ Leftwards
  private val gt: Parser[HorizontalOrientation] = ">" ^^^ Rightwards
  private val dir = lt | gt

  private val qt: Parser[String] = "\""
  private val notQt: Parser[String] = "[^\"]*".r
  private val qtStr = qt ~> notQt <~ qt


  val entry = code ~ dir ~ qtStr.? ^^ { case c ~ d ~ l => GlyphFactory(c, d, l) }

  def Code(c: String): GlyphFamily.FixedWidth =
    throw new IllegalArgumentException(s"Unknown code: $c")
}