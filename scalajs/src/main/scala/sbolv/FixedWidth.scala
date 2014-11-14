package sbolv

import org.scalajs.dom._
import org.scalajs.dom.extensions._
import rx.core.{Rx, Var, Obs}
import rx.ops._
import sbolv.FixedWidth.GlyphHolder

import scala.scalajs.js._
import scala.util.parsing.combinator.RegexParsers
import scalatags.JsDom.all.bindNode
import scalatags.JsDom.implicits._
import scalatags.JsDom.svgAttrs._
import scalatags.JsDom.svgTags._

import scalatags.ext._
import SeqDiff._
import Framework._
import Updater._

/**
 *
 *
 * @author Matthew Pocock
 */
case class FixedWidth(boxWidthHeight: Rx[Double],
                      alignment: Rx[BackboneAlignment],
                      glyphs: Rx[IndexedSeq[GlyphFactory]])
{
  Obs(glyphs) {
    println("glyphs changed")
  }

  val glyphUpdater = new Updater[GlyphFactory] {
    override def onEntered(en: Entered[GlyphFactory]): scalatags.JsDom.all.Frag = {
      val vert: Var[VerticalOrientation] = Var(Upwards)
      val gf: GlyphFamily = en.item.create()(boxWidthHeight, vert)
      val vert2 = Rx {
              (alignment(), gf.horizontalOrientation()) match {
                case (AboveBackbone, _) | (CentredOnBackbone, Rightwards) => Upwards
                case (BelowBackbone, _) | (CentredOnBackbone, Leftwards)  => Downwards
              }
            }
      Obs(vert2) { vert() = vert2() } // initialization order hack
      val lastIndex = Var(en.at.index)
      val index = Var(en.at.index)

      val labelled = LabelledGlyph.from(gf, en.item.label)
      val gh = FixedWidth.GlyphHolder(en.item, labelled, lastIndex, index)

      val lastTranslate = Rx {
        val x = boxWidthHeight() * lastIndex()
        s"$x,0"
      }
      val currentTranslate = Rx {
        val x = boxWidthHeight() * index()
        s"$x,0"
      }

      val translate = Rx {
        val x = boxWidthHeight() * index()
        s"translate($x 0)"
      }

      val animTranslate = Rx {
        s"${lastTranslate()} ; ${currentTranslate()}"
      }

      val holder = g(
        transform := translate,
        labelled.svgElement,
        animatetransform.copy(tag="animateTransform")(
          `class` := "glyphMoveAnimation",
          attributeName := "transform",
          attributeType := "XML",
          `type` := "translate",
          values := animTranslate,
          dur := "0.3s",
          begin := "indefinite"
        )).render

      Dynamic(holder).updateDynamic("__sbolv_widget")(Dynamic(gh))

      println("returning glyph")
      holder
    }

    override def onModified(mod: Modified[GlyphFactory], existing: Node): Option[Frag] = {
      val holder = Dynamic(existing).selectDynamic("__sbolv_widget").asInstanceOf[GlyphHolder]
      holder.lastIndex() = mod.at._1.index
      holder.index() = mod.at._2.index
      for(n <- holder.lab.svgElement.parentNode.asInstanceOf[Element].getElementsByClassName("glyphMoveAnimation")) {
        Dynamic(n).beginElement()
      }
      None
    }

    override def onExited(ex: Exited[GlyphFactory], existing: Node): Option[Frag] = {
      val holder = Dynamic(existing).selectDynamic("__sbolv_widget").asInstanceOf[GlyphHolder]

      Some(
        existing.asInstanceOf[Element].modifyWith(
          opacity := 1 to 0 dur 0.2.s withFill "freeze" modifyWith (
            Events.endEvent := {
              (e: Event) => {
                existing.parentNode.removeChild(existing)
                ()
              }
            }
          )
        )
      )
    }
  }

  val allGlyphs = g(
    `class` := "sbolv fixed-width glyphs",
    glyphs updateWith glyphUpdater).render

  Dynamic(allGlyphs).updateDynamic("__sbolv_widget")(Dynamic(this))
}

object FixedWidth {
  case class GlyphHolder(gf: GlyphFactory, lab: LabelledGlyph, lastIndex: Var[Int], index: Var[Int])

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

        svg(width := wdth * glyphs.length, height := wdth, `class` := "sbolv_inline")(
          fixedWidth.allGlyphs
        ).render
    }

    abstract override def shortcodeHandlers(sc: Shortcode) = super.shortcodeHandlers(sc) orElse sbolvHandler.lift(sc)
  }
}

case class GlyphFactory(glyphFamily: GlyphFamily.FixedWidth, direction: HorizontalOrientation, label: Option[String]) {
  def create() = {
    println(s"Creating glyph family $glyphFamily, with direction $direction")
    glyphFamily(direction)
  }
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