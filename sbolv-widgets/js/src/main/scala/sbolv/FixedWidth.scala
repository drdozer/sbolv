package sbolv

import org.scalajs.dom._
import org.scalajs.dom.extensions._
import rx._
import sbolv.GlyphFamily.{GlyphType, GlyphSpec}

import scala.scalajs.js._
import scala.util.parsing.combinator.RegexParsers
import scalatags.ext._
import scalatags.JsDom
import JsDom.all.{width => _, height => _, opacity => _, `type` => _, _}
import JsDom.svgTags._
import JsDom.svgAttrs.{`class` => _, _}
import scalatags.ext._
import scalatags.ext.Framework._
import scalatags.ext.Updater._

/**
 *
 *
 * @author Matthew Pocock
 */
case class FixedWidth(boxWidthHeight: Rx[Double],
                      alignment: Rx[BackboneAlignment],
                      glyphs: Rx[IndexedSeq[GlyphSpec]])
{
  val glyphUpdater = new Updater[GlyphSpec] {
    override def onEntered(en: SeqDiff.Entered[GlyphSpec]): scalatags.JsDom.all.Frag = {
      val hor: Var[HorizontalOrientation] = Var(en.item.horizontalOrientation)
      val vert: Var[VerticalOrientation] = Var(Upwards)
      val stroke = Var(en.item.stroke)
      val fill = Var(en.item.fill)
      val cssClasses = Var(en.item.cssClasses)
      val label = Var(en.item.label)

      val gf: GlyphFamily = en.item.glyphType(boxWidthHeight, hor, vert, stroke, fill, cssClasses, label)
      val vert2 = Rx {
              (alignment(), gf.horizontalOrientation()) match {
                case (AboveBackbone, _) | (CentredOnBackbone, Rightwards) => Upwards
                case (BelowBackbone, _) | (CentredOnBackbone, Leftwards)  => Downwards
              }
            }
      Obs(vert2) { vert() = vert2() } // initialization order hack
      val lastIndex = Var(en.at.index)
      val index = Var(en.at.index)

      val labelled = LabelledGlyph.from(gf, label)
      val gh = FixedWidth.GlyphHolder(en.item, labelled, hor, vert, stroke, fill, cssClasses, label, lastIndex, index)

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

      holder
    }

    override def onModified(mod: SeqDiff.Modified[GlyphSpec], existing: Node): Option[Frag] = {
      val holder = Dynamic(existing).selectDynamic("__sbolv_widget").asInstanceOf[FixedWidth.GlyphHolder]
      holder.lastIndex() = mod.at._1.index
      holder.index() = mod.at._2.index
      holder.label() = mod.item._2.label
      holder.horizontalOrientation() = mod.item._2.horizontalOrientation
      holder.stroke() = mod.item._2.stroke
      holder.fill() = mod.item._2.fill
      holder.cssClasses() = mod.item._2.cssClasses
      for(n <- holder.lab.svgElement.parentNode.asInstanceOf[Element].getElementsByClassName("glyphMoveAnimation")) {
        Dynamic(n).beginElement()
      }
      None
    }

    override def onExited(ex: SeqDiff.Exited[GlyphSpec], existing: Node): Option[Frag] = {
      val holder = Dynamic(existing).selectDynamic("__sbolv_widget").asInstanceOf[FixedWidth.GlyphHolder]

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

  private implicit val gfScoreF = new ScoreFunction[GlyphSpec] {
    override def indelCost(t: GlyphSpec) = - 7

    override def matchCost(t1: GlyphSpec, t2: GlyphSpec) =
      if(t1.glyphType != t2.glyphType) -15
      else {
        val h = if(t1.horizontalOrientation == t2.horizontalOrientation) 0 else -1
        val v = if(t1.verticalOrientation == t2.verticalOrientation) 0 else -1
        val s = if(t1.stroke == t2.stroke) 0 else -1
        val f = if(t1.fill == t2.fill) 0 else -1
        val c = if(t1.cssClasses == t2.cssClasses) 0 else -1
        val l = if(t1.label == t2.label) 0 else -1
        h + v + s + f + c + l
      }
  }

  private implicit val gfamOrd: Ordering[GlyphFamily.GlyphType] = Ordering.by(
      (_: GlyphFamily.GlyphType).fixedWidthId)
  private implicit val gfOrd: Ordering[GlyphSpec] = Ordering.by(
    (gf: GlyphSpec) => (
      gf.glyphType, gf.horizontalOrientation, gf.verticalOrientation, gf.fill, gf.stroke, gf.cssClasses.mkString(" "), gf.label))


  val allGlyphs = g(
    `class` := "sbolv fixed-width glyphs",
    glyphs updateWith glyphUpdater).render

  Dynamic(allGlyphs).updateDynamic("__sbolv_widget")(Dynamic(this))

  val glyphUpdates = SeqDiff(glyphs).updates

}

object FixedWidth {
  case class GlyphHolder(gf: GlyphSpec,
                         lab: LabelledGlyph,
                         horizontalOrientation: Var[HorizontalOrientation],
                         verticalOrientation: Var[VerticalOrientation],
                         stroke: Var[Option[String]],
                         fill: Var[Option[String]],
                         cssClasses: Var[Seq[String]],
                         label: Var[Option[String]],
                         lastIndex: Var[Int],
                         index: Var[Int])

  trait SCProvider extends ShortcodeProvider {

    object FWSC extends FixedWidthShortcodeContent
        with Promoter.FWSC
        with RibosomeEntrySite.FWSC
        with Cds.FWSC
        with Terminator.FWSC
        with ProteaseSite.FWSC
        with PrimerBindingSite.FWSC

    private val sbolvHandler: PartialFunction[Shortcode, Node] = {
      case Shortcode("sbolv", attrs, content) =>
        val attrsM = attrs.toMap
        val wdth = attrsM.get("width").map(_.toDouble).getOrElse(50.0)

        val glyphs = for {
          c <- content.to[IndexedSeq]
          g <- c.split("""\s+""").to[IndexedSeq]
        } yield {
          FWSC.parseAll(FWSC.entry, g).get
        }
        val glyphsV = Var(IndexedSeq.empty[GlyphSpec])
        val fixedWidth = FixedWidth(Var(wdth), Var(AboveBackbone), glyphsV)
        glyphsV() = glyphs

        svg(width := wdth * glyphs.length, height := wdth, `class` := "sbolv_inline")(
          fixedWidth.allGlyphs
        ).render
    }

    abstract override def shortcodeHandlers(sc: Shortcode) = super.shortcodeHandlers(sc) orElse sbolvHandler.lift(sc)
  }
}

abstract class FixedWidthShortcodeContent extends RegexParsers {
  private val code = """[a-zA-Z]""".r ^^ { case c => Code(c) }

  private val lt: Parser[HorizontalOrientation] = "<" ^^^ Leftwards
  private val gt: Parser[HorizontalOrientation] = ">" ^^^ Rightwards
  private val dir = lt | gt

  private val qt: Parser[String] = "\""
  private val notQt: Parser[String] = "[^\"]*".r
  private val qtStr = qt ~> notQt <~ qt


  val entry = code ~ dir ~ qtStr.? ^^ { case c ~ d ~ l => GlyphSpec(
    glyphType = c,
    horizontalOrientation = d,
    label = l)
  }

  def Code(c: String): GlyphFamily.GlyphType =
    throw new IllegalArgumentException(s"Unknown code: $c")
}
