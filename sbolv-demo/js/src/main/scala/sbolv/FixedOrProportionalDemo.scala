package sbolv

import sbolv.GlyphFamily.GlyphSpec
import sbolv.geom._

import org.scalajs.dom._
import org.scalajs.dom.extensions._

import rx._
import rx.ops._

import scala.scalajs.js.annotation.JSExport
import scalatags.JsDom
import scalatags.JsDom.all._
import scalatags.ext._
import Framework._


/**
 *
 *
 * @author Matthew Pocock
 */
@JSExport
object FixedOrProportionalDemo {

  @JSExport
  def wireFixedWidthExample(divId: String): Unit = {
    val exampleDiv = document.getElementById(divId).asInstanceOf[HTMLDivElement]
    val lengthSlider = ReactiveSlider(exampleDiv.getElementsByClassName("length_slider").head.asInstanceOf[HTMLInputElement])
    val lengthSpan = exampleDiv.getElementsByClassName("length")
    val buttons = exampleDiv.getElementsByTagName("button").map(_.asInstanceOf[HTMLButtonElement])
    val fixedWidthSvg = exampleDiv.getElementsByClassName("glyphs").map(_.asInstanceOf[SVGSVGElement]).head
    val track = fixedWidthSvg.getElementsByClassName("track").map(_.asInstanceOf[SVGGElement]).head

    Obs(lengthSlider.valueAsNumber) {
      lengthSpan.foreach(_.textContent = lengthSlider.valueAsNumber().toString)
    }

    val glyphs = Var(IndexedSeq.empty[GlyphSpec])
    val fw = FixedWidth(lengthSlider.valueAsNumber map (_.toDouble), Var(CentredOnBackbone : BackboneAlignment), glyphs)
    track.modifyWith(fw.allGlyphs).render

    val selectedGlyphHolder: Var[Option[FixedWidth.GlyphHolder]] = Var(None)

    val selectedGlyphVars = Rx {
      val io = for(
        gh <- selectedGlyphHolder()
      ) yield {
        val text = gh.lab.label.content.asInstanceOf[Var[Option[String]]]

        (Some(text), Some(gh.lab.gf.horizontalOrientation.asInstanceOf[Var[HorizontalOrientation]]))
      }

      io.getOrElse(None, None)
    }

    val selectionWidget = Rx {
      def editor(rxO: Option[Var[Option[String]]], descr: String) = rxO map { rx =>
        def changeHandler(e: Event) =
          rx() = e.srcElement.asInstanceOf[HTMLInputElement].value.trim() match {
            case "" => None
            case l => Some(l)
          }

        span(`class` := "label_editor")(
          span(`class` := "label_description")(descr),
          input(
            `class` := "label_textField",
            `type` := "text",
            value := (rx map (_ getOrElse "")),
            Events.keypress := changeHandler _,
            Events.input := changeHandler _)
        )
      }

      def flipper(dO: Option[Var[HorizontalOrientation]]) = dO map { d =>
        def flipHandler(e: Event) = d() = d().reverse

        span(`class` := "direction_editor")(
          button("flip", Events.click := flipHandler _)
        )
      }


      def deleter = {
        def deleteHandler(e: Event): Unit = {
          for(gh <- selectedGlyphHolder()) {
            selectedGlyphHolder() = None
            val indx = gh.index()
            val newGlyphs = glyphs().zipWithIndex.filter(_._2 != indx).unzip._1
            glyphs() = newGlyphs
          }
        }
        span(`class` := "delete_glyph"){
          button("x", Events.click := deleteHandler _)
        }
      }

      val (text, dO) = selectedGlyphVars()
      div(
        `class` := "glyph_editor")(
          deleter,
          flipper(dO),
          editor(text, "label")
      )
    }

    val labelEditorPosition = Var(Point2(0,0))
    val labelEditor = div(
      position.absolute,
      display := selectedGlyphHolder map { g => if(g.isDefined) "block" else "none"},
      JsDom.all.left := labelEditorPosition map (_.x px),
      JsDom.all.top := labelEditorPosition map (_.y px),
      selectionWidget).render
    exampleDiv.modifyWith(labelEditor).render

    case class ClickAdder(gffw: GlyphFamily.GlyphType) extends GlyphFamily.GlyphType {
      def apply(boxWidthHeight: Rx[Double],
                horizontalOrientation: Rx[HorizontalOrientation],
                verticalOrientation: Rx[VerticalOrientation]): GlyphFamily = {

        import scala.scalajs.js.Dynamic

        val g = gffw(boxWidthHeight, horizontalOrientation, verticalOrientation)
        g.glyph.addEventListener("click", (e: Event) => {
          e.stopPropagation()
          selectedGlyphHolder() = Some(Dynamic(g.glyph.parentNode.parentNode).__sbolv_widget.asInstanceOf[FixedWidth.GlyphHolder])
          labelEditorPosition() = Point2(Dynamic(e).pageX.asInstanceOf[Double], Dynamic(e).pageY.asInstanceOf[Double])
        }, true)
        g
      }

      override def fixedWidthId = gffw.fixedWidthId
    }

    fixedWidthSvg.modifyWith(
      Events.click := {(me: Event) => selectedGlyphHolder() = None }
    ).render

    for(b <- buttons) {
      b.onclick = (e: Event) => {
        b.value match {
          case "promoter_rightwards" =>
            glyphs() = glyphs() :+ GlyphSpec(ClickAdder(Promoter.GlyphType), Rightwards)
          case "promoter_leftwards" =>
            glyphs() = glyphs() :+ GlyphSpec(ClickAdder(Promoter.GlyphType), Leftwards)
          case "cds_rightwards" =>
            glyphs() = glyphs() :+ GlyphSpec(ClickAdder(Cds.GlyphType), Rightwards)
          case "cds_leftwards" =>
            glyphs() = glyphs() :+ GlyphSpec(ClickAdder(Cds.GlyphType), Leftwards)
          case "res_rightwards" =>
            glyphs() = glyphs() :+ GlyphSpec(ClickAdder(RibosomeEntrySite.GlyphType), Rightwards)
          case "res_leftwards" =>
            glyphs() = glyphs() :+ GlyphSpec(ClickAdder(RibosomeEntrySite.GlyphType), Leftwards)
          case "term_rightwards" =>
            glyphs() = glyphs() :+ GlyphSpec(ClickAdder(Terminator.GlyphType), Rightwards)
          case "term_leftwards" =>
            glyphs() = glyphs() :+ GlyphSpec(ClickAdder(Terminator.GlyphType), Leftwards)
          case "pbs_rightwards" =>
            glyphs() = glyphs() :+ GlyphSpec(ClickAdder(PrimerBindingSite.GlyphType), Rightwards)
          case "pbs_leftwards" =>
            glyphs() = glyphs() :+ GlyphSpec(ClickAdder(PrimerBindingSite.GlyphType), Leftwards)
        }
      }
    }
  }


}
