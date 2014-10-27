package sbolv

import org.scalajs.dom._
import rx.core._
import rx.ops._
import sbolv.geom.Point2

import scala.scalajs.js.annotation.JSExport
import scalatags.JsDom
import scalatags.JsDom.GenericStyle
import scalatags.generic.Attr
import Framework._

/**
 *
 *
 * @author Matthew Pocock
 */
@JSExport
object FixedOrProportionalDemo {

  import Enhancements._

  @JSExport
  def wireFixedWidthExample(divId: String): Unit = {
    val exampleDiv = document.getElementById(divId).asInstanceOf[HTMLDivElement]
    val lengthSlider = ReactiveSlider(exampleDiv.getElementsByClassName("length_slider").elements.head.asInstanceOf[HTMLInputElement])
    val lengthSpan = exampleDiv.getElementsByClassName("length").elements
    val buttons = exampleDiv.getElementsByTagName("button").elements.map(_.asInstanceOf[HTMLButtonElement])
    val svg = exampleDiv.getElementsByClassName("glyphs").elements.map(_.asInstanceOf[SVGSVGElement]).head
    val track = svg.getElementsByClassName("track").elements.map(_.asInstanceOf[SVGGElement]).head

    Obs(lengthSlider.valueAsNumber) {
      lengthSpan.foreach(_.textContent = lengthSlider.valueAsNumber().toString)
    }

    val glyphs = Var(IndexedSeq.empty[GlyphFactory])
    val fw = FixedWidth(lengthSlider.valueAsNumber map (_.toDouble), Var(CentredOnBackbone : BackboneAlignment), glyphs)
    track(fw.allGlyphs)

    import scalatags.JsDom.all._
    import Framework._

    val selectedGlyph: Var[Option[GlyphFamily]] = Var(None)

    val selectedGlyphVars = Rx {
      val io = for(
        g <- selectedGlyph()
      ) yield {
//        val inner = g match {
//          case i : GlyphFamilyWithInnerLabel =>
//            Some(i.innerLabel.asInstanceOf[Var[Option[String]]])
//          case _ => None
//        }
//        val outer = g match {
//          case o : GlyphFamilyWithOuterLabel =>
//            Some(o.outerLabel.asInstanceOf[Var[Option[String]]])
//          case _ => None
//        }

        (/* inner, outer, */ Some(g.horizontalOrientation.asInstanceOf[Var[HorizontalOrientation]]))
      }

      io.getOrElse((/* None, None,*/ None))
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
        import scalajs.js._
        import Enhancements._
        def deleteHandler(e: Event): Unit = {
          for(gl <- selectedGlyph()) {
            selectedGlyph() = None
            val holder = Dynamic(gl.glyph.parentNode.parentNode).__sbolv_widget.asInstanceOf[FixedWidth.GlyphHolder]
            val indx = holder.index()
            val newGlyphs = glyphs().zipWithIndex.filter(_._2 != indx).unzip._1
            glyphs() = newGlyphs
          }
        }
        span(`class` := "delete_glyph"){
          button("x", Events.click := deleteHandler _)
        }
      }

      val ( /* innerRxO, outerRxO, */ dO) = selectedGlyphVars()
      div(
        `class` := "glyph_editor")(
          deleter,
          flipper(dO) /*,
          editor(innerRxO, "inner"),
          editor(outerRxO, "outer") */
      )
    }

    val labelEditorPosition = Var(Point2(0,0))
    val labelEditor = div(
      position.absolute,
      display := selectedGlyph map { g => if(g.isDefined) "block" else "none"},
      JsDom.all.left := labelEditorPosition map (_.x px),
      JsDom.all.top := labelEditorPosition map (_.y px),
      selectionWidget).render
    exampleDiv.modifyWith(labelEditor).render

    case class ClickAdder(gffw: GlyphFamily.FixedWidth) extends GlyphFamily.FixedWidth {
      override def apply(direction: HorizontalOrientation):
      (Rx[Double], Rx[VerticalOrientation]) => GlyphFamily = {
        import scalajs.js.Dynamic

        val gf = gffw(direction)

        (w, d) => {
          val g = gf(w, d)
          g.glyph.addEventListener("click", (e: Event) => {
            val me = e.asInstanceOf[MouseEvent]
            e.stopPropagation()
            selectedGlyph() = Some(g)
            labelEditorPosition() = Point2(Dynamic(e).pageX.asInstanceOf[Double], Dynamic(e).pageY.asInstanceOf[Double])
          }, true)
          g
        }
      }
    }

    svg.modifyWith(
      Events.click := {(me: Event) => selectedGlyph() = None }
    ).render

    for(b <- buttons) {
      b.onclick = (e: Event) => {
        b.value match {
          case "promoter_rightwards" =>
            glyphs() = glyphs() :+ GlyphFactory(ClickAdder(Promoter.FixedWidth), Rightwards, None)
          case "promoter_leftwards" =>
            glyphs() = glyphs() :+ GlyphFactory(ClickAdder(Promoter.FixedWidth), Leftwards, None)
          case "cds_rightwards" =>
            glyphs() = glyphs() :+ GlyphFactory(ClickAdder(Cds.FixedWidth), Rightwards, None)
          case "cds_leftwards" =>
            glyphs() = glyphs() :+ GlyphFactory(ClickAdder(Cds.FixedWidth), Leftwards, None)
          case "res_rightwards" =>
            glyphs() = glyphs() :+ GlyphFactory(ClickAdder(RibosomeEntrySite.FixedWidth), Rightwards, None)
          case "res_leftwards" =>
            glyphs() = glyphs() :+ GlyphFactory(ClickAdder(RibosomeEntrySite.FixedWidth), Leftwards, None)
          case "term_rightwards" =>
            glyphs() = glyphs() :+ GlyphFactory(ClickAdder(Terminator.FixedWidth), Rightwards, None)
          case "term_leftwards" =>
            glyphs() = glyphs() :+ GlyphFactory(ClickAdder(Terminator.FixedWidth), Leftwards, None)
        }
      }
    }
  }


}
