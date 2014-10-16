package sbolv

import org.scalajs.dom._
import rx.core._
import rx.ops._

import scala.scalajs.js.annotation.JSExport
import scalatags.generic.Attr

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
    val labelEditor = exampleDiv.getElementsByClassName("label_editor").elements.head.asInstanceOf[HTMLDivElement]
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
        val inner = g match {
          case i : GlyphFamilyWithInnerLabel =>
            Some(i.innerLabel.asInstanceOf[Var[Option[String]]])
          case _ => None
        }
        val outer = g match {
          case o : GlyphFamilyWithOuterLabel =>
            Some(o.outerLabel.asInstanceOf[Var[Option[String]]])
          case _ => None
        }

        (inner, outer, Some(g.direction.asInstanceOf[Var[Direction]]))
      }

      io.getOrElse((None, None, None))
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
            onkeypress := changeHandler _,
            Attr("oninput") := changeHandler _)
        )
      }

      def flipper(dO: Option[Var[Direction]]) = dO map { d =>
        def flipHandler(e: Event) = d() = d().reverse

        span(`class` := "direction_editor")(
          button("flip", onclick := flipHandler _)
        )
      }


      def deleter = {
        import scalajs.js._
        import Enhancements._
        def deleteHandler(e: Event): Unit = {
          for(gl <- selectedGlyph()) {
            selectedGlyph() = None
            val holder = Dynamic(gl.glyph.parentNode).__sbolv_widget.asInstanceOf[FixedWidth.GlyphHolder]
            val indx = holder.index()
            val newGlyphs = glyphs().zipWithIndex.filter(_._2 != indx).unzip._1
            glyphs() = newGlyphs
          }
        }
        span(`class` := "delete_glyph"){
          button("x", onclick := deleteHandler _)
        }
      }

      val (innerRxO, outerRxO, dO) = selectedGlyphVars()
      div(
        `class` := "glyph_editor")(
          deleter,
          flipper(dO),
          editor(innerRxO, "inner"),
          editor(outerRxO, "outer")
      )
    }

    labelEditor.appendChild(span(selectionWidget).render)
    labelEditor.setAttribute("style", "position: absolute; display: none")
    Obs(selectedGlyph) {
      labelEditor.setAttribute("style", "position: absolute; display: none")
    }

    case class ClickAdder(gffw: GlyphFamily.FixedWidth) extends GlyphFamily.FixedWidth {
      override def apply(direction: Direction, label: Option[String]): (Rx[Double], Rx[BackboneAlignment]) => GlyphFamily = {
        import scalajs.js.Dynamic

        val gf = gffw(direction, label)

        (w, d) => {
          val g = gf(w, d)
          g.glyph.addEventListener("click", (e: Event) => {
            val me = e.asInstanceOf[MouseEvent]
            e.stopPropagation()
            selectedGlyph() = Some(g)
            labelEditor.setAttribute("style", s"position: absolute; left: ${Dynamic(me).pageX}px; top: ${Dynamic(me).pageY}px")
          }, true)
          g
        }
      }
    }

    svg.addEventListener("click", (me: Event) => {
      selectedGlyph() = None })

    for(b <- buttons) {
      b.onclick = (e: Event) => {
        b.value match {
          case "promoter_rightwards" =>
            glyphs() = glyphs() :+ GlyphFactory(ClickAdder(Promoter.FixedWidth), Rightwards)
          case "promoter_leftwards" =>
            glyphs() = glyphs() :+ GlyphFactory(ClickAdder(Promoter.FixedWidth), Leftwards)
          case "cds_rightwards" =>
            glyphs() = glyphs() :+ GlyphFactory(ClickAdder(Cds.FixedWidth), Rightwards)
          case "cds_leftwards" =>
            glyphs() = glyphs() :+ GlyphFactory(ClickAdder(Cds.FixedWidth), Leftwards)
          case "res_rightwards" =>
            glyphs() = glyphs() :+ GlyphFactory(ClickAdder(RibosomeEntrySite.FixedWidth), Rightwards)
          case "res_leftwards" =>
            glyphs() = glyphs() :+ GlyphFactory(ClickAdder(RibosomeEntrySite.FixedWidth), Leftwards)
          case "term_rightwards" =>
            glyphs() = glyphs() :+ GlyphFactory(ClickAdder(Terminator.FixedWidth), Rightwards)
          case "term_leftwards" =>
            glyphs() = glyphs() :+ GlyphFactory(ClickAdder(Terminator.FixedWidth), Leftwards)
        }
      }
    }
  }


}
