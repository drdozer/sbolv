package talk

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

    val glyphs = Var(Seq.empty[(Rx[Double], Rx[BackboneAlignment]) => GlyphFamily])
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
        def deleteHandler(e: Event): Unit = {
          println(s"Deleting glyph: ${selectedGlyph().get}")
          glyphs() = glyphs() filterNot (_ == selectedGlyph().get)
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

    def addSelectHandler(gf: (Rx[Double], Rx[BackboneAlignment]) => GlyphFamily):
    (Rx[Double], Rx[BackboneAlignment]) => GlyphFamily = (w: Rx[Double], a: Rx[BackboneAlignment]) =>
    {
      val g = gf(w, a)
      g.glyph.addEventListener("click", (e: Event) => {
        selectedGlyph() = Some(g)
      }, true)
      g
    }

    for(b <- buttons) {
      b.onclick = (e: Event) => {
        b.value match {
          case "promoter_rightwards" =>
            glyphs() = glyphs() :+ addSelectHandler(Promoter.fixedWidth(Rightwards))
          case "promoter_leftwards" =>
            glyphs() = glyphs() :+ addSelectHandler(Promoter.fixedWidth(Leftwards))
          case "cds_rightwards" =>
            glyphs() = glyphs() :+ addSelectHandler(Cds.fixedWidth(Rightwards))
          case "cds_leftwards" =>
            glyphs() = glyphs() :+ addSelectHandler(Cds.fixedWidth(Leftwards))
          case "res_rightwards" =>
            glyphs() = glyphs() :+ addSelectHandler(RibosomeEntrySite.fixedWidth(Rightwards))
          case "res_leftwards" =>
            glyphs() = glyphs() :+ addSelectHandler(RibosomeEntrySite.fixedWidth(Leftwards))
          case "term_rightwards" =>
            glyphs() = glyphs() :+ addSelectHandler(Terminator.fixedWidth(Rightwards))
          case "term_leftwards" =>
            glyphs() = glyphs() :+ addSelectHandler(Terminator.fixedWidth(Leftwards))
        }
      }
    }
  }


}
