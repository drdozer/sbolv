package talk

import org.scalajs.dom._
import rx.core._
import rx.ops._

import scala.scalajs.js.annotation.JSExport

/**
 *
 *
 * @author Matthew Pocock
 */
@JSExport
object FixedOrProportionalDemo {

  import Enhancements._

  @JSExport
  def wireFixdWidthExample(divId: String): Unit = {
    val div = document.getElementById(divId).asInstanceOf[HTMLDivElement]
    val lengthSlider = ReactiveSlider(div.getElementsByClassName("length_slider").elements.head.asInstanceOf[HTMLInputElement])
    val lengthSpan = div.getElementsByClassName("length").elements
    val buttons = div.getElementsByTagName("button").elements.map(_.asInstanceOf[HTMLButtonElement])
    val svg = div.getElementsByClassName("glyphs").elements.map(_.asInstanceOf[SVGSVGElement]).head
    val track = svg.getElementsByClassName("track").elements.map(_.asInstanceOf[SVGGElement]).head

    Obs(lengthSlider.valueAsNumber) {
      lengthSpan.foreach(_.textContent = lengthSlider.valueAsNumber().toString)
    }

    val glyphs = Var(Seq.empty[(Rx[Double], Rx[BackboneAlignment]) => GlyphFamily])
    val fw = FixedWidth(lengthSlider.valueAsNumber map (_.toDouble), Var(CentredOnBackbone : BackboneAlignment), glyphs)
    track(fw.allGlyphs)

    for(b <- buttons) {
      b.onclick = (e: Event) => {
        b.value match {
          case "promoter_rightwards" =>
            glyphs() = glyphs() :+ Promoter.fixedWidth(Rightwards)
          case "promoter_leftwards" =>
            glyphs() = glyphs() :+ Promoter.fixedWidth(Leftwards)
          case "cds_rightwards" =>
            glyphs() = glyphs() :+ Cds.fixedWidth(Rightwards)
          case "cds_leftwards" =>
            glyphs() = glyphs() :+ Cds.fixedWidth(Leftwards)
          case "res_rightwards" =>
            glyphs() = glyphs() :+ RibosomeEntrySite.fixedWidth(Rightwards)
          case "res_leftwards" =>
            glyphs() = glyphs() :+ RibosomeEntrySite.fixedWidth(Leftwards)
        }
      }
    }
  }


}
