package sbolv

import org.scalajs.dom._
import rx.core._
import rx.ops._
import sbolv.geom.Point2

import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js
import scalatags.JsDom
import scalatags.JsDom.GenericStyle
import scalatags.generic.Attr
import Framework._

@JSExport
object PigeonParserDemo {

  import Enhancements._

  @JSExport
  def wireExample(divId: String): Unit = {
    val exampleDiv = document.getElementById(divId).asInstanceOf[HTMLDivElement]
    val lengthSlider = ReactiveSlider(exampleDiv.getElementsByClassName("length_slider").elements.head.asInstanceOf[HTMLInputElement])
    val lengthSpan = exampleDiv.getElementsByClassName("length").elements
    val buttons = exampleDiv.getElementsByTagName("button").elements.map(_.asInstanceOf[HTMLButtonElement])
    val fixedWidthSvg = exampleDiv.getElementsByClassName("glyphs").elements.map(_.asInstanceOf[SVGSVGElement]).head
    val track = fixedWidthSvg.getElementsByClassName("track").elements.map(_.asInstanceOf[SVGGElement]).head

    Obs(lengthSlider.valueAsNumber) {
      lengthSpan.foreach(_.textContent = lengthSlider.valueAsNumber().toString)
    }

    val glyphs = Var(IndexedSeq.empty[GlyphFactory])
    val fw = FixedWidth(lengthSlider.valueAsNumber map (_.toDouble), Var(CentredOnBackbone : BackboneAlignment), glyphs)
    track(fw.allGlyphs)

    import scalatags.JsDom.all._
    import Framework._

    val pigeonBox = ReactiveTextArea(document.getElementById("pigeon").asInstanceOf[HTMLTextAreaElement])
    val jsonBox = document.getElementById("json").asInstanceOf[HTMLTextAreaElement]

    Obs(pigeonBox.value) {

        val pigeon = pigeonBox.value().toString().asInstanceOf[String]
        val displayList = js.Dynamic.global.window.parsePigeon(pigeon)
        
        jsonBox.value = js.Dynamic.global.JSON.stringify(displayList, null, 4).asInstanceOf[String]

        val segments = displayList.segments.asInstanceOf[js.Array[js.Dynamic]].toList

        val sequence = segments.head.sequence.asInstanceOf[js.Array[js.Dynamic]].toList

        glyphs() = sequence.map { glyph =>


                val direction = glyph.direction.asInstanceOf[String] match {
                    case "rightwards" =>
                        Rightwards
                    case "leftwards" =>
                        Leftwards
                }

                val name = glyph.name.asInstanceOf[String]

                glyph.`type`.asInstanceOf[String] match {
                    case "terminator" => 
                        GlyphFactory(Terminator.FixedWidth, direction, Option(name))
                    case "operator" => 
                        GlyphFactory(Operator.FixedWidth, direction, Option(name))
                    case "cds" => 
                        GlyphFactory(Cds.FixedWidth, direction, Option(name))
                    case "res" => 
                        GlyphFactory(RibosomeEntrySite.FixedWidth, direction, Option(name))
                    case "promoter" => 
                        GlyphFactory(Promoter.FixedWidth, direction, Option(name))
                    case "v" => 
                        GlyphFactory(Terminator.FixedWidth, direction, Option(name))

                }

        }.toIndexedSeq
    }
}



}
