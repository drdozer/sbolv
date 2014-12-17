package sbolv

import org.scalajs.dom._
import org.scalajs.dom.extensions._

import rx._
import rx.ops._
import sbolv.GlyphFamily.GlyphSpec

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import scalatags.JsDom.all._
import scalatags.ext.Framework._

@JSExport
object PigeonParserDemo {

  @JSExport
  def wireExample(divId: String): Unit = {
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

        val label = Option(glyph.name).filter(_ != js.undefined).map(_.asInstanceOf[String]).filter(_.length > 0)

        val fill = Option(glyph.color).filter(_ != js.undefined).map(_.asInstanceOf[String]).filter(_.length > 0)

        val glyphType = glyph.`type`.asInstanceOf[String] match {
          case "terminator" => Terminator.GlyphType
          case "operator" => Operator.GlyphType
          case "cds" => Cds.GlyphType
          case "res" => RibosomeEntrySite.GlyphType
          case "promoter" => Promoter.GlyphType
          case "v" => Terminator.GlyphType
        }

        GlyphSpec(glyphType = glyphType, horizontalOrientation = direction, label = label, fill = fill)

      }.toIndexedSeq
    }
  }



}
