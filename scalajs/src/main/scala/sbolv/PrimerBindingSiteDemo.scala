package sbolv

import org.scalajs.dom._
import rx.core._
import rx.ops._
import sbolv.PromoterDemo._
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport

@JSExport
object PrimerBindingSiteDemo extends GlyphDemo {

  import Enhancements._

  @JSExport
  def wireScaledExample(scaled_example: String, shortcode_example: String): Unit = {
    mkGlyphExample(scaled_example: String)
    mkShortcodeExample(shortcode_example, "pbs")
  }

  def mkGlyphExample(scaled_example: String): Unit = {
    val maxWidth = 300
    val maxHeight = 300
    val padding = 5

    val (widthSlider, widthRx) = mkLabelledSlider("width", 10, maxWidth, 50, "px", _.toDouble)
    val (heightSlider, heightRx) = mkLabelledSlider("height", 10, maxHeight, 50, "px", _.toDouble)
    val (lengthSlider, lengthRx) = mkPctSlider("length", 60)
    val (depthSlider, depthRx) = mkPctSlider("depth", 30)
    val (tickSlider, tickRx) = mkLabelledSlider("tickSlider", -100, 100, -15, "px", _.toDouble)

    val sliderEls = List(
      widthSlider, heightSlider, lengthSlider, depthSlider, tickSlider)

    val metrics = Rx {
      PrimerBindingSite.Metrics(length = lengthRx(), depth = depthRx(), tick = tickRx())
    }

    def glyphElement(hO: HorizontalOrientation, vO: VerticalOrientation) = mkGlyph(
      padding, hO, vO, widthRx, heightRx, metrics, PrimerBindingSite.apply)

    mkGlyphExample(scaled_example, sliderEls, glyphElement)
  }
}
