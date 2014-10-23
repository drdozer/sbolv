package sbolv

import rx.core._
import scala.scalajs.js.annotation.JSExport

/**
 *
 *
 * @author Matthew Pocock
 */
@JSExport
object RibosomeEntrySiteDemo extends GlyphDemo {
  @JSExport
  def wireScaledExample(scaled_example: String, shortcode_example: String): Unit = {
    mkGlyphExample(scaled_example: String)
    mkShortcodeExample(shortcode_example, "res")
  }

  def mkGlyphExample(scaled_example: String): Unit = {
    val maxWidth = 300
    val maxHeight = 300
    val padding = 5

    val (widthSlider, widthRx) = mkLabelledSlider("width", 10, maxWidth, 50, "px", _.toDouble)
    val (heightSlider, heightRx) = mkLabelledSlider("height", 10, maxHeight, 50, "px", _.toDouble)
    val (lengthSlider, lengthRx) = mkPctSlider("length", 60)
    val (depthSlider, depthRx) = mkPctSlider("depth", 30)

    val sliderEls = List(
      widthSlider, heightSlider, lengthSlider, depthSlider)

    val metrics = Rx {
      BoxyGlyph.Metrics(length = lengthRx(), depth = depthRx())
    }

    def glyphElement(hO: HorizontalOrientation, vO: VerticalOrientation) = mkGlyph(
          padding, hO, vO, widthRx, heightRx, metrics, RibosomeEntrySite.apply)

    mkGlyphExample(scaled_example, sliderEls, glyphElement)
  }
}
