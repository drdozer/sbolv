
package sbolv

import org.scalajs.dom._
import rx.core._
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport

@JSExport
object ProteinStabilityElementDemo extends GlyphDemo {
  @JSExport
  def wireScaledExample(scaled_example: String, shortcode_example: String): Unit = {
    mkGlyphExample(scaled_example: String)
    mkShortcodeExample(shortcode_example, "pts")
  }

  def mkGlyphExample(scaled_example: String): Unit = {
    val maxWidth = 300
    val maxHeight = 300
    val padding = 5

    val (widthSlider, widthRx) = mkLabelledSlider("width", 10, maxWidth, 50, "px", _.toDouble)
    val (heightSlider, heightRx) = mkLabelledSlider("height", 10, maxHeight, 50, "px", _.toDouble)
    val (lengthSlider, lengthRx) = mkPctSlider("length", 60)
    val (depthSlider, depthRx) = mkPctSlider("depth", 30)
    val (stemHeightSlider, stemHeightRx) = mkPctSlider("stemHeight", 30)

    val sliderEls = List(
      widthSlider, heightSlider, lengthSlider, depthSlider, stemHeightSlider)

    val metrics = Rx {
      StemmyGlyph.Metrics(length = lengthRx(), depth = depthRx(), stemHeight = stemHeightRx())
    }

    def glyphElement(hO: HorizontalOrientation, vO: VerticalOrientation) = mkGlyph(
          padding, hO, vO, widthRx, heightRx, metrics, ProteinStabilityElement.apply)

    mkGlyphExample(scaled_example, sliderEls, glyphElement)
  }
}
