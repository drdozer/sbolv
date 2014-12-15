package sbolv


import org.scalajs.dom.{onclick => _, _}
import rx.core._
import scala.scalajs.js.annotation.JSExport

/**
 *
 *
 * @author Matthew Pocock
 */
@JSExport
object PromoterDemo extends GlyphDemo {

  @JSExport
  def wireScaledExample(scaled_example: String, shortcode_example: String): Unit = {
    mkGlyphExample(scaled_example: String)
    mkShortcodeExample(shortcode_example, "promoter")
  }

  def mkGlyphExample(scaled_example: String): Unit = {
    val maxWidth = 300
    val maxHeight = 300
    val padding = 5

    val (widthSlider, widthRx) = mkLabelledSlider("width", 10, maxWidth, 50, "px", _.toDouble)
    val (heightSlider, heightRx) = mkLabelledSlider("height", 10, maxHeight, 50, "px", _.toDouble)
    val (verticalSlider, verticalRx) = mkPctSlider("vertical", 40)
    val (horizontalSlider, horizontalRx) = mkPctSlider("horizontal", 60)
    val (arrowWidthSlider, arrowWidthRx) = mkPctSlider("arrowWidth", 10)
    val (arrowHeightSlider, arrowHeightRx) = mkPctSlider("arrowHeight", 10)

    val sliderEls = List(
      widthSlider, heightSlider, verticalSlider, horizontalSlider, arrowWidthSlider, arrowHeightSlider)

    val metrics = Rx {
      Promoter.Metrics(
        vertical = verticalRx(),
        horizontal = horizontalRx(),
        arrowHeight = arrowHeightRx(),
        arrowWidth = arrowWidthRx())
    }

    def glyphElement(hO: HorizontalOrientation, vO: VerticalOrientation) = mkGlyph(
      padding, hO, vO, widthRx, heightRx, metrics, Promoter.apply)

    mkGlyphExample(scaled_example, sliderEls, glyphElement)
  }
}
