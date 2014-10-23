package sbolv


import org.scalajs.dom.{onclick => _, _}
import rx.core._
import rx.ops._
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import scala.util.Random
import scalatags.JsDom.all._
import scalatags.JsDom.svgTags._
import scalatags.JsDom.svgAttrs
import Framework._

/**
 *
 *
 * @author Matthew Pocock
 */
@JSExport
object PromoterDemo {

  @JSExport
  def wireScaledExample(scaled_example: String, shortcode_example: String) {
    val container = document.getElementById(scaled_example).asInstanceOf[HTMLDivElement]
    val sliders = container.getElementsByClassName("sliders")(0).asInstanceOf[HTMLElement]
    val glyphs = container.getElementsByClassName("glyphs")(0).asInstanceOf[HTMLElement]
    
    val shortcodeExample = document.getElementById(shortcode_example).asInstanceOf[HTMLDivElement]

    def mkLabelledSlider(name: String, minV: Int, maxV: Int, valueV: Int, units: String, scale: Int => Double) = {
      val slider = input(
        `class` := s"${name}_slider",
        `type` := "range",
        min := minV,
        max := maxV,
        value := valueV)
      val rSlider = ReactiveSlider(slider)
      val scaled = rSlider.valueAsNumber map scale

      val label = span(
        `class` := name)(
          span(`class` := "value", scaled),
          span(`class` := "units", units)
        )

      val sl = div(style := "display: inline-block")(
        div(width := "100%", marginLeft := "auto", marginRight := "auto", label),
        div(rSlider.slider))
      (sl, scaled)
    }

    val maxWidth = 300
    val maxHeight = 300

    val (widthSlider, widthRx) = mkLabelledSlider("width", 10, maxWidth, 50, "px", _.toDouble)
    val (heightSlider, heightRx) = mkLabelledSlider("height", 10, maxHeight, 50, "px", _.toDouble)
    val (verticalSlider, verticalRx) = mkLabelledSlider("vertical", 0, 100, 40, "%", _.toDouble * 0.01)
    val (horizontalSlider, horizontalRx) = mkLabelledSlider("horizontal", 0, 100, 60, "%", _.toDouble * 0.01)
    val (arrowWidthSlider, arrowWidthRx) = mkLabelledSlider("arrowWidth", 0, 100, 10, "%", _.toDouble * 0.01)
    val (arrowHeightSlider, arrowHeightRx) = mkLabelledSlider("arrowHeight", 0, 100, 10, "%", _.toDouble * 0.01)

    sliders.modifyWith(
      widthSlider,
      heightSlider,
      verticalSlider,
      horizontalSlider,
      arrowWidthSlider,
      arrowHeightSlider).render

    val promoterMetrics = Rx {
      Promoter.Metrics(vertical = verticalRx(),
                       horizontal = horizontalRx(),
                       arrowHeight = arrowHeightRx(),
                       arrowWidth = arrowWidthRx())
    }

    val padding = 5

    def mkGlyph(leftRight: HorizontalOrientation, upDown: VerticalOrientation) = {
      val promoter = Promoter(Var(leftRight), Var(upDown), widthRx, heightRx, promoterMetrics)

      svg(width := widthRx map (_ + padding * 2.0), height := heightRx map (_ + padding * 2.0),
        g(svgAttrs.transform := s"translate($padding $padding)",
        rect(`class` := "glyphBox", svgAttrs.width := widthRx, svgAttrs.height := heightRx),
        promoter.glyph))
    }

    glyphs.modifyWith(table(
      tr(
        td(`class` := "empty_cell"),
        td(`class` := "horizontal glyphGridLabel", colspan := 2, textAlign := "centre", "Upwards"),
        td(`class` := "empty_cell")),
      tr(
        td(`class` := "vertical glyphGridLabel", webkitTransform := "rotate(270deg)", rowspan := 2, "Leftwards"),
        td(mkGlyph(Leftwards, Upwards)),
        td(mkGlyph(Rightwards, Upwards)),
        td(`class` := "vertical glyphGridLabel", webkitTransform := "rotate(90deg)", rowspan := 2, "Rightwards")),
      tr(
        td(mkGlyph(Leftwards, Downwards)),
        td(mkGlyph(Rightwards, Downwards))),
      tr(
        td(`class` := "empty_cell"),
        td(`class` := "horizontal glyphGridLabel", colspan := 2, textAlign := "centre", "Downwards"),
        td(`class` := "empty_cell")))).render

    shortcodeExample.modifyWith(
      shortcodeText("promoter"),
      div(borderColor := "black", borderStyle := "solid",
        shortcodeText("promoter"),
        script("SBOLv().shortcodes()")),
      div("Shortcode processing is enabled within part of your document by including ",
          code("SBOLv().shortcodes()"),
          " in a &lt;script&gt; element. This will apply shortcodes to all of its siblings.")
    ).render
  }

  def shortcodeText(glyphName: String) = div(
    s"""You can embed $glyphName symbols directly into the flow of text by using SBOLv the $glyphName shortcode """,
    code(s"""[$glyphName]fish[/$glyphName]"""),
    s""" within normal text. If you don't need a label you can either use an empty shortcode ([$glyphName][/$glyphName])
       or a self-closing shortcode ([$glyphName/]). The width of the symbol can be set manually as in
       [$glyphName width="100"]looong[/$glyphName] and [$glyphName width="30"/] and the orientation with
       [$glyphName dir="<"/] and [$glyphName dir=">"/]."""
  )
}
