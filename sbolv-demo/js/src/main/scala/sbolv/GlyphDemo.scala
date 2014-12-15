package sbolv

import org.scalajs.dom._
import rx.core.{Var, Rx}
import rx.ops._

import scalatags.JsDom
import scalatags.JsDom.all._

import scalatags.ext._
import scalatags.ext.Framework._

import scalatags.JsDom.svgAttrs
import scalatags.JsDom.svgTags._

/**
 *
 *
 * @author Matthew Pocock
 */
trait GlyphDemo {

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

  def mkPctSlider(name: String, valueV: Int) = mkLabelledSlider(name, 0, 100, valueV, "%", _.toDouble * 0.01)

  def mkGlyph[M](padding: Double,
                 leftRight: HorizontalOrientation,
                 upDown: VerticalOrientation,
                 widthRx: Rx[Double],
                 heightRx: Rx[Double],
                 metrics: Rx[M],
                 glyphCstr: (Rx[HorizontalOrientation], Rx[VerticalOrientation], Rx[Double], Rx[Double], Rx[M]) => GlyphFamily) = {
    val glyphFamily = glyphCstr(Var(leftRight), Var(upDown), widthRx, heightRx, metrics)

    svg(width := widthRx map (_ + padding * 2.0), height := heightRx map (_ + padding * 2.0),
      g(svgAttrs.transform := s"translate($padding $padding)",
        rect(`class` := "glyphBox", svgAttrs.width := widthRx, svgAttrs.height := heightRx),
        glyphFamily.glyph))
  }

  def mkGlyphs(glyphs: Element, glyphElement: (HorizontalOrientation, VerticalOrientation) => JsDom.Frag): Unit = {
    glyphs.modifyWith(table(
      tr(
        td(`class` := "empty_cell"),
        td(`class` := "horizontal glyphGridLabel", colspan := 2, textAlign := "centre", "Upwards"),
        td(`class` := "empty_cell")),
      tr(
        td(`class` := "vertical glyphGridLabel", webkitTransform := "rotate(270deg)", rowspan := 2, "Leftwards"),
        td(glyphElement(Leftwards, Upwards)),
        td(glyphElement(Rightwards, Upwards)),
        td(`class` := "vertical glyphGridLabel", webkitTransform := "rotate(90deg)", rowspan := 2, "Rightwards")),
      tr(
        td(glyphElement(Leftwards, Downwards)),
        td(glyphElement(Rightwards, Downwards))),
      tr(
        td(`class` := "empty_cell"),
        td(`class` := "horizontal glyphGridLabel", colspan := 2, textAlign := "centre", "Downwards"),
        td(`class` := "empty_cell")))).render
  }

  def mkGlyphExample(scaled_example: String,
                     sliderEls: Seq[JsDom.Frag],
                     glyphElement: (HorizontalOrientation, VerticalOrientation) => JsDom.Frag): Unit =
  {
    val container = document.getElementById(scaled_example).asInstanceOf[HTMLDivElement]
    val glyphs = container.getElementsByClassName("glyphs")(0).asInstanceOf[HTMLElement]
    val sliders = container.getElementsByClassName("sliders")(0).asInstanceOf[HTMLElement]
    sliders.modifyWith(JsDom.all.SeqFrag(sliderEls)).render
    mkGlyphs(glyphs, glyphElement)
  }

  def mkShortcodeExample(shortcode_example: String, glyphName: String): Unit = {
    val shortcodeExample = document.getElementById(shortcode_example).asInstanceOf[HTMLDivElement]
    val sct = shortcodeText(glyphName)

    shortcodeExample.modifyWith(
      sct,
      div(borderColor := "black", borderStyle := "solid",
        sct,
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
