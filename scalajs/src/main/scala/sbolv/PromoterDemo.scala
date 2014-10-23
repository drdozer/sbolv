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
  def wireScaledExample(divId: String) {
    val container = document.getElementById(divId).asInstanceOf[HTMLDivElement]
    val sliders = container.getElementsByClassName("sliders")(0).asInstanceOf[HTMLElement]
    val glyphs = container.getElementsByClassName("glyphs")(0).asInstanceOf[HTMLElement]

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
  }

//  @JSExport
//  def wireAlignmentExample(divId: String): Unit = {
//    import Enhancements._
//
//    val div = document.getElementById(divId).asInstanceOf[HTMLDivElement]
//    val alignmentRadio = div.getElementsByTagName("input").elements.
//      map(_.asInstanceOf[HTMLInputElement]).filter(_.name == "alignment")
//    val alignmentSpan = div.getElementsByClassName("alignment").elements
//    val directionRadio = div.getElementsByTagName("input").elements.
//      map(_.asInstanceOf[HTMLInputElement]).filter(_.name == "direction")
//    val directionSpan = div.getElementsByClassName("direction").elements
//    val exampleG = div.getElementsByClassName("promoter_on_backbone").elements.head
//
//    val horizontalOrientation = Var(Rightwards : HorizontalOrientation)
//    for(i <- directionRadio) i.onclick = { (me: MouseEvent) =>
//      horizontalOrientation() = HorizontalOrientation.lowerCaseNames enumFor i.value
//    }
//    Obs(horizontalOrientation) {
//      directionSpan.foreach(_.textContent = HorizontalOrientation.upperCaseNames nameFor horizontalOrientation())
//    }
//
//    val verticalOrientation = Var(Upwards : VerticalOrientation)
//    for(i <- alignmentRadio) i.onclick = { (me: MouseEvent) =>
//      verticalOrientation() = VerticalOrientation.lowerCaseNames enumFor i.value
//    }
//    for(i <- alignmentRadio) if(i.checked) verticalOrientation() = VerticalOrientation.lowerCaseNames enumFor i.value
//    Obs(verticalOrientation) {
//      alignmentSpan.foreach(_.textContent = verticalOrientation().toString)
//    }
//
//    val forwardStrand = "line".asSVGElement[SVGLineElement](
//      "x1" -> "0",
//      "y1" -> "-2",
//      "x2" -> "100",
//      "y2" -> "-2",
//      "style" -> "stroke-dasharray: 6 2; stroke-width: 2; stroke: darkgrey;"
//    )
//    val reverseStrand = "line".asSVGElement[SVGLineElement](
//      "x1" -> "0",
//      "y1" -> "2",
//      "x2" -> "100",
//      "y2" -> "2",
//      "style" -> "stroke-dasharray: 6 2; stroke-width: 2; stroke: darkgrey;"
//    )
//
//    val backbone = "g".asSVGElement[SVGGElement](forwardStrand, reverseStrand)
//
//    val outer = Var(None: Option[String])
//
//    val promoter = Promoter(horizontalOrientation, verticalOrientation, Var(100), Var(100), Var(Promoter.Metrics(0.9, 0.9, 0.1, 0.1)))
//
//
//    val placedCds = "g".asSVGElement[SVGGElement](
//      "transform" -> s"translate(${100/2} 0)"
//    ) apply(promoter.glyph)
//
//    exampleG(backbone, placedCds)
//
//    outer() = Some("Outer")
//  }
}
