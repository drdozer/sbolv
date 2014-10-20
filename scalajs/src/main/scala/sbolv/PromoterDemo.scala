package sbolv

import org.scalajs.dom._
import rx.core._
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport

/**
 *
 *
 * @author Matthew Pocock
 */
@JSExport
object PromoterDemo {

  import Enhancements._

  @JSExport
  def wireScaledExample(divId: String) {
    val div = document.getElementById(divId).asInstanceOf[HTMLDivElement]
    val verticalSlider = ReactiveSlider(div.getElementsByClassName("vertical_slider").elements.head.asInstanceOf[HTMLInputElement])
    val verticalSpan = div.getElementsByClassName("vertical").elements
    val horizontalSlider = ReactiveSlider(div.getElementsByClassName("horizontal_slider").elements.head.asInstanceOf[HTMLInputElement])
    val horizontalSpan = div.getElementsByClassName("horizontal").elements
    val arrowWidthSlider = ReactiveSlider(div.getElementsByClassName("arrowWidth_slider").elements.head.asInstanceOf[HTMLInputElement])
    val arrowWidthSpan = div.getElementsByClassName("arrowWidth").elements
    val arrowHeightSlider = ReactiveSlider(div.getElementsByClassName("arrowHeight_slider").elements.head.asInstanceOf[HTMLInputElement])
    val arrowHeightSpan = div.getElementsByClassName("arrowHeight").elements
    val topPromoter = div.getElementsByClassName("scaled_promoter_top").elements.head.asInstanceOf[SVGGElement]
    val bottomPromoter = div.getElementsByClassName("scaled_promoter_bottom").elements.head.asInstanceOf[SVGGElement]

    val padding = 5

    Obs(verticalSlider.valueAsNumber) {
      verticalSpan.foreach(_.textContent = verticalSlider.valueAsNumber().toString)
    }
    Obs(horizontalSlider.valueAsNumber) {
      horizontalSpan.foreach(_.textContent = horizontalSlider.valueAsNumber().toString)
    }
    Obs(arrowWidthSlider.valueAsNumber) {
      arrowWidthSpan.foreach(_.textContent = arrowWidthSlider.valueAsNumber().toString)
    }
    Obs(arrowHeightSlider.valueAsNumber) {
      arrowHeightSpan.foreach(_.textContent = arrowHeightSlider.valueAsNumber().toString)
    }

    val width = Rx {
      verticalSlider.valueAsNumber()*2 + arrowWidthSlider.valueAsNumber() + padding*2
    }
    val height = Rx {
      horizontalSlider.valueAsNumber()*2 + arrowHeightSlider.valueAsNumber() + padding*2

    }
    val widthHeight = Rx {
      Math.max(width(), height())
    }
    val centre = Rx {
      widthHeight() / 2
    }

    val promoterMetrics = Rx {
      Promoter.Metrics(vertical = verticalSlider.valueAsNumber(),
                       horizontal = horizontalSlider.valueAsNumber(),
                       arrowHeight = arrowHeightSlider.valueAsNumber(),
                       arrowWidth = arrowWidthSlider.valueAsNumber())
    }

    {
      val box = "rect".asSVGElement[SVGRectElement]("class" -> "glyphBox")
      Obs(widthHeight) {
        box("width" -> s"${widthHeight()}",
            "height" -> s"${widthHeight()}")
      }

      val promoter = Promoter(Var(Rightwards), Var(CentredOnBackbone), Var(None), Var(0.0), promoterMetrics)
      val centred = "g".asSVGElement[SVGGElement](promoter.glyph)

      Obs(centre) {
        centred.apply("transform" -> s"translate(${centre()} ${centre()})")
      }

      topPromoter(
        "transform" -> s"translate($padding $padding)"
      ).apply(box, centred)
    }

    {
      val box = "rect".asSVGElement[SVGRectElement]("class" -> "glyphBox")
      Obs(widthHeight) {
        box("width" -> s"${widthHeight()}",
            "height" -> s"${widthHeight()}")
      }

      val promoter = Promoter(Var(Leftwards), Var(CentredOnBackbone), Var(None), Var(0.0), promoterMetrics)
      val centred = "g".asSVGElement[SVGGElement](promoter.glyph)

      Obs(centre) {
        centred.apply("transform" -> s"translate(${centre()} ${centre()})")
      }

      bottomPromoter(
        "transform" -> s"translate($padding $padding)"
      ).apply(box, centred)
    }
  }

  @JSExport
  def wireAlignmentExample(divId: String): Unit = {
    val div = document.getElementById(divId).asInstanceOf[HTMLDivElement]
    val alignmentRadio = div.getElementsByTagName("input").elements.
      map(_.asInstanceOf[HTMLInputElement]).filter(_.name == "alignment")
    val alignmentSpan = div.getElementsByClassName("alignment").elements
    val directionRadio = div.getElementsByTagName("input").elements.
      map(_.asInstanceOf[HTMLInputElement]).filter(_.name == "direction")
    val directionSpan = div.getElementsByClassName("direction").elements
    val exampleG = div.getElementsByClassName("promoter_on_backbone").elements.head

    var direction = Var(Rightwards : HorizontalOrientation)
    for(i <- directionRadio) i.onclick = { (me: MouseEvent) =>
      direction() = i.value match {
        case "rightwards" => Rightwards
        case "leftwards" => Leftwards
      }
    }
    Obs(direction) {
      directionSpan.foreach(_.textContent = direction().toString)
    }

    var alignment = Var(CentredOnBackbone : BackboneAlignment)
    for(i <- alignmentRadio) if(i.checked) alignment() = BackboneAlignment.parse(i.value)

    for(i <- alignmentRadio) i.onclick = { (me: MouseEvent) =>
      alignment() = BackboneAlignment.parse(i.value)
    }
    for(i <- alignmentRadio) if(i.checked) alignment() = BackboneAlignment.parse(i.value)
    Obs(alignment) {
      alignmentSpan.foreach(_.textContent = alignment().toString)
    }

    val forwardStrand = "line".asSVGElement[SVGLineElement](
      "x1" -> "0",
      "y1" -> "-2",
      "x2" -> "100",
      "y2" -> "-2",
      "style" -> "stroke-dasharray: 6 2; stroke-width: 2; stroke: darkgrey;"
    )
    val reverseStrand = "line".asSVGElement[SVGLineElement](
      "x1" -> "0",
      "y1" -> "2",
      "x2" -> "100",
      "y2" -> "2",
      "style" -> "stroke-dasharray: 6 2; stroke-width: 2; stroke: darkgrey;"
    )

    val backbone = "g".asSVGElement[SVGGElement](forwardStrand, reverseStrand)

    val outer = Var(None: Option[String])

    val promoter = Promoter(direction, alignment, outer, Var(7.0), Var(Promoter.Metrics(30, 30, 10, 10)))


    val placedCds = "g".asSVGElement[SVGGElement](
      "transform" -> s"translate(${100/2} 0)"
    ) apply(promoter.glyph)

    exampleG(backbone, placedCds)

    outer() = Some("Outer")
  }
}
