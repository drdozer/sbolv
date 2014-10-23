package sbolv

import org.scalajs.dom._
import rx.core._
import rx.ops._
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport

/**
 *
 *
 * @author Matthew Pocock
 */
@JSExport
object CdsDemo {

  import Enhancements._

  @JSExport
  def wireScaledExample(divId: String) {
    val div = document.getElementById(divId).asInstanceOf[HTMLDivElement]
    val lengthSlider = ReactiveSlider(div.getElementsByClassName("length_slider").elements.head.asInstanceOf[HTMLInputElement])
    val lengthSpan = div.getElementsByClassName("length").elements
    val depthSlider = ReactiveSlider(div.getElementsByClassName("depth_slider").elements.head.asInstanceOf[HTMLInputElement])
    val depthSpan = div.getElementsByClassName("depth").elements
    val headSlider = ReactiveSlider(div.getElementsByClassName("head_slider").elements.head.asInstanceOf[HTMLInputElement])
    val headSpan = div.getElementsByClassName("head").elements
    val topCds = div.getElementsByClassName("scaled_cds_top").elements.head.asInstanceOf[SVGGElement]
    val bottomCds = div.getElementsByClassName("scaled_cds_bottom").elements.head.asInstanceOf[SVGGElement]

    val padding = 5
    val length = lengthSlider.valueAsNumber
    val depth = depthSlider.valueAsNumber
    val head = headSlider.valueAsNumber

    Obs(length) {
      lengthSpan.foreach(_.textContent = length().toString)
    }

    Obs(depth) {
      depthSpan.foreach(_.textContent = depth().toString)
    }

    Obs(head) {
      headSpan.foreach(_.textContent = head().toString)
    }

    val width = Rx {
      length() + padding*2.0
    }
    val height = Rx {
      depth() + padding*2.0

    }
    val widthHeight = Rx {
      Math.max(width(), height())
    }
    val centre = Rx {
      widthHeight() * 0.5
    }

    val cdsMetrics = Rx {
      Cds.Metrics(length = length(), depth = depth(), head = head())
    }

    {
      val box = "rect".asSVGElement[SVGRectElement]("class" -> "glyphBox")
      Obs(widthHeight) {
        box("width" -> s"${widthHeight()}",
            "height" -> s"${widthHeight()}")
      }

      val cds = Cds(Var(Rightwards), Var(Upwards), width, height, cdsMetrics)
      val centred = "g".asSVGElement[SVGGElement](cds.glyph)

      Obs(centre) {
        centred.apply("transform" -> s"translate(${centre()} ${centre()})")
      }

      topCds(
        "transform" -> s"translate($padding $padding)"
      ).apply(box, centred)
    }

    {
      val box = "rect".asSVGElement[SVGRectElement]("class" -> "glyphBox")
      Obs(widthHeight) {
        box("width" -> s"${widthHeight()}",
            "height" -> s"${widthHeight()}")
      }

      val cds = Cds(Var(Leftwards), Var(Upwards), width, height, cdsMetrics)
      val centred = "g".asSVGElement[SVGGElement](cds.glyph)

      Obs(centre) {
        centred.apply("transform" -> s"translate(${centre()} ${centre()})")
      }

      bottomCds(
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
    val exampleG = div.getElementsByClassName("cds_on_backbone").elements.head

    val horizontalOrientation = Var(Rightwards : HorizontalOrientation)
    for(i <- directionRadio) i.onclick = { (me: MouseEvent) =>
      horizontalOrientation() = HorizontalOrientation.lowerCaseNames enumFor i.value
    }
    Obs(horizontalOrientation) {
      directionSpan.foreach(_.textContent = HorizontalOrientation.upperCaseNames nameFor horizontalOrientation())
    }

    val verticalOrientation = Var(Upwards : VerticalOrientation)
    for(i <- alignmentRadio) i.onclick = { (me: MouseEvent) =>
      verticalOrientation() = VerticalOrientation.lowerCaseNames enumFor i.value
    }
    for(i <- alignmentRadio) if(i.checked) verticalOrientation() = VerticalOrientation.lowerCaseNames enumFor i.value
    Obs(verticalOrientation) {
      alignmentSpan.foreach(_.textContent = verticalOrientation().toString)
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

    val inner = Var(None: Option[String])
    val outer = Var(None: Option[String])

    val cds = Cds(horizontalOrientation, verticalOrientation, Var(100), Var(100), Var(Cds.Metrics(0.6, 0.2, 0.1)))


    val placedCds = "g".asSVGElement[SVGGElement](
      "transform" -> s"translate(${100/2} 0)"
    ) apply(cds.glyph)

    exampleG(backbone, placedCds)

    inner() = Some("Inner")
    outer() = Some("Outer")
  }
}
