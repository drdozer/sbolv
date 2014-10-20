
package sbolv

import org.scalajs.dom._
import rx.core._
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport

@JSExport
object TerminatorDemo {

  import Enhancements._

  @JSExport
  def wireScaledExample(divId: String) {
    val div = document.getElementById(divId).asInstanceOf[HTMLDivElement]
    val lengthSlider = ReactiveSlider(div.getElementsByClassName("length_slider").elements.head.asInstanceOf[HTMLInputElement])
    val lengthSpan = div.getElementsByClassName("length").elements
    val depthSlider = ReactiveSlider(div.getElementsByClassName("depth_slider").elements.head.asInstanceOf[HTMLInputElement])
    val depthSpan = div.getElementsByClassName("depth").elements
    val topTerminator = div.getElementsByClassName("scaled_term_top").elements.head.asInstanceOf[SVGGElement]
    val bottomTerminator = div.getElementsByClassName("scaled_term_bottom").elements.head.asInstanceOf[SVGGElement]

    val padding = 5
    val length = lengthSlider.valueAsNumber
    val depth = depthSlider.valueAsNumber

    Obs(length) {
      lengthSpan.foreach(_.textContent = length().toString)
    }

    Obs(depth) {
      depthSpan.foreach(_.textContent = depth().toString)
    }

    val width = Rx {
      length() + padding*2
    }
    val height = Rx {
      depth() + padding*2

    }
    val widthHeight = Rx {
      Math.max(width(), height())
    }
    val centre = Rx {
      widthHeight() / 2
    }

    val termMetrics = Rx {
      Terminator.Metrics(length = length(), depth = depth())
    }

    {
      val box = "rect".asSVGElement[SVGRectElement]("class" -> "glyphBox")
      Obs(widthHeight) {
        box("width" -> s"${widthHeight()}",
            "height" -> s"${widthHeight()}")
      }

      val term = Terminator(Var(Rightwards), Var(CentredOnBackbone), Var(None), Var(0.0), termMetrics)
      val centred = "g".asSVGElement[SVGGElement](term.glyph)

      Obs(centre) {
        centred.apply("transform" -> s"translate(${centre()} ${centre()})")
      }

      topTerminator(
        "transform" -> s"translate($padding $padding)"
      ).apply(box, centred)
    }

    {
      val box = "rect".asSVGElement[SVGRectElement]("class" -> "glyphBox")
      Obs(widthHeight) {
        box("width" -> s"${widthHeight()}",
            "height" -> s"${widthHeight()}")
      }

      val term = Terminator(Var(Leftwards), Var(CentredOnBackbone), Var(None), Var(0.0), termMetrics)
      val centred = "g".asSVGElement[SVGGElement](term.glyph)

      Obs(centre) {
        centred.apply("transform" -> s"translate(${centre()} ${centre()})")
      }

      bottomTerminator(
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
    val exampleG = div.getElementsByClassName("term_on_backbone").elements.head

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

    val inner = Var(None: Option[String])
    val outer = Var(None: Option[String])

    val term = Terminator(direction, alignment, outer, Var(7.0), Var(Terminator.Metrics(60, 20)))

    val placedTerminator = "g".asSVGElement[SVGGElement](
      "transform" -> s"translate(${100/2} 0)"
    ) apply(term.glyph)

    exampleG(backbone, placedTerminator)

    outer() = Some("Outer")
  }
}
