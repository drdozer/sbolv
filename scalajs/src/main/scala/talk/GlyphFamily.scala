package talk

import org.scalajs.dom._
import rx.core.{Var, Obs, Rx}
import talk.geom.{Box}

/**
 *
 *
 * @author Matthew Pocock
 */
trait GlyphFamily {
  type Metrics

  def glyph: SVGGElement
  def direction: Rx[Direction]
  def alignment: Rx[BackboneAlignment]
  def metrics: Rx[Metrics]
}

trait GlyphFamilyWithInnerLabel {
  self : GlyphFamily =>

  import Enhancements._

  def innerLabel: Rx[Option[String]]

  protected final val innerLabelText = "text".asSVGElement[SVGTextElement]("class" -> "sbolv_glyph_label")

  private val innerLabelTick = Var(0)
  private def tick() = innerLabelTick() = innerLabelTick() + 1
  innerLabelText.addEventListener("DOMNodeInsertedIntoDocument", (e: Event) => tick())
  innerLabelText.addEventListener("DOMNodeInserted", (e: Event) => tick())

  private val innerLabelText_transform = Rx {
    innerLabelTick() // for the temporal dependency
    val bounds = Box(innerLabelText.getBBox())
    val centre = bounds.centre
    s"translate(${-centre.x} ${-centre.y})"
  }

  private val innerLabelText_obs = Obs(innerLabel) {
    innerLabelText.textContent = innerLabel().getOrElse("")
    tick()
  }

  private val innerLabelText_transform_obs = Obs(innerLabelText_transform) {
    innerLabelText("transform" -> innerLabelText_transform())
  }
}

trait GlyphFamilyWithOuterLabel {
  self : GlyphFamily =>

  import Enhancements._
  def outerLabel: Rx[Option[String]]
  protected def offset: Rx[Double]

  protected final val outerLabelText = "text".asSVGElement[SVGTextElement]("class" -> "sbolv_glyph_label")
  private val outerLabelTick = Var(0)

  private val outerLabelText_obs = Obs(outerLabel) {
    outerLabelText.textContent = outerLabel().getOrElse("")
    outerLabelTick() = outerLabelTick() + 1
  }


  private val outerLabelText_transform = Rx {
    outerLabelTick() // for the temporal dependency
    val bounds = Box(outerLabelText.getBBox())
    val centre = bounds.centre
    val m = metrics()
    val o = offset()
    alignment() match {
      case AboveBackbone => s"translate(${-centre.x} ${+o +bounds.bottom})"
      case CentredOnBackbone => s"translate(${-centre.x} ${+o +bounds.bottom})"
      case BelowBackbone => s"translate(${-centre.x} ${+o -bounds.top})"
    }
  }

  private val outerLabelText_transform_obs = Obs(outerLabelText_transform) {
    outerLabelText("transform" -> outerLabelText_transform())
  }
}
