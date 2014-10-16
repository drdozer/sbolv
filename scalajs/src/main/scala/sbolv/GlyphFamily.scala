package sbolv

import java.util.Random

import org.scalajs.dom._
import rx.core.{Var, Obs, Rx}
import sbolv.geom.{Box}

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

object GlyphFamily {
  private var ctr = 0
  trait FixedWidth {
    def apply(direction: Direction, label: Option[String] = None): (Rx[Double], Rx[BackboneAlignment]) => GlyphFamily
    val uuid: Int = {
      val u = ctr
      ctr = ctr + 1
      u
    }
  }

  object FixedWidth {
    implicit val ordering: Ordering[FixedWidth] = Ordering.by(_.uuid)
  }
}

trait GlyphFamilyWithInnerLabel {
  self : GlyphFamily =>

  import scalatags.JsDom.implicits._
  import scalatags.JsDom.svgTags._
  import scalatags.JsDom.svgAttrs._
  import Framework._

  def innerLabel: Rx[Option[String]]

  private val innerLabelTick = Var(0)
  private def tickInner() = innerLabelTick() = innerLabelTick() + 1

  private val innerLabelClean = Rx {
    innerLabel() getOrElse ""
  }

  private lazy val innerLabelText_transform = Var("translate(0 0)")

  protected final val innerLabelText = {
    val txt = text(
      `class` := "sbolv_glyph_label",
      transform := (innerLabelText_transform : Rx[String]),
      DOMNodeInsertedIntoDocument := { (e: Event) =>
        tickInner() },
      DOMNodeInserted := { (e: Event) =>
        tickInner() },
      DOMSubtreeModified := { (e: Event) =>
        tickInner() }
    )(innerLabelClean).render


    val textBox = Rx {
      innerLabelTick()
      innerLabelClean()
      Box(txt.getBBox())
    }

    val innerLabelText_transform_obs = Obs(textBox) {
      val bounds = textBox()
      val centre = bounds.centre
      innerLabelText_transform() = s"translate(${-centre.x} ${-centre.y})"
    }

    txt
  }

}

trait GlyphFamilyWithOuterLabel {
  self : GlyphFamily =>

  import scalatags.JsDom.implicits._
  import scalatags.JsDom.svgTags._
  import scalatags.JsDom.svgAttrs._
  import Framework._

  def outerLabel: Rx[Option[String]]
  protected def offset: Rx[Double]

  private val outerLabelTick = Var(0)
  private def tickOuter() = outerLabelTick() = outerLabelTick() + 1

  private lazy val outerLabelClean = Rx {
    outerLabel() getOrElse ""
  }

  private lazy val outerLabelText_transform = Var("translate(0 0)")

  protected final val outerLabelText = {
    val txt = text(
      `class` := "sbolv_glyph_label",
      transform := (outerLabelText_transform : Rx[String]),
      DOMNodeInsertedIntoDocument := { (e: Event) => tickOuter() },
      DOMNodeInserted := { (e: Event) => tickOuter() },
      DOMSubtreeModified := { (e: Event) => tickOuter() }
    )(outerLabelClean).render

    val textBox = Rx {
      outerLabelTick()
      outerLabelClean()
      Box(txt.getBBox())
    }

    val trans = Rx {
      val bounds = textBox()
      val centre = bounds.centre
      val m = metrics()
      val o = offset()
      alignment() match {
        case AboveBackbone => s"translate(${-centre.x} ${+o +bounds.bottom})"
        case CentredOnBackbone => s"translate(${-centre.x} ${+o +bounds.bottom})"
        case BelowBackbone => s"translate(${-centre.x} ${+o +bounds.bottom})"
      }
    }

    val transform_obs = Obs(trans) {
      outerLabelText_transform() = trans()
    }

    txt
  }
}
