package sbolv

import org.scalajs.dom.{Event, HTMLInputElement}
import rx.core.Var

import scalatags.JsDom.TypedTag
import Framework._

/**
 *
 *
 * @author Matthew Pocock
 */
case class ReactiveSlider(slider: HTMLInputElement) {
  // fixme: lazify event Rx-s
  val value = Var(slider.value)
  val valueAsNumber = Var(slider.valueAsNumber)

  def updateValue(e: Event) = {
      value() = slider.value
      valueAsNumber() = slider.valueAsNumber
    }
  slider.modifyWith(Events.input := updateValue _).render
}

object ReactiveSlider {
  def apply(slider: TypedTag[HTMLInputElement]): ReactiveSlider = ReactiveSlider(slider.render)
}