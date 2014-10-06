package talk

import org.scalajs.dom.{Event, HTMLInputElement}
import rx.core.Var

/**
 *
 *
 * @author Matthew Pocock
 */
case class ReactiveSlider(slider: HTMLInputElement) {
  val value = Var(slider.value)
  val valueAsNumber = Var(slider.valueAsNumber)

  slider.oninput = (e: Event) => {
    println("Updating slider to: " + slider.value)
    value() = slider.value
    valueAsNumber() = slider.valueAsNumber
  }
}
