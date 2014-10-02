package talk

import org.scalajs.dom.{Event, HTMLInputElement}
import rx.core.Var

/**
 *
 *
 * @author Matthew Pocock
 */
case class ReactiveSlider(slider: HTMLInputElement) {
  val value = Var("")
  val valueAsNumber = Var(0)

  slider.oninput = (e: Event) => {
    value() = slider.value
    valueAsNumber() = slider.valueAsNumber
  }
}
