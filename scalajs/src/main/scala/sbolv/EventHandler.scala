package sbolv

import scalatags.generic.{Modifier}


case class NamedEvent(name: String) {

  def :=[Builder, T](v: T)(implicit ev: EventValue[Builder, T]) = EventHandlerPair(this, v, ev)

}

/**
 * A [[Modifier]] which wraps up an event listener.
 *
 * Rather than adding an attribute or a child, an [[EventHandlerPair]] will call `addEventListner()` to register the
 * listener.
 *
 * @tparam Builder
 */
case class EventHandlerPair[Builder, T](ne: NamedEvent, v: T, ev: EventValue[Builder, T]) extends Modifier[Builder] {
  override def applyTo(t: Builder): Unit = {
    ev.apply(t, ne, v)
  }
}

trait EventValue[Builder, T] {
  def apply(t: Builder, ne: NamedEvent, v: T)
}

trait Util {
  implicit class ExtendedString(s: String) {

    def namedEvent = NamedEvent(s)

  }
}

trait DomL3 extends Util {
  val DOMNodeInsertedIntoDocument = "DOMNodeInsertedIntoDocument".namedEvent

  val DOMNodeInserted = "DOMNodeInserted".namedEvent

  val DOMSubtreeModified = "DOMSubtreeModified".namedEvent

  val DOMAttrModified = "DOMAttrModified".namedEvent
}