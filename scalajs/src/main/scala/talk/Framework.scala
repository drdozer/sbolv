package talk

import scala.collection.{SortedMap, mutable}
import scalatags.JsDom.all._
import scala.util.{Failure, Success, Random}
import rx._
import rx.core.{Propagator, Obs}
import org.scalajs.dom
import org.scalajs.dom.{Event, Element, document}
import scala.scalajs.js
import scalatags.generic


// taken from:
//   https://github.com/lihaoyi/workbench-example-app/blob/todomvc/src/main/scala/example/Framework.scala

/**
 * A minimal binding between Scala.Rx and Scalatags and Scala-Js-Dom
 */
object Framework extends DomL3 {

  /**
   * Wraps reactive values in spans, so they can be referenced/replaced
   * when the Rx changes.
   */
  implicit def RxT[T](r: Rx[T])(implicit f: T => Frag): Frag = {
    rxModT(Rx(span(r())))
  }

  /**
   * Wrap a reactive string.
   */
  implicit def RxStr[T](r: Rx[String]): Frag = {
    rxModStr(Rx(document.createTextNode(r())))
  }

  /**
   * Sticks some Rx into a Scalatags fragment, which means hooking up an Obs
   * to propagate changes into the DOM via the element's ID. Monkey-patches
   * the Obs onto the element itself so we have a reference to kill it when
   * the element leaves the DOM (e.g. it gets deleted).
   */
  implicit def rxModT[T <: dom.HTMLElement](r: Rx[HtmlTag]): Frag = {
    def rSafe = r.toTry match {
      case Success(v) => v.render
      case Failure(e) => span(e.toString, backgroundColor := "red").render
    }
    var last = rSafe
    Obs(r, skipInitial = true){
      val newLast = rSafe
      last.parentNode.replaceChild(newLast, last)
      last = newLast
    }
    bindNode(last)
  }
  /**
   * Sticks some Rx into a Scalatags fragment, which means hooking up an Obs
   * to propagate changes into the DOM via the element's ID. Monkey-patches
   * the Obs onto the element itself so we have a reference to kill it when
   * the element leaves the DOM (e.g. it gets deleted).
   */
  implicit def rxModStr(r: Rx[dom.Node]): Frag = {
    def rSafe = r.toTry match {
      case Success(v) => v.render
      case Failure(e) => span(e.toString, backgroundColor := "red").render
    }
    var last = rSafe
    Obs(r, skipInitial = true){
      val newLast = rSafe
      last.parentNode.replaceChild(newLast, last)
      last = newLast
    }
    bindNode(last)
  }
  implicit def RxAttrValue[T: AttrValue]: AttrValue[Rx[T]] = new AttrValue[Rx[T]]{
    def apply(t: Element, a: Attr, r: Rx[T]): Unit = {
      Obs(r){ implicitly[AttrValue[T]].apply(t, a, r())}
    }
  }
  implicit def RxStyleValue[T: StyleValue]: StyleValue[Rx[T]] = new StyleValue[Rx[T]]{
    def apply(t: Element, s: Style, r: Rx[T]): Unit = {
      Obs(r){ implicitly[StyleValue[T]].apply(t, s, r())}
    }
  }

  implicit def FuncEventValue: EventValue[Element, Event => Unit] = new EventValue[Element, Event => Unit] {
    override def apply(t: Element, ne: NamedEvent, v: (Event) => Unit) =
      t.addEventListener(ne.name, v)
  }

  implicit def RxAttrValueFactory[T: AttrValue]: AttrValue[dom.Element => T] = new AttrValue[dom.Element => T] {
    override def apply(t: Element, a: generic.Attr, v: (Element) => T) = {
      implicitly[AttrValue[T]].apply(t, a, v(t))
    }
  }
}
