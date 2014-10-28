package sbolv

import sbolv.SeqDiff._

import scala.annotation.unchecked.uncheckedVariance
import scalatags.JsDom.all._
import scala.util.{Failure, Success}
import rx._
import rx.core.Obs
import rx.ops._
import org.scalajs.dom
import org.scalajs.dom.{Event, Element, document}
import scalatags.{JsDom, generic}
import scalatags.generic.Attr


// taken from:
//   https://github.com/lihaoyi/workbench-example-app/blob/todomvc/src/main/scala/example/Framework.scala

/**
 * A minimal binding between Scala.Rx and Scalatags and Scala-Js-Dom
 */
object Framework extends Reactives with Html5 with Webkit {

  object Events extends NamedEventUtil with DomL3 with Events

  val rowspan = "rowspan".attr
  val colspan = "colspan".attr

  implicit def FuncEventValue: EventValue[Element, Event => Unit] = new EventValue[Element, Event => Unit] {
    override def apply(t: Element, ne: NamedEvent, v: (Event) => Unit) = {
      t.addEventListener(ne.name, v)
    }
  }

  implicit class EnhancedElement[Output <: dom.Element](val elem: Output) extends AnyVal {
    def modifyWith: ElementModifier[Output] = ElementModifier(elem, Nil)
  }

  case class ElementModifier[Output <: dom.Element](elem: Output,
                                                    modifiers: List[Seq[Modifier]])
    extends generic.TypedTag[dom.Element, Output, dom.Node]
    with scalatags.jsdom.Frag
  {
    // unchecked because Scala 2.10.4 seems to not like this, even though
    // 2.11.1 works just fine. I trust that 2.11.1 is more correct than 2.10.4
    // and so just force this.
    protected[this] type Self = ElementModifier[Output @uncheckedVariance]

    def render: Output = {
      build(elem)
      elem
    }

    /**
     * Trivial override, not strictly necessary, but it makes IntelliJ happy...
     */
    def apply(xs: Modifier*): Self = {
      this.copy(modifiers = xs :: modifiers)
    }

    override def tag = elem.nodeName

    override def toString = render.outerHTML
  }

  def modifyWith(el: Element => Unit): Modifier = new generic.Modifier[dom.Element] {
    override def applyTo(t: Element) = el(t)
  }
}

trait Reactives {

  implicit def RxAttrValueFactory[T: AttrValue]: AttrValue[dom.Element => T] = new AttrValue[dom.Element => T] {
    override def apply(t: Element, a: generic.Attr, v: (Element) => T) = {
      implicitly[AttrValue[T]].apply(t, a, v(t))
    }
  }

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
    rxModNode(Rx(document.createTextNode(r())))
  }

  /**
   * Wrap a reactive string.
   */
  implicit def RxOStr[T](r: Rx[Option[String]]): Frag = {
    RxStr(r map (_ getOrElse ""))
  }

  /**
   * Sticks some Rx into a Scalatags fragment, which means hooking up an Obs
   * to propagate changes into the DOM via the element's ID. Monkey-patches
   * the Obs onto the element itself so we have a reference to kill it when
   * the element leaves the DOM (e.g. it gets deleted).
   */
  implicit def rxModT[T <: dom.Element](r: Rx[JsDom.TypedTag[T]]): Frag = {
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
  implicit def rxModNode(r: Rx[dom.Node]): Frag = {
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

  implicit def RxAttrValue[T](implicit tAttr: AttrValue[T]): AttrValue[Rx[T]] = new AttrValue[Rx[T]]{
    def apply(t: Element, a: Attr, r: Rx[T]): Unit = {
      Obs(r){ implicitly[AttrValue[T]].apply(t, a, r())}
    }
  }

  implicit def RxStyleValue[T](implicit tSty: StyleValue[T]): StyleValue[Rx[T]] = new StyleValue[Rx[T]]{
    def apply(t: Element, s: Style, r: Rx[T]): Unit = {
      Obs(r){ implicitly[StyleValue[T]].apply(t, s, r())}
    }
  }

}
