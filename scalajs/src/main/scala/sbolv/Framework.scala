package sbolv

import sbolv.SeqDiff._

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.{SortedMap, mutable}
import scala.scalajs.js.Dynamic
import scalatags.JsDom.all._
import scala.util.{Failure, Success, Random}
import rx._
import rx.core.{Propagator, Obs}
import rx.ops._
import org.scalajs.dom
import org.scalajs.dom.{Event, Element, document}
import scala.scalajs.js
import scalatags.{JsDom, generic}
import scalatags.generic.Attr


// taken from:
//   https://github.com/lihaoyi/workbench-example-app/blob/todomvc/src/main/scala/example/Framework.scala

/**
 * A minimal binding between Scala.Rx and Scalatags and Scala-Js-Dom
 */
object Framework extends Html5 with Webkit {

  object Events extends NamedEventUtil with DomL3 with Events

  val rowspan = "rowspan".attr
  val colspan = "colspan".attr


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
    override def apply(t: Element, ne: NamedEvent, v: (Event) => Unit) = {
      println(s"Registering event for ${ne.name} on $t")
      t.addEventListener(ne.name, (_: Event) => println(s"Event $ne.name was fired"))
      t.addEventListener(ne.name, v)
    }
  }

  implicit def RxAttrValueFactory[T: AttrValue]: AttrValue[dom.Element => T] = new AttrValue[dom.Element => T] {
    override def apply(t: Element, a: generic.Attr, v: (Element) => T) = {
      implicitly[AttrValue[T]].apply(t, a, v(t))
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
}

trait Updater[T] {
  def onEntered(en: Entered[T]): Frag
  def onExited(ex: Exited[T], existing: dom.Node): Option[Frag] = None
  def onModified(mod: Modified[T], existing: dom.Node): Option[Frag] = None
  def onUnchanged(un: Unchanged[T], existing: dom.Node): Option[Frag] = None

  def dataAttribute: String = "__rx_data"
}

object Updater {
  import Enhancements.DynamicApply

  implicit class RxEnhancer[T : Ordering](_rx: Rx[IndexedSeq[T]]) {

    def updateWith(updater: Updater[T]): Modifier = new Modifier {

      val diff = SeqDiff(_rx)
      val Undefined = js.undefined

      def advanceTo(n: dom.Node, t: T): dom.Node = {
        n match {
          case null | Undefined =>
            n
          case e: dom.Element =>
            val da = Dynamic(e).selectDynamic(updater.dataAttribute)
            if (implicitly[Ordering[T]].equiv(da.asInstanceOf[T], t)) {
              e
            } else {
              advanceTo(n.nextSibling, t)
            }
          case _ => advanceTo(n.nextSibling, t)
        }
      }

      override def applyTo(parent: Element) = Obs(diff.updates) {

        def unwind(child: dom.Node, dss: List[Update[T]]): Unit = {

          dss match {
            case (u@Entered(_, _))::us =>
              val fragR = updater.onEntered(u).render.asInstanceOf[dom.Element]
              Dynamic(fragR).updateDynamic(updater.dataAttribute)(Dynamic(u.item))
              parent.insertBefore(fragR, child)
              unwind(child, us)
            case (u@Exited(_, _))::us =>
              val thisChild = advanceTo(child, u.item)
              val nextChild = thisChild.nextSibling
              updater.onExited(u, thisChild) match {
                case Some(frag) =>
                  val fragR = frag.render
                  if(fragR eq thisChild) {
                    Dynamic(child).updateDynamic(updater.dataAttribute)(js.undefined)
                  } else {
                    parent.insertBefore(fragR, thisChild)
                    parent.removeChild(thisChild)
                  }
                case None =>
                  parent.removeChild(thisChild)
              }
              unwind(nextChild, us)
            case (u@Modified(_, _))::us =>
              val thisChild = advanceTo(child, u.item._1)
              val nextChild = thisChild.nextSibling
              updater.onModified(u, child) match {
                case Some(frag) =>
                  val fragR = frag.render
                  if(fragR eq thisChild) {
                    Dynamic(thisChild).updateDynamic(updater.dataAttribute)(Dynamic(u.item._2))
                  } else {
                    Dynamic(thisChild).updateDynamic(updater.dataAttribute)(js.undefined)
                    Dynamic(fragR).updateDynamic(updater.dataAttribute)(Dynamic(u.item._2))
                    parent.insertBefore(fragR, child)
                    parent.removeChild(child)
                  }
                case None =>
                  Dynamic(thisChild).updateDynamic(updater.dataAttribute)(Dynamic(u.item._2))
              }
              unwind(nextChild, us)
            case (u@Unchanged(_, _))::us =>
              val thisChild = advanceTo(child, u.item._1)
              val nextChild = thisChild.nextSibling
              updater.onUnchanged(u, child) match {
                case Some(fragR) =>
                  val frag = fragR.render
                  if(frag eq child) {
                    Dynamic(thisChild).updateDynamic(updater.dataAttribute)(Dynamic(u.item._2))
                  } else {
                    Dynamic(child).updateDynamic(updater.dataAttribute)(Dynamic(u.item))
                    parent.insertBefore(fragR.render, child)
                    parent.removeChild(child)
                  }
                case None =>
                  Dynamic(thisChild).updateDynamic(updater.dataAttribute)(Dynamic(u.item._2))
              }
              unwind(nextChild, us)
            case Nil =>
              // noop
          }
        }

        unwind(parent.firstChild, diff.updates())
      }
    }
  }

}