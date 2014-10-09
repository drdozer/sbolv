package talk

import org.scalajs.dom._

import scala.scalajs.js

/**
 *
 *
 * @author Matthew Pocock
 */
object Enhancements {

  private val svgNamespace = "http://www.w3.org/2000/svg"

  implicit class StringSvgTagName(val _tag: String) extends AnyVal {
    def asSVGElement[E <: SVGElement]: E = document.createElementNS(svgNamespace, _tag).asInstanceOf[E]
  }


  implicit class EnhancedDOMList[A](val _dl: DOMList[A]) extends AnyVal {
    def indexes = 0 until _dl.length
    def nodes: IndexedSeq[A] = indexes map _dl.apply
    def elements(implicit ev: A <:< Node) = nodes.filter(_.nodeType == 1).map(_.asInstanceOf[HTMLElement])
  }

  implicit class EnhancedNode(val _node: Node) extends AnyVal {
    def childElements = _node.childNodes.elements
    def childNodes = _node.childNodes.nodes
    def removeAllChildren() = while(_node.firstChild != null) {
      _node.removeChild(_node.firstChild)
    }
    def removeAllOtherChildren(toKeep: Option[Node]) = {
      def isToKeep(n: Node) = toKeep.contains(n)
      while(_node.firstChild != null && !isToKeep(_node.firstChild)) {
        _node.removeChild(_node.firstChild)
      }
      while(_node.lastChild != null && !isToKeep(_node.lastChild)) {
        _node.removeChild(_node.lastChild)
      }
    }
    def hasChildNode(n: Node) = childElements.contains(n)
    def removeChild(i: Int) = _node.removeChild(childNodes(i))
  }

  implicit class EnhancedElement[E <: Element](val _el: E) extends AnyVal {
    def apply[T](ts: T*)(implicit at: Applier[T]) = {
      for(a <- ts) at.apply(_el, a)
      _el
    }

    def apply(textContent: String) = {
      _el.textContent = textContent
      _el
    }
  }

  trait Applier[T] {
    def apply(e: Element, t: T)
  }

  implicit def attrApplier: Applier[(String, String)] = new Applier[(String, String)] {
    override def apply(e: Element, t: (String, String)) = e.setAttribute(t._1, t._2)
  }

  implicit def childApplier[N <: Node]: Applier[N] = new Applier[N] {
    override def apply(e: Element, t: N) = e.appendChild(t)
  }

  def styleOf(avs: (String, String)*): String = (avs map { case (a, v) => s"$a: $v" }).mkString(";")

  implicit class DynamicApply(val _dynamic: js.Dynamic.type) extends AnyVal {
    def apply(x: Any): js.Dynamic = x.asInstanceOf[js.Dynamic]
  }

  implicit class EnhancedSVGRect(val _rect: SVGRect) extends AnyVal {
    def asRectangle: SVGRectElement = "rect".asSVGElement[SVGRectElement](
      "rx" -> _rect.x.toString,
      "ry" -> _rect.y.toString,
      "width" -> _rect.width.toString,
      "height" -> _rect.height.toString)
  }
}
