package talk.binding

import java.util.Date

import language.experimental.macros
import reflect.macros.blackbox.Context

import org.scalajs.dom.{Element}

import scala.annotation.StaticAnnotation

object Binder {
  implicit class BindableElement(val _el: Element) extends AnyVal {
    def bind[T <: Product](t: T)(implicit b: ClassBinder[T]) = b.bindClass(t, _el)
  }
}

/**
 * Bind an instance of a class to an element, by binding each member of the class to descendents of the element.
 *
 * @tparam T
 */
trait ClassBinder[T <: Product] {
  def bindClass(t: T, e: Element)
}


/**
 * Bind terminal values to an element's content.
 */
trait ValueBinder[T] {
  def bindValue(t: T, e: Element)
}

object ClassBinder {

  implicit def usingValueBinder[T <: Product](implicit vb: ValueBinder[T]): ClassBinder[T] = new ClassBinder[T] {
    override def bindClass(t: T, element: Element) = vb.bindValue(t, element)
  }

//  implicit def materialize[T <: Product]: ClassBinder[T] = macro binderForImpl
//
//  def binderForImpl[T <: Product : c.WeakTypeTag](c: Context)(): c.Expr[Binder[T]] = {
//    import c.universe._
//
//    val tTpe = weakTypeTag[T].tpe
//    val tSym = tTpe.typeSymbol.asClass
//
//    if(!tSym.isCaseClass) {
//      c.error(c.enclosingPosition, "Cannot create Binder class for non-case class")
//    }
//
//    val accessors = (tTpe.decls collect {
//      case acc: MethodSymbol if acc.isCaseAccessor => acc
//    }).to[List]
//
//    val
//  }
}

object ValueBinder {
  implicit object stringBinder extends ValueBinder[String] {
    override def bindValue(t: String, element: Element) = element.textContent = t
  }

  implicit object intBinder extends ValueBinder[Int] {
    override def bindValue(t: Int, element: Element) = element.textContent = t.toString
  }

  implicit object dateBinder extends ValueBinder[java.util.Date] {
    override def bindValue(t: Date, e: Element) = e.textContent = s"${t.getDate()}/${t.getMonth}/${t.getYear}"
  }
}
