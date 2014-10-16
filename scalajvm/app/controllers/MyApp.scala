package controllers

import rx.core._
import rx.ops._
import sbolv.SeqDiff.Entered
import sbolv._

import scala.scalajs.js

/**
 *
 *
 * @author Matthew Pocock
 */
object MyApp extends js.JSApp {

  def main(args: Array[String]): Unit = main()

  def main(): Unit = {
    var ctr = 0
    def counter() = {
      val c = ctr
      ctr = ctr + 1
      c
    }

    case class C(c: Char)(i: Int = counter()) {
      override def toString = s"C($c)($i)"
    }

    val s = Var("")
    val ss = s map (_.to[IndexedSeq])
    val sd = SeqDiff(ss)
    val sm = sd.map[C](
      entered = { case Entered(c, _) => C(c)() },
      modified = { case (_, c) => c })

    println(s())
    println(ss())
    println(sd.aligned())
    println(sm())

    s() = "hi mum"

    println(s())
    println(ss())
    println(sd.aligned())
    println(sm())

    s() = "mum is the word"

    println(s())
    println(ss())
    println(sd.aligned())
    println(sm())

    s() = "mum hi"

    println(s())
    println(ss())
    println(sd.aligned())
    println(sm())

    s() = "mam ji"

    println(s())
    println(ss())
    println(sd.aligned())
    println(sm())

    s() = ""

    println(s())
    println(ss())
    println(sd.aligned())
    println(sm())
  }
}
