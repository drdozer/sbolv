package sbolv

import rx.ops._
import rx.core._

import scala.scalajs.js
import scalatags.ext.SeqDiff

/**
 *
 *
 * @author Matthew Pocock
 */
object MyApp extends js.JSApp {

  def main(): Unit = {
    val s = Var("")
    val ss = s map (_.to[IndexedSeq])
    val sd = SeqDiff(ss)

    println(s())
    println(ss())
    println(sd.aligned())

    s() = "hi mum"

    println(s())
    println(ss())
    println(sd.aligned())

    s() = "mum is the word"

    println(s())
    println(ss())
    println(sd.aligned())
  }
}
