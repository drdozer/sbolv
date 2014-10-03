package talk

import org.scalajs.dom.SVGGElement
import rx.core.{Obs, Rx}
import rx.ops._

import scala.scalajs.js

/**
 *
 *
 * @author Matthew Pocock
 */
case class FixedWidth(boxWidthHeight: Rx[Double],
                      alignment: Rx[BackboneAlignment],
                      glyphs: Rx[Seq[(Rx[Double], Rx[BackboneAlignment]) => GlyphFamily]])
{
  import Enhancements._

  private val verticalCentre = Rx {
    boxWidthHeight() * 0.5
  }
  private val allGlyphs_transform = Rx {
    s"translate(0 ${verticalCentre()})"
  }

  private val dd = DataDiff(glyphs)

  private val dd_obs = Obs(dd.updates) {
    val updates = dd.updates()
    for((g, i) <- updates.entered) {
      val gl = g(boxWidthHeight, alignment)
      val tr = "g".asSVGElement[SVGGElement](gl.glyph)
      Obs(boxWidthHeight) {
        tr("transform" -> s"translate(${(i + 0.5) * boxWidthHeight()} 0)")
      }

      allGlyphs.insertBefore(tr, Option(allGlyphs.childNodes.apply(i)).filterNot(_ == js.undefined).map(_.nextSibling).getOrElse(null))
    }
  }


  val allGlyphs = "g".asSVGElement[SVGGElement]("class" -> "sbolv fixed-width glyphs")

  private val allGlyphs_tranform_obs = Obs(allGlyphs_transform) {
    allGlyphs("transform" -> allGlyphs_transform())
  }

}

case class DataDiff[D, K](items: Rx[Seq[D]], byKey: D => K, areEqual: (D, D) => Boolean) {
  var dataMap = Rx {
    (items().zipWithIndex.map { case(d, i) => byKey(d) -> (d, i) }).toMap
  }

  var updates = dataMap.diff({ (previous, next) =>
    val enteredKeys = next.keySet &~ previous.keySet
    val exitedKeys = previous.keySet &~ next.keySet
    val remainingKeys = previous.keySet & next.keySet
    val (unchangedKeys, updatedKeys) = remainingKeys.partition { k =>
      val pk = previous(k)
      val nk = next(k)
      (pk._2 == nk._2) && areEqual(pk._1, nk._1)
    }

    DataDiff.Updates(
      entered = for(k <- enteredKeys.to[Seq]) yield next(k),
      exited = for(k <- exitedKeys.to[Seq]) yield previous(k),
      updated = for(k <- updatedKeys.to[Seq]) yield  {
        val pk = previous(k)
        val nk = next(k)
        nk._1 -> (pk._2, nk._2)
      },
      unchanged = for(k <- unchangedKeys.to[Seq]) yield  next(k))
  }, { (next) =>
    DataDiff.Updates(
      entered = next.values.to[Seq],
      exited = Seq(),
      updated = Seq(),
      unchanged = Seq())
  })
}

case object DataDiff {

  def apply[D](items: Rx[Seq[D]]): DataDiff[D, D] = DataDiff(items, identity, _ == _)
  def apply[D, K](items: Rx[Seq[D]], byKey: D => K): DataDiff[D, K] = DataDiff(items, byKey, _ == _)

  case class Updates[D](entered: Seq[(D, Int)],
                        exited: Seq[(D, Int)],
                        updated: Seq[(D, (Int, Int))],
                        unchanged: Seq[(D, Int)])
}

