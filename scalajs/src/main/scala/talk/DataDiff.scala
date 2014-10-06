package talk

import rx.ops._
import rx.core.Rx

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




/**
 *
 *
 * @author Matthew Pocock
 */
case object DataDiff {

  def apply[D](items: Rx[Seq[D]]): DataDiff[D, D] = DataDiff(items, identity, _ == _)
  def apply[D, K](items: Rx[Seq[D]], byKey: D => K): DataDiff[D, K] = DataDiff(items, byKey, _ == _)

  case class Updates[D](entered: Seq[(D, Int)],
                        exited: Seq[(D, Int)],
                        updated: Seq[(D, (Int, Int))],
                        unchanged: Seq[(D, Int)])
}
