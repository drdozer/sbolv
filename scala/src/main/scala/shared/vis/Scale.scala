package shared.vis

/**
 * A co-linear function from elements of type `A` to elements of type `B`.
 *
 * @tparam A
 * @tparam B
 */
trait Mapping[A, B] {
  def apply(a: A): B
}

/**
 * A contiguous set of values with a lowest and highest value.
 */
trait Interval[T] {
  self =>

  def lowest: T

  def highest: T

  implicit def ordT: Ordering[T]

  def map[U](f: T => U)(implicit ordU: Ordering[U]): Interval[U] = new Interval[U] {
    def lowest = f(self.lowest)
    def highest = f(self.highest)

    implicit val ordT = if(ordU.compare(lowest, highest) < 0) ordU else ordU.reverse
  }
}

object Interval {
  def apply[T](l: T, h: T)(implicit ot: Ordering[T]): Interval[T] = {
    assert(ot.lt(l, h), "Lowest value of interval must be lower than the highest value")
    new Interval[T] {
      override def lowest = l
      override def highest = h
      override implicit def ordT = ot
    }
  }

  def unit = apply(0.0, 1.0)

  implicit class DoubleInterval(val _interval: Interval[Double]) extends AnyVal {
    def unitScale: Scale[Double, Double] = Scale(Interval.unit, _interval)
  }
}

/**
 * Some collection of landmark values.
 *
 * @tparam T
 */
trait Landmarks[T] {
  def values: Seq[T]
}

/**
 * A scale maps from one range of values into another.
 *
 * The scale is valid within the domain and range intervals. The lowest domain value maps to the lowest range value, and
 * the highest domain value maps to the highest range value.
 *
 * @author Matthew Pocock
 */
trait Scale[A, B] extends Mapping[A, B] {
  def domain: Interval[A]
  def range: Interval[B]
}

object Scale {
  def apply(dom: Interval[Double], rng: Interval[Double]): Scale[Double, Double] = new Scale[Double, Double] {

    override def domain = dom

    override def range = rng

    override def apply(a: Double) = (a - dom.lowest) * (rng.highest - rng.lowest) / (dom.highest - dom.lowest)
  }

  def apply[A, B](landmarks : Landmarks[A])
                 (f: A => B)
                 (implicit oa: Ordering[A], ob: Ordering[B]): Scale[A, B] =
  {
    val d = Interval(landmarks.values.head, landmarks.values.last)
    val r = Interval(f(d.lowest), f(d.highest))

    new Scale[A, B] {
      override def domain = d

      override def range = r

      override def apply(a: A) = f(a)
    }
  }
}